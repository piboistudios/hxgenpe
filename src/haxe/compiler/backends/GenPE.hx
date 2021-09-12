package haxe.compiler.backends;

import mono.ilasm.MethodRef;
import mono.ilasm.BaseClassRef;
import mono.ilasm.ExternTypeRef;
import mono.ilasm.GenericArguments;
import haxe.exceptions.NotImplementedException;
import mono.ilasm.BaseGenericTypeRef;
import cs.system.reflection.SignatureGenericParameterType;
import peapi.PrimitiveType;
import mono.ilasm.PrimitiveTypeRef;
import mono.ilasm.TypeDef;

using hscript.Tools;

import hscript.Interp;
import hscript.Checker.TType;
import hscript.PECheckerTypes;
import haxe.io.Bytes;
import hscript.Checker.CheckerTypes;
import cs.StdTypes.UInt8;
import cs.Lib;
import mono.ilasm.LdstrInstr;
import mono.ilasm.MethodInstr;

using hscript.Tools;

import mono.ilasm.BaseTypeRef;
import mono.ilasm.TypeRef;
import peapi.ImplAttr;
import hscript.Expr;
import mono.ilasm.MethodDef;
import peapi.MethAttr;
import peapi.FieldAttr;
import mono.ilasm.FieldDef;
import hscript.Expr.VarDecl;
import hscript.Expr.FieldDecl;
import hscript.Expr.FunctionDecl;
import mono.ilasm.Location;
import peapi.TypeAttr;
import mono.ilasm.PeapiTypeRef;
import hscript.Expr.ModuleDecl;
import mono.ilasm.CodeGen;

using Lambda;
using StringTools;
using haxe.compiler.backends.GenPE;

class GenPE {
	public var gen:CodeGen;
	public var types:PECheckerTypes;

	var outputFile = '';
	var currentPackage = '';
	// I guess we shouldn't force dependencies... but for now, this will rely on System.Linq.Expressions.dll
	var useSystemDynamicTypes = true;
	var evaluatedStaticSets:Map<String, Map<String, Dynamic>> = [];
	var deferred:Array<Void->Void> = [];
	var interp = new Interp();

	public function new(outputFile, isDll, debuggingInfo, autoInherit) {
		this.gen = new CodeGen(outputFile, isDll, debuggingInfo, autoInherit);
		this.outputFile = outputFile;
	}

	// pretty sure you won't be able to support complex static initializers
	// because eval doesn't exist in Haxe... I guess unless you run haxe and interpret stuff..
	public function buildHaxe(types:Array<haxe.macro.Type>) {
		throw 'not implemented';
	}

	public function buildHscript(types) {
		firstPass(types);
		secondPass(types);
		finalPass(types);
		var bytecode:Bytes = getBinary();
		return bytecode;
	}

	// add types to checker
	function firstPass(decls:Array<hscript.Expr.ModuleDecl>) {
		for (decl in decls)
			types.addType(decl);
	}

	// generate PE (or apparently not... just prep the codegen and set up deferred tasks)
	function secondPass(types:Array<hscript.Expr.ModuleDecl>) {
		for (type in types)
			switch type {
				case DPackage(pack):
					currentPackage = pack.join('.');
					this.types.setPack(currentPackage);
					gen.set_CurrentNameSpace(pack.toClrPath());
				// case DImport(parts, everything):
				case DClass(c):
					var name = currentPackage + c.name;
					var flags = 0;

					if (c.isPrivate)
						flags = flags | cast TypeAttr.Private;
					else
						flags = flags | cast TypeAttr.Public;

					gen.BeginTypeDef(cast flags, c.name.split('.').pop(), null, null, type.location(), null);
					for (field in c.fields) {
						switch field.kind {
							case KFunction(f):
								generateMethod(name, field, f);
							case KVar(v):
								generateVar(name, field, v);
						}
					}
				default:
			}
	}

	// run deferred tasks (finish prepping codegen) and generate PE
	// I think, if this isn't clumsy you shouldn't need types but..
	// what the heck, more info the better I guess..
	function finalPass(types:Array<hscript.Expr.ModuleDecl>) {
		for (deferred in deferred)
			deferred();
		gen.Write(); // assemblies have been outputted
	}

	function generateMethod(owner:String, field:FieldDecl, f:FunctionDecl) {
		if (field.access.contains(AStatic)) {
			var func = EFunction(f.args, f.expr, field.name, f.ret).mk(f.expr);
			evaluatedStaticSets[owner].set(field.name, interp.execute(func));
		}
		var flags = getClrFlags(field, [cast MethAttr.Public, cast MethAttr.Static, cast MethAttr.Private]);
		var conv = peapi.CallConv.Default;
		var implAttr = ImplAttr.IL;
		var ret = types.checker.check(f.expr, WithType(types.toTType(f.ret)));
		var retType = toClrTypeRef(ret);
		var ownerType = gen.CurrentTypeDef; // lookupType(owner).toClrTypeDef(this);
		var paramList = new cs.system.collections.ArrayList();
		for (arg in f.args)
			paramList.Add(if (arg.value == null) types.toTType(arg.t) else types.checker.check(arg.value, WithType(types.toTType(arg.t))));

		var genericParameters = null;
		var method = new MethodDef(gen, cast flags, conv, implAttr, field.name, retType, paramList, f.expr.location(), genericParameters, ownerType);
		if (field.name == "main" && owner == mainClass)
			method.EntryPoint();
		f.expr.mapToClrMethodBody(method, ret);
	}

	function generateVar(owner:String, field:FieldDecl, v:VarDecl) {
		if (field.access.contains(AStatic)) {
			deferred.push(() -> evaluatedStaticSets[owner].set(field.name, interp.execute(v.expr)));
		}
		var flags = getClrFlags(field, [cast FieldAttr.Public, cast FieldAttr.Static, cast FieldAttr.Private]);
		var name = field.name;
		var type = toClrTypeRef(if (v.expr != null) types.checker.check(v.expr, WithType(types.toTType(v.type))) else types.toTType(v.type));
		var field = new FieldDef(flags, name, type);
		gen.AddFieldDef(field);
		deferred.push(() -> tryInitField(field, gen.CurrentTypeDef));
	}

	var mainClass(default, null):String;

	// this is kind of dumb, really only exists to some interface I made up for haxe compiler back ends..
	// technically back-ends return binaries directly.. I think that's the right way to go about this
	// if you just want the file, you can ignore the output I guess, but I imagine if you're
	// generating binaries and intending to send them somewhere, this I will handle reading the file for you
	// hm... opinions.. decisions..
	function getBinary():Bytes {
		return sys.io.File.getBytes(outputFile);
	}

	public function clrApply(e:BaseClassRef, types:Array<TType>)
		return e.GetGenericTypeInst({
			var args = new GenericArguments();
			for (t in types)
				args.Add(toClrTypeRef(t));
			args;
		});

	function toClrTypeRef(t:TType):BaseTypeRef
		return switch types.checker.follow(t) {
			case TVoid: new PrimitiveTypeRef(PrimitiveType.Void, "System.Void");
			case TInt: new PrimitiveTypeRef(PrimitiveType.Int32, "System.Int32");
			case TFloat: new PrimitiveTypeRef(PrimitiveType.Float32, "System.Single");
			case TBool: new PrimitiveTypeRef(PrimitiveType.Boolean, "System.Boolean");
			case TDynamic: getDynamicTypeRef();
			case TAbstract(a, args): toClrTypeRef(types.checker.apply(a.t, a.params, args));
			case TInst(c, args): clrApply(gen.GetTypeRef(c.name), args);
			case TNull(t): clrApply(gen.ExternTable.GetTypeRef("mscorlib.dll", "System.Nullable", false), [t]);
            case TEnum(e, args): clrApply(gen.GetTypeRef(e.name), args) ;
            case TType(t, args): clrApply(gen.GetTypeRef(t.name), args);
            case TFun(args, ret): clrApply(getFunctionTypeRef(args, ret), [for(arg in args) arg.t]); // These need to capture generics?
            case TAnon(fields): getDynamicTypeRef();
            default:
                throw 'Error, invalid type: $t';
			case TParam(name):
				/*
					This is what needs to happen for TParam
					From ILParser.jay...
					For generics in the current type definition's signature:
						int num = -1;
						string name = (string) $2;
						if (codegen.CurrentTypeDef != null)
							num = codegen.CurrentTypeDef.GetGenericParamNum (name);
						GenParam gpar = new GenParam (num, name, GenParamType.Var);
										$$ = new GenericParamRef (gpar, name);
					For generics in the current method definition's signature:
						int num = -1;
						string name = (string) $3;
						if (codegen.CurrentMethodDef != null)
							num = codegen.CurrentMethodDef.GetGenericParamNum (name);
						GenParam gpar = new GenParam (num, name, GenParamType.MVar);
										$$ = new GenericParamRef (gpar, name);
						To do this, we'd need to capture:
							- Whether or not we're in a type def (error otherwise :) )
							- Whether or not we're in a method body
							- A list of the type parameters belonging to the current class
							- A list of the type parameters belonging to the current method (if we're in one)
				 */
				throw new NotImplementedException();
		}

	/**
	 * Gets the CLR flags for a hscript field decl
	 * @param field 
	 * @param enumSet - The set of flags to use, in order they should be: public flag, static flag, private flag
	 */
	function getClrFlags<T>(field:FieldDecl, enumSet:Array<Int>):T {
		var flags = 0;
		for (access in field.access)
			switch access {
				case APublic:
					flags |= enumSet[0];
				case AStatic:
					flags |= enumSet[1];
				case APrivate:
					flags |= enumSet[2];
				default:
			}
		return cast flags;
	}

	function tryInitField(field:FieldDef, typeDef:TypeDef) {}

	function getDynamicTypeRef():BaseTypeRef {
		throw new haxe.exceptions.NotImplementedException();
	}

	function getFunctionTypeRef(args:Array<{name:String, opt:Bool, t:TType}>, ret:TType):BaseClassRef {
		throw new haxe.exceptions.NotImplementedException();
	}
}

class ClrMethodDefTools {
	public static function op(method:MethodDef, op, operand, loc)
		method.AddInstr(new MethodInstr(op, operand, loc));
}

class OpTools {
	public static inline function ldstr(s:String, loc)
		return new LdstrInstr({
			var arr:Array<UInt8> = [for (i in 0...s.length) s.fastCodeAt(i)];
			Lib.nativeArray(arr, false);
		}, loc);
}

class HscriptExprTools {
	public static inline function location(expr:hscript.Expr)
		return new Location(expr.line, expr.pmin);

	public static function mapToClrMethodBody(expr:hscript.Expr, method:MethodDef, type:TType)
		expr.iter(e -> {});
}

class HscriptModuleDeclTools {
	public static function location(cl:hscript.Expr.ModuleDecl) {
		// return cl.pos; TODO: add position tracking to hscript module decls
		return new Location(0, 0);
	}
}

class PackTools {
	public static function toClrPath(path:Array<String>) {
		return path.map(part -> part.substr(0, 1).toUpperCase() + part.substr(1)).join('.');
	}
}
