package haxe.compiler.backends;

import hscript.Checker;
import mono.ilasm.HandlerBlock;
import mono.ilasm.Local;
import cs.system.collections.ArrayList;
import mono.ilasm.SwitchInstr;
import mono.ilasm.LabelInfo;
import mono.ilasm.LdtokenInstr;
import mono.ilasm.IFieldRef;
import haxe.ds.Either;
import mono.ilasm.CalliInstr;
import peapi.CallConv;
import mono.ilasm.TypeInstr;
import peapi.TypeOp;
import mono.ilasm.FieldInstr;
import mono.ilasm.GenericParamRef;
import peapi.FieldOp;
import mono.ilasm.BaseMethodRef;
import peapi.MethodOp;
import mono.ilasm.BranchInstr;
import peapi.BranchOp;
import mono.ilasm.LdcInstr;
import mono.ilasm.MiscInstr;
import cs.types.UInt64;
import peapi.IntOp;
import mono.ilasm.IntInstr;
import mono.ilasm.SimpInstr;
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

typedef Deferred = Array<Void->Void>;

class GenPE {
	public var gen:CodeGen;
	public var types:PECheckerTypes;

	var previous:Any;
	var locals:Map<String, BaseTypeRef> = [];
    var closure:Closure;
	var printer = new hscript.Printer();

	var outputFile = '';
	var labelRefCount:Map<String, Int> = [];
	var currentPackage = '';
	// I guess we shouldn't force dependencies... but for now, this will rely on System.Linq.Expressions.dll
	var useSystemDynamicTypes = true;
	var firstPassComplete = false;
	var evaluatedStaticSets:Map<String, Map<String, Dynamic>> = [];
	var deferred:{
		firstWave:Deferred,
		secondWave:Deferred
	} = {firstWave: [], secondWave: []}
	var interp = new Interp();

	public function new(outputFile, isDll, debuggingInfo, autoInherit) {
		this.gen = new CodeGen(outputFile, isDll, debuggingInfo, autoInherit);
		this.outputFile = outputFile;
	}

	// wait that's not how static initializers work, I guess just macros won't work.
	// So the AST would have to have had macros already applied
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
		firstPassComplete = true;
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
		for (deferred in deferred.firstWave)
			deferred();
		for (deferred in deferred.secondWave)
			deferred();
		gen.Write(); // assemblies have been outputted
	}

	function generateMethod(owner:String, field:FieldDecl, f:FunctionDecl) {
		var flags = getClrFlags(field, [cast MethAttr.Public, cast MethAttr.Static, cast MethAttr.Private]);
		flags |= cast MethAttr.HideBySig;
		var conv:Int = cast peapi.CallConv.Default;
		if (field.name == 'new') {
			flags |= cast MethAttr.SpecialName;
			flags |= cast MethAttr.RTSpecialName;
		}
		if (field.access.contains(AStatic)) {
			// what?
			// var func = EFunction(f.args, f.expr, field.name, f.ret).mk(f.expr);
			// evaluatedStaticSets[owner].set(field.name, interp.execute(func));
		} else {
			conv |= cast CallConv.Instance;
		}
		if (false) { // check if f has params...
			conv |= cast CallConv.Generic;
		}

		var implAttr = ImplAttr.IL;
		var ret = types.checker.check(f.expr, WithType(types.toTType(f.ret)));
		var retType = toClrTypeRef(ret);
		var ownerType = gen.CurrentTypeDef; // lookupType(owner).toClrTypeDef(this);
		var paramList = new cs.system.collections.ArrayList();
		for (arg in f.args)
			paramList.Add(if (arg.value == null) types.toTType(arg.t) else types.checker.check(arg.value, WithType(types.toTType(arg.t))));

		var genericParameters = null;
		var method = new MethodDef(gen, cast flags, cast conv, implAttr, field.name, retType, paramList, f.expr.location(), genericParameters, ownerType);
		if (field.name == "main" && owner == mainClass)
			method.EntryPoint();
		method.SetMaxStack(8); // apparently this only matters for PE verification and has nothing to do with runtime
		// so, unless this value is intelligently set, there's a chance the PE won't be verifiable
		// BUT by default .NET allows unverifiable PEs so.... is it worth the bother? Not right now
		labelRefCount = [];
		locals = [];
        closure = null;
		mapToClrMethodBody(f.expr, method, ret);
	}

	function referenceLabel(refName) {
		var c = labelRefCount[refName];
		if (c == null)
			c = 0;
		return '$refName$c';
	}

	function placeLabelRef(refName) {
		var labelRef = referenceLabel(refName);
		if (!labelRefCount.exists(refName)) {
			labelRefCount[refName] = 1;
		} else
			labelRefCount[refName]++;
		return gen.CurrentMethodDef.AddLabel(labelRef);
	}

	function generateVar(owner:String, field:FieldDecl, v:VarDecl) {
		// what?
		// if (field.access.contains(AStatic)) {
		//     deferred.firstWave.push(() -> evaluatedStaticSets[owner].set(field.name, interp.execute(v.expr)));
		// }
		var flags = getClrFlags(field, [cast FieldAttr.Public, cast FieldAttr.Static, cast FieldAttr.Private]);
		var name = field.name;

		var type = toClrTypeRef(if (v.expr != null) types.checker.check(v.expr, WithType(types.toTType(v.type))) else types.toTType(v.type));
		var field = new FieldDef(flags, name, type);
		gen.AddFieldDef(field);
		deferred.secondWave.push(() -> tryInitField(field, gen.CurrentTypeDef));
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

	function clrApply(e:BaseClassRef, types:Array<TType>)
		return e.GetGenericTypeInst({
			var args = new GenericArguments();
			for (t in types)
				args.Add(toClrTypeRef(t));
			args;
		});

	function toClrTypeRef(t:TType):BaseTypeRef
		return // this of course can only be called after the first pass
			if (!firstPassComplete) throw 'First pass incomplete; cannot convert types'; else switch types.checker.follow(t) {
				case TVoid: new PrimitiveTypeRef(PrimitiveType.Void, "System.Void");
				case TInt: new PrimitiveTypeRef(PrimitiveType.Int32, "System.Int32");
				case TFloat: new PrimitiveTypeRef(PrimitiveType.Float32, "System.Single");
				case TBool: new PrimitiveTypeRef(PrimitiveType.Boolean, "System.Boolean");
				case TDynamic: getDynamicTypeRef();
				case TAbstract(a, args): toClrTypeRef(types.checker.apply(a.t, a.params, args));
				case TInst(c, args): clrApply(gen.GetTypeRef(c.name), args);
				case TNull(t): clrApply(gen.ExternTable.GetTypeRef("mscorlib.dll", "System.Nullable", false), [t]);
				case TEnum(e, args): clrApply(gen.GetTypeRef(e.name), args);
				case TType(t, args): clrApply(gen.GetTypeRef(t.name), args);
				case TFun(args, ret): clrApply(getFunctionTypeRef(args, ret), [for (arg in args) arg.t]); // These need to capture generics?
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

	// try to evaluate the field to a constant, if it works, set the value for the CIL member
	function tryInitField(field:FieldDef, typeDef:TypeDef) {
		throw new NotImplementedException();
	}

	// System.Dynamic.DynamicObject or ExpandoObject or whatever? But also, maybe some homegrown type for this?
	function getDynamicTypeRef():BaseTypeRef {
		throw new NotImplementedException();
	}

	// da meat
	function mapToClrMethodBody(expr:hscript.Expr, method:MethodDef, ret:TType, ?withType:TType, ?_after:Void->Void) {
		inline function after()
			if (_after != null)
				_after();
		var with = if (withType != null) WithType(withType) else null;
		expr.iter(e -> switch e.expr() {
			case EVar(n, t, e): // should support an array... e.g. var a,b,c;
				// technically after should be null at this point.. not sure if should throw
				var type = types.checker.check(e, if (t != null) WithType(types.toTType(t)) else null);
				if (closure == null)
					locals.set(n, toClrTypeRef(type));
				else
					closure.locals.set(n, toClrTypeRef(type));
				mapToClrMethodBody(e, method, ret, type, () -> localIdInstr(IntOp.stloc_s, n, e.location()));
			case EBlock(e):
				var from = gen.CurrentMethodDef.AddLabel();
				gen.CurrentMethodDef.BeginLocalsScope();
				for (expr in e)
					mapToClrMethodBody(expr, method, ret, withType);
				after();
				previous = new HandlerBlock(from, gen.CurrentMethodDef.AddLabel());
				gen.CurrentMethodDef.EndLocalsScope();
			case EReturn(e):
				mapToClrMethodBody(e, method, ret, withType, _after);
				noneInstr(peapi.Op.ret, e.location());
			case EIdent(e):
				handleIdent(e);
			case EUnop(op, prefix, e):
				var type = types.checker.check(e, with);
				handleUnop(op, prefix, e, type);
				after();
			case EBinop(op, e1, e2):
				var type1 = types.checker.check(e1, with);
				var type2 = types.checker.check(e2, WithType(type1));
				handleBinop(op, [e1, e2], [type1, type2]);
				after();
			case ECall(e, params):
				var callerType = types.checker.check(e, with);
				var paramTypes = [];
				switch callerType {
					case TFun(args, ret):
						paramTypes = [
							for (i in 0...params.length) {
								var arg = params[i];
								var argType = args[i].t;
								types.checker.check(arg, if (argType != null) WithType(argType) else null);
							}
						];
					// not sure if the default case is right... I mean, there's callable types in Haxe
					default: throw '${printer.exprToString(e)} is not callable (${e.location()})';
				}
				// i think potentially withType below should be the ret from TFun above..
				handleCall(e, callerType, params, paramTypes, withType);
				after();
			case EIf(cond, e1, e2):
				types.checker.check(cond, WithType(TBool));

				// var branchEnd:LabelInfo = null;
				mapToClrMethodBody(cond, method, ret, withType);
				brTargetIdInstr(BranchOp.brfalse_s, referenceLabel(LabelRefs.END_COND), cond.location());
				mapToClrMethodBody(e1, method, ret, withType);
				after();
				brTargetIdInstr(BranchOp.br_s, referenceLabel(LabelRefs.END_IF), e1.location());
				noneInstr(peapi.Op.nop, e1.location());
				var endOfConditionLabel = placeLabelRef(LabelRefs.END_COND);
				if (e2 != null) {
					mapToClrMethodBody(e2, method, ret, withType, _after);
				}
				noneInstr(peapi.Op.nop, e.location());
				var endIfLabel = placeLabelRef(LabelRefs.END_IF);
			case EWhile(cond, e):
				types.checker.check(cond, WithType(TBool));
				mapToClrMethodBody(cond, method, ret, withType);
				brTargetIdInstr(BranchOp.brfalse_s, referenceLabel(LabelRefs.END_LOOP), cond.location());
				mapToClrMethodBody(e, method, ret, withType);
			case EFor(v, it, e):
			// handle int in 0...n with a more optimized approach than obj in iterable
			case EFunction(args, e, name, ret):

			case EArray(e, index): // array access

			case EArrayDecl(e): // array declaration
			case ENew(cl, params): // newobj
			case EThrow(e):
			case ETry(e, v, t, ecatch): // TODO: hscript: catch should be an array....
			case EObject(fl):
			case ETernary(cond, e1, e2):
			case ESwitch(e, cases, defaultExpr):
			case EDoWhile(cond, e):
			case EMeta(name, args, e):
			case ECheckType(e, t):
			// this is probably where I'll get a good chance to fix CLR generic calling convention...
			default:
		});
	}

	// haxe.lang.Function? Right? Wrapping a delegate?
	function getFunctionTypeRef(args:Array<{name:String, opt:Bool, t:TType}>, ret:TType):BaseClassRef {
		throw new NotImplementedException();
	}

	// begin shameless ripping from ILParser.jay
	inline function noneInstr(opcode:peapi.Op, loc:Location)
		gen.CurrentMethodDef.AddInstr(new SimpInstr(opcode, loc));

	inline function localIntInstr(opcode:IntOp, int:Int, loc:Location)
		gen.CurrentMethodDef.AddInstr(new IntInstr(opcode, int, loc));

	inline function localIdInstr(opcode:IntOp, id:String, loc:Location) {
		var slot = gen.CurrentMethodDef.GetNamedLocalSlot(id);
		if (slot < 0)
			throw 'Undeclared identifier $id';
		gen.CurrentMethodDef.AddInstr(new IntInstr(opcode, slot, loc));
	}

	inline function paramInt32Instr(opcode:IntOp, int:Int, loc:Location)
		return localIntInstr(opcode, int, loc);

	inline function paramIdInstr(opcode:IntOp, id:String, loc:Location) {
		var pos = gen.CurrentMethodDef.GetNamedParamPos(id);
		if (pos < 0)
			throw 'Undeclared identifier $id';
		gen.CurrentMethodDef.AddInstr(new IntInstr(opcode, pos, loc));
	}

	inline function intInt32Instr(opcode:IntOp, int:Int, loc:Location)
		return localIntInstr(opcode, int, loc);

	inline function intIdInstr(opcode:IntOp, id:String, loc:Location)
		return localIdInstr(opcode, id, loc);

	inline function r8Int64Instr(instr:MiscInstr, long:Int64, loc:Location)
		switch instr {
			case MiscInstr.ldc_i8:
				gen.CurrentMethodDef.AddInstr(new LdcInstr(instr, long, loc));
			default:
		}

	inline function r8Float64Instr(instr:MiscInstr, double:Float, loc:Location)
		switch instr {
			case MiscInstr.ldc_r4 | MiscInstr.ldc_i8:
				gen.CurrentMethodDef.AddInstr(new LdcInstr(instr, double, loc));
			default:
		}

	inline function brTargetInt32Instr(branchOp:BranchOp, int:Int, loc:Location) {
		var target = gen.CurrentMethodDef.AddLabel(int);
		gen.CurrentMethodDef.AddInstr(new BranchInstr(branchOp, target, loc));
	}

	inline function brTargetIdInstr(branchOp:BranchOp, id:String, loc:Location) {
		var target = gen.CurrentMethodDef.AddLabelRef(id);
		gen.CurrentMethodDef.AddInstr(new BranchInstr(branchOp, target, loc));
	}

	inline function methodInstr(methodOp:MethodOp, methRef:BaseMethodRef, loc:Location)
		gen.CurrentMethodDef.AddInstr(new MethodInstr(methodOp, methRef, loc));

	inline function fieldInstr(fieldOp:FieldOp, type:BaseTypeRef, owner:BaseTypeRef, name:String, loc:Location) {
		// not sure if this is right
		var gpr = Std.downcast(type, GenericParamRef);
		if (gpr != null && gen.CurrentMethodDef != null)
			gen.CurrentMethodDef.ResolveGenParam(cast gpr.PeapiType);
		var fieldref = owner.GetFieldRef(type, name);
		gen.CurrentMethodDef.AddInstr(new FieldInstr(fieldOp, fieldref, loc));
	}

	inline function basicFieldInstr(fieldOp:FieldOp, type:BaseTypeRef, name:String, loc:Location) {
		var fieldRef = gen.GetGlobalFieldRef(type, name);
		gen.CurrentMethodDef.AddInstr(new FieldInstr(fieldOp, fieldRef, loc));
	}

	inline function typeInstr(typeOp:TypeOp, type:BaseTypeRef, loc:Location)
		gen.CurrentMethodDef.AddInstr(new TypeInstr(typeOp, type, loc));

	inline function loadStringInstr(string:String, loc:Location)
		gen.CurrentMethodDef.AddInstr(new LdstrInstr(string, loc));

	inline function callInstr(callingConvention:CallConv, type:BaseTypeRef, types:Array<BaseTypeRef>, loc:Location)
		gen.CurrentMethodDef.AddInstr(new CalliInstr(callingConvention, type, cs.Lib.nativeArray(types, false), loc));

	inline function tokenInstr(instr:MiscInstr, tokenType:Either<IFieldRef, Either<BaseMethodRef, BaseTypeRef>>, loc:Location)
		gen.CurrentMethodDef.AddInstr(switch tokenType {
			case Left(v):
				new LdtokenInstr(v, loc);
			case Right(v):
				switch v {
					case Left(v):
						new LdtokenInstr(v, loc);
					case Right(v):
						new LdtokenInstr(v, loc);
				}
		});

	inline function switchInstr(labels:Array<LabelInfo>, loc:Location)
		gen.CurrentMethodDef.AddInstr(new SwitchInstr({
			var list = new ArrayList();
			for (label in labels)
				list.Add(label);
			list;
		}, loc));

	function handleUnop(op:String, prefix:Bool, e:Expr, type:TType) {
		throw new haxe.exceptions.NotImplementedException();
	}

	function handleBinop(op:String, arg1:Array<Expr>, arg2:Array<TType>) {
		throw new haxe.exceptions.NotImplementedException();
	}

	function handleCall(e:Expr, callerType:TType, params:Array<Expr>, paramTypes:Array<TType>, retType) {
		throw new haxe.exceptions.NotImplementedException();
	}

	// so, to reference a field in CLR, you need to know if
	// its a local or a field (there's different instructions for
	    // accessing locals and fields)
	// also, any locals that get captured/referenced inside a closure have to be
	    // moved into the closure as member fields.
	function handleIdent(e:String) {
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

enum abstract LabelRefs(String) from String to String {
	var END_IF = "END_IF_";
	var END_COND = "END_COND_";
	var END_LOOP = "END_LOOP_";
}

interface Closure {
	var name:String;
	var locals:Map<String, BaseTypeRef>;
	var methods:Array<{name:String, f:FunctionDecl}>;
}
