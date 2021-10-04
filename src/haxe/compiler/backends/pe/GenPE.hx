package haxe.compiler.backends.pe;

import peapi.GenericParamConstraint;
import peapi.GenericParamAttributes;
import mono.ilasm.GenericParameter;
import mono.ilasm.GenericParameters;
import haxe.compiler.backends.Core.Gen;
import hscript.Printer;
import cs.system.reflection.AssemblyName;
import cs.NativeArray;
import mono.ilasm.CatchBlock;
import mono.ilasm.TryBlock;
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
import mono.ilasm.TypeDef;

using hscript.Tools;

import hscript.Interp;
import hscript.Checker.TType;
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
using hscript.Checker;
using haxe.compiler.backends.pe.GenPE;

typedef Deferred = Array<Void->Void>;

class GenPE extends Gen {
    public var gen:CodeGen;
    public var peTypes:PECheckerTypes;

    static var COMPILER_GENERATED_PREFIX = "<>_";

    var handlerBlock:HandlerBlock;
    var locals:Map<String, BaseTypeRef> = [];
    var localSlots:Map<String, Int> = [];
    var beforeNextType:Array<Void->Void> = [];
    var closure:Closure;
    var dynamicCallSites:Array<Dynamic>; // not sure what info this will need just yet but.. need to record it for dynamics
    var next:() -> Void;
    var meta:{name:String, args:Array<Expr>, expr:Expr};

    var outputFile = '';
    var labelRefCount:Map<String, Int> = [];
    var localRefCount:Map<String, Int> = [];
    var currentPackage = '';
    var constructorFields:Array<{field:FieldDef, decl:VarDecl}> = [];
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
        this.types = this.peTypes = new PECheckerTypes();
        this.types.checker = new hscript.Checker(this.types);
    }

    // wait that's not how static initializers work, I guess just macros won't work.
    // So the AST would have to have had macros already applied
    public function buildHaxe(types:Array<haxe.macro.Type>) {
        throw 'not implemented';
    }

    public function buildHscript(sourceModule, types) {
        gen.BeginSourceFile('$sourceModule.hscript');
        // TODO: Add actual mechanism for loading assemblies...
        loadAssemblies();
        gen.SetThisAssembly({
            var l = outputFile.split('.');
            l.pop();
            l.join('.');
        }, peapi.AssemAttr.Retargetable);
        gen.CurrentCustomAttrTarget = gen.ThisAssembly;
        gen.CurrentDeclSecurityTarget = gen.ThisAssembly;
        init();
        firstPass(types);
        secondPass(types);
        finalPass(types);
        gen.EndSourceFile();
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
                    var name = currentPackage + '.${c.name}';
                    var flags = 0;

                    if (c.isPrivate)
                        flags = flags | cast TypeAttr.Private;
                    else
                        flags = flags | cast TypeAttr.Public;
                    var superCl = switch c.extend {
                        case CTPath(path, params): '${path.join('.')}';
                        default: null;
                    }

                    var superType = if (superCl != null) gen.GetTypeRef(superCl); else null;
                    gen.BeginTypeDef(cast flags, c.name.split('.').pop(), superType, null, type.location(), null);
                    for (field in c.fields) {
                        switch field.kind {
                            case KFunction(f):
                                if (field.name == 'new')
                                    currentConstructor = {field: field, decl: f};
                                else
                                    generateMethod(name, field, f);
                                null;
                            case KVar(v):
                                generateVar(name, field, v);
                        }
                    }
                    generateConstructor();
                    afterType();
                    gen.EndTypeDef();

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
        if (isSpecialName(field.name))
            flags |= cast MethAttr.RTSpecialName;

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

        var ret = types.checker.check(f.expr, if (f.ret != null) WithType(types.checker.makeType(f.ret, f.expr)) else WithType(TVoid));
        var retType = toClrTypeRef(ret);

        var ownerType = gen.CurrentTypeDef; // lookupType(owner).toClrTypeDef(this);
        var paramList = new cs.system.collections.ArrayList();
        for (arg in f.args)
            paramList.Add(if (arg.value == null) types.checker.makeType(arg.t,
                arg.value) else types.checker.check(arg.value, WithType(types.checker.makeType(arg.t, arg.value))));
        var genericParameters = getGenericParameters(f);

        var method = new MethodDef(gen, cast flags, cast conv, implAttr, field.name, retType, paramList, f.expr.location(), genericParameters, ownerType);

        if (field.name == "main" && owner == mainClass)
            method.EntryPoint();
        method.SetMaxStack(8); // apparently this only matters for PE verification and has nothing to do with runtime
        // so, unless this value is intelligently set, there's a chance the PE won't be verifiable
        // BUT by default .NET allows unverifiable PEs so.... is it worth the bother? Not right now
        labelRefCount = [];
        locals = [];
        mapToClrMethodBody(f.expr, method, ret);
        generateLocals();
        if (retType.FullName == "System.Void") {
            noneInstr(peapi.Op.ret, f.expr.location());
        }
        if (closure != null)
            beforeNextType.push(() -> defineClosure());
        gen.EndMethodDef(f.expr.location());
    }

    function referenceLabel(refName) {
        var c = labelRefCount[refName];
        if (c == null)
            c = 0;
        return '$refName$c';
    }

    function getLocal(local) {
        var c = if (!labelRefCount.exists(local)) {
            labelRefCount[local] = 0;
        } else labelRefCount[local]++;
        return '$local$c';
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
        var type = if (v.expr != null) types.checker.check(v.expr,
            WithType(types.checker.makeType(v.type, v.expr))) else types.checker.makeType(v.type, v.expr);
        var clrType = toClrTypeRef(type);
        var field = new FieldDef(flags, name, clrType);
        gen.AddFieldDef(field);
        types.checker.setGlobal(name, type);
        constructorFields.push({field: field, decl: v});
    }

    public function setMainClass(c) {
        mainClass = c;
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
        return if (types == null || types.length == 0) e else e.GetGenericTypeInst({
            var args = new GenericArguments();
            for (t in types)
                args.Add(toClrTypeRef(t));
            args;
        });

    function toClrTypeRef(t:TType):BaseTypeRef {
        trace(t);
        return // this of course can only be called after the first pass
            if (!firstPassComplete) throw 'First pass incomplete; cannot convert types'; else switch types.checker.follow(t) {
                case TMono({r: null}): Primitives.VOID;
                case TVoid: Primitives.VOID;
                case TInt: Primitives.INT;
                case TFloat: Primitives.FLOAT;
                case TBool: Primitives.BOOL;
                case TDynamic: getDynamicTypeRef();
                case TAbstract(a, args): toClrTypeRef(types.checker.apply(a.t, a.params, args));
                case TInst(c, args): clrApply(getTypeRef(c.name), args);
                case TNull(t): clrApply(gen.ExternTable.GetTypeRef("mscorlib", "System.Nullable", false), [t]);
                case TEnum(e, args): clrApply(getTypeRef(e.name), args);
                case TType(t, args): clrApply(getTypeRef(t.name), args);
                case TFun(args, ret): clrApply(getFunctionTypeRef(args, ret), [for (arg in args) arg.t]); // These need to capture generics?
                case TAnon(fields): getDynamicTypeRef();
                default:
                    throw 'Error, invalid type: $t';
                case TParam(name, index):
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

    // System.Dynamic.DynamicObject or ExpandoObject or whatever? But also, maybe some homegrown type for this?
    function getDynamicTypeRef():BaseTypeRef {
        throw new NotImplementedException();
    }

    // da meat
    function mapToClrMethodBody(expr:hscript.Expr, method:MethodDef, ret:TType, ?withType:TType, ?_after:Void->Void, ?_before:Void->Void) {
        inline function doNext(?done)
            if (next != null) {
                next();
                if (done)
                    next = null;
            }
        inline function after()
            if (_after != null)
                _after();
        inline function before()
            if (_before != null)
                _before();
        inline function doIf(e:Expr, cond:Expr, e1:Expr, e2:Null<Expr>) {
            // var branchEnd:LabelInfo = null;
            mapToClrMethodBody(cond, method, ret, withType);
            brTargetIdInstr(BranchOp.brfalse_s, referenceLabel(LabelRefs.END_COND), cond.location());
            doNext();
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
        }

        var with = if (withType != null) WithType(withType) else null;
        // make sure we move locals to closure as necessary, also define locals up front
        function firstPass(e:Expr) {
            trace(e.expr());
            switch e.expr() {
                case EIf(cond, e1, e2):
                    firstPass(cond);
                    firstPass(e1);
                    if (e2 != null)
                        firstPass(e2);

                case EBlock(e):
                    for (expr in e)
                        firstPass(expr);
                case EVar(n, t, e):
                    var type = types.checker.check(e, if (t != null) WithType(types.checker.makeType(t, expr)) else null);
                    declareLocal(n, type);
                case EFor(v, it, e):
                    var type = switch types.checker.check(it) {
                        case TAnon(f):
                            switch f[0].t {
                                case TFun(args, ret): ret;
                                default: throw 'Invalid iterator type';
                            }
                        default: throw 'Invalid iterator type';
                    }

                    declareLocal(v, type);
                    firstPass(e);
                case ETry(e, v, t, ecatch):
                    var catchType = types.checker.makeType(t, expr);
                    declareLocal(v, catchType);
                    firstPass(e);
                    firstPass(ecatch);
                case EFunction(_, {expr: e}):
                    e.iter(e -> switch e.expr() {
                        case EIdent(v):
                            if (isLocal(v)) {
                                moveToClosure(v);
                            }
                        default:
                    });
                default:
            }
        }
        function doMap(e:Expr)
            switch e.expr() {
                case EIdent('null'):
                    noneInstr(peapi.Op.ldnull, e.location());
                    after();
                case EIdent('true'): // this should be EConst.CBool(v)...
                    noneInstr(peapi.Op.ldc_i4_1, e.location());
                    after();
                case EIdent('false'): // this should be EConst.CBool(v)...
                    noneInstr(peapi.Op.ldc_i4_0, e.location());
                    after();
                case EConst(c):
                    switch c {
                        case CInt(v):
                            intInt32Instr(IntOp.ldc_i4_s, v, e.location());
                        case CFloat(f):
                            r8Float64Instr(MiscInstr.ldc_r8, f, e.location());
                        case CString(s):
                            loadStringInstr(s, e.location());
                    }
                    after();
                case EVar(n, t, e): // should support an array... e.g. var a,b,c;
                    // technically after should be null at this point.. not sure if should throw
                    types.checker.allowGlobalsDefine = true;
                    var type = types.checker.check(e, if (t != null) WithType(types.checker.makeType(t, expr)) else null);
                    mapToClrMethodBody(e, method, ret, type, () -> setVar(n, toClrTypeRef(types.checker.makeType(t, expr)), e.location()));
                    doNext();
                case EBlock(e):
                    var from = gen.CurrentMethodDef.AddLabel();
                    gen.CurrentMethodDef.BeginLocalsScope();
                    before();
                    for (expr in e)
                        mapToClrMethodBody(expr, method, ret, withType);
                    after();
                    handlerBlock = new HandlerBlock(from, gen.CurrentMethodDef.AddLabel());
                    gen.CurrentMethodDef.EndLocalsScope();
                case EReturn(e):
                    mapToClrMethodBody(e, method, ret, withType, _after);
                    noneInstr(peapi.Op.ret, e.location());
                case EIdent(i):
                    trace(locals);
                    handleIdent(i, e.location());
                case EUnop(op, prefix, e):
                    var type = types.checker.check(e, with);
                    handleUnop(op, prefix, e, type);
                    after();
                case EBinop(op, e1, e2):
                    var type1 = types.checker.check(e1, with);
                    var type2 = types.checker.check(e2, WithType(type1));
                    handleBinop(op, [e1, e2], [type1, type2]);
                    after();
                    doNext();
                case ECall(e, params):
                    var funcType = types.checker.check(e, with);
                    // i think potentially withType below should be the ret from TFun above..
                    handleCall(e, funcType, params, [for (param in params) types.checker.check(param)], withType);
                    after();
                case EIf(cond, e1, e2):
                    doIf(e, cond, e1, e2);
                case EWhile(cond, e):
                    var beginLoop = placeLabelRef(LabelRefs.BEGIN_LOOP);
                    mapToClrMethodBody(cond, method, ret, withType);
                    brTargetIdInstr(BranchOp.brfalse_s, referenceLabel(LabelRefs.END_LOOP), cond.location());
                    mapToClrMethodBody(e, method, ret, withType);
                    brTargetIdInstr(BranchOp.br_s, beginLoop.Name, e.location());
                    noneInstr(peapi.Op.nop, e.location());
                    placeLabelRef(LabelRefs.END_LOOP);
                case EFor(v, it, e):
                    // handle int in 0...n with a more optimized approach than obj in iterable
                    var loopVar = v;

                    switch it.expr() {
                        case EBinop('...', min, max) if (types.checker.check(min).match(TInt) && types.checker.check(max).match(TInt)):
                            // TODO: first pass over method body for closures
                            // intInt32Instr(IntOp.ldc_i4, min, it.location());
                            doMap(min);
                            doNext();
                            setVar(loopVar, Primitives.INT, e.location());
                            var beginLoop = placeLabelRef(LabelRefs.BEGIN_LOOP);
                            doMap(max);
                            doNext();
                            handleIdent(loopVar, e.location());
                            brTargetIdInstr(BranchOp.beq, referenceLabel(LabelRefs.END_LOOP), it.location());
                            mapToClrMethodBody(e, method, ret, withType);
                            noneInstr(peapi.Op.ldc_i4_1, e.location());
                            handleIdent(loopVar, e.location());
                            noneInstr(peapi.Op.add, e.location());
                            setVar(loopVar, Primitives.INT, e.location());
                            brTargetIdInstr(BranchOp.br_s, beginLoop.Name, e.location());
                            placeLabelRef(LabelRefs.END_LOOP);
                        default:
                            // TODO: crap... forgot the checker needs locals loaded to it...
                            // TODO: crap, forgot again
                    }
                case EFunction(kind, decl):
                    var closureMethod = addToClosure(kind, decl);
                    var fieldExpr = EField(EIdent(getClosureLocal()).mk(e), closureMethod).mk(e);
                    // <closureLocal>.<closureMethod>;
                    mapToClrMethodBody(fieldExpr, gen.CurrentMethodDef, ret, withType, _after, _before);
                case EArray(e, index): // array access
                // if has an indexer decl, do indexer access
                // e.g. .method public final hidebysig virtual newslot specialname instance !0/*T*/ get_Item
                // and.method public final hidebysig virtual newslot specialname instance void set_Item
                // if it doesn't have an indexer decl,
                // check for Haxe abstract array access override...
                case EArrayDecl(e): // array declaration
                // if the types of expressions in the array are the the same type T: create a haxe Array<T>
                // if the types of expressions in the array are different types: create a haxe Array<Dynamic>
                // array comprehension... speaking of, TODO: hscript, need EMapDecl(Array<{k:Expr,v:Expr})
                case ENew(cl, params): // newobj
                // if it's not an abstract, nothing fancy here
                // otherwise bang head on keyboard
                case EThrow(e):
                    var exception:Expr = convertToException(e);
                    mapToClrMethodBody(exception, method, ret);
                    noneInstr(peapi.Op.throwOp, e.location());
                case ETry(e, v, t, ecatch): // TODO: hscript: catch should be an array....
                    // similar semantics as try/catch in c#
                    if (!e.expr().match(EBlock(_))) {
                        e.e = EBlock([e]);
                    }
                    if (!ecatch.expr().match(EBlock(_))) {
                        ecatch.e = EBlock([e]);
                    }
                    var catchType = toClrTypeRef(types.checker.makeType(t, expr));
                    mapToClrMethodBody(e, method, ret, withType, () -> brTargetIdInstr(BranchOp.leave_s, referenceLabel(LabelRefs.END_TRY), e.location()));

                    var tryBlock = new TryBlock(handlerBlock, e.location());
                    mapToClrMethodBody(ecatch, method, ret, withType, _after, () -> setVar(v, catchType, ecatch.location()));
                    brTargetIdInstr(BranchOp.leave_s, referenceLabel(LabelRefs.END_TRY), e.location());
                    var cb = new CatchBlock(catchType);
                    cb.SetHandlerBlock(handlerBlock);
                    tryBlock.AddSehClause(cb);
                    gen.CurrentMethodDef.AddInstr(tryBlock);
                    placeLabelRef(LabelRefs.END_TRY);
                case EObject(fl): // handle this very last...
                // basically have to create:
                // - Some helper for Haxe runtime callsite bindings
                // - haxe.lang.DynamicObject (which needs to contain a reference to the underlying object and also not be derived from haxe.lang.HxObject)
                // - And also I guess anons or something should be derived from that..
                // and we should be able to optimize callsite bindings for anons/typedefs since we know the fields up front..
                // unlike c# dynamics
                // though haxe Dynamics will work pretty much just like c# ones
                case ETernary(cond, e1, e2):
                    doIf(e, cond, e1, e2);
                case ESwitch(e, cases, defaultExpr):
                // god this is going to be fun.. pattern matching
                // for anons/typedefs, the subject of the switch needs to be converted to
                case EDoWhile(cond, e):
                    var beginLoop = placeLabelRef(LabelRefs.BEGIN_LOOP);
                    mapToClrMethodBody(e, method, ret, withType);
                    mapToClrMethodBody(cond, method, ret, withType);
                    brTargetIdInstr(BranchOp.brfalse_s, referenceLabel(LabelRefs.END_LOOP), cond.location());
                    brTargetIdInstr(BranchOp.br_s, beginLoop.Name, e.location());
                    noneInstr(peapi.Op.nop, e.location());
                    placeLabelRef(LabelRefs.END_LOOP);
                case EMeta(name, args, e):
                    meta = {name: name, args: args, expr: e};
                    mapToClrMethodBody(e, method, ret, withType, _after);
                case ECheckType(e, t): // or not
                    mapToClrMethodBody(e, method, ret, types.checker.makeType(t, expr), _after);
                default:
            }
        if (expr.expr().match(EBlock(_))) {
            firstPass(expr);
            setLocalSlots();
        }
        doMap(expr);
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
        var slot = localSlots[id];
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
            case MiscInstr.ldc_r4 | MiscInstr.ldc_r8:
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

    inline function callInstr(callingConvention:CallConv, methodRef:BaseMethodRef, loc:Location) {
        gen.CurrentMethodDef.AddInstr(new MethodInstr(MethodOp.call, methodRef, loc));
    }

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
        switch op {
            case '--' | '++':
                handleIncOrDec(op, prefix, e, type);
            case '!' if (prefix):
                mapToClrMethodBody(e, gen.CurrentMethodDef, null);
                noneInstr(peapi.Op.ldc_i4_0, e.location());
                noneInstr(peapi.Op.ceq, e.location());
            case '~' if (prefix):
                mapToClrMethodBody(e, gen.CurrentMethodDef, null);
                noneInstr(peapi.Op.not, e.location());
            default:
                throw 'invalid unary operator: $op (near ${printer.exprToString(e)})';
        }
    }

    function handleIncOrDec(op:String, prefix:Bool, e:Expr, type:TType) {
        var ident = '';
        var opCode:peapi.Op = peapi.Op.nop;
        inline function pre() {
            switch e.expr() {
                case EIdent(v):
                    ident = v;
                    handleIdent(v, e.location());
                default:
            }

            noneInstr(peapi.Op.ldc_i4_1, e.location());
            noneInstr(opCode, e.location());
            setVar(ident, toClrTypeRef(types.checker.check(e)), e.location());
            handleIdent(ident, e.location());
        }
        function post() {
            switch e.expr() {
                case EIdent(v):
                    ident = v;
                    handleIdent(v, e.location());
                default:
            }
            noneInstr(peapi.Op.ldc_i4_1, e.location());
            noneInstr(opCode, e.location());
            setVar(ident, toClrTypeRef(types.checker.check(e)), e.location());
        }
        switch type {
            case TInt | TFloat: // lets only handle numbers for now...
                switch op {
                    case '++': opCode = peapi.Op.add;
                    case '--': opCode = peapi.Op.sub;
                }
            default:
        }
        if (prefix)
            pre();
        if (!prefix)
            next = post;
    }

    function handleBinop(op:String, arg1:Array<Expr>, arg2:Array<TType>)
        throw new NotImplementedException();

    function handleCall(e:Expr, funcType:TType, params:Array<Expr>, paramTypes:Array<TType>, retType:TType) {
        // so first we had to set up a hell of a lot of state

        var methodName = '';
        var callerType = types.checker.check(e);
        var callerClrType = switch e.expr() {
            case EField(e, f): // method call
                methodName = f;
                toClrTypeRef(callerType);
            case EIdent(v): // closure
                methodName = v;
                closure.clrTypeRef;
            case EFunction(k, f): // IIFE
                methodName = addToClosure(k, f);
                closure.clrTypeRef;
            default: throw 'assert';
        }
        var isInstanceMethod = !types.checker.isFieldStatic(callerType, methodName, paramTypes, retType);

        var funcArgs = switch funcType {
            case TFun(args, ret):
                args;
            default: throw 'assert';
        }

        var genericParamMap:Map<Int, BaseTypeRef> = [];
        for (i in 0...funcArgs.length) {
            var funcArg = funcArgs[i];
            var paramType = paramTypes[i];
            if (types.checker.tryUnify(funcArg.t, paramType)) {
                // if the signature arg type and the arg expression type unify..
                switch funcArg.t {
                    // and the function arg type is a generic parameter...
                    case TParam(name, index):
                        genericParamMap.set(index, toClrTypeRef(paramType)); // map it by index
                        for (i in i...funcArgs.length) { // loop through remaining signature arg types
                            var funcArg = funcArgs[i];
                            switch funcArg.t { // if the signature arg type is the same generic parameter
                                case TParam(name2, index2) if (index == index2 && name == name2): funcArg.t = paramType;
                                // change it to the type  we just unified with
                                // this will prevent the same parameter from being remapped
                                // as well as force them to unify
                                default:
                            }
                        }
                    default:
                }
            } else { // if they don't unify...
                if (!funcArg.opt)
                    throw 'parameter type mismatch in call expression (have: ${paramType.typeStr()}, want: ${funcArg.t.typeStr()} )';
                else { // and its not optional
                    paramTypes.insert(i, funcArg.t);
                    params.insert(i, EIdent('null').mk(null)); // pass null value in for this arg...
                }
            }
        }

        var genericParamNames:Map<Int, String> = [];
        var genericParamCount = 0;
        for (paramType in funcArgs) {
            switch paramType.t {
                case TParam(param, i):
                    if (genericParamNames.exists(i)) {
                        genericParamNames.set(i, param);
                        genericParamCount++;
                    }
                default:
            }
        }

        // yes... all of that just to set up state for generic parameters (and also handle optional params)
        var genericArguments = if (genericParamCount != 0) new GenericArguments() else null;
        if (genericArguments != null)
            for (i in 0...genericParamMap.list().length) {
                genericArguments.Add(genericParamMap[i]);
            }

        var callConv = if (isInstanceMethod) CallConv.Instance else CallConv.Default;

        var retClrType = toClrTypeRef(retType);

        var methRef = callerClrType.GetMethodRef(retClrType, callConv, methodName,
            cs.Lib.nativeArray([for (funcArg in funcArgs) toClrTypeRef(funcArg.t)], true), genericParamCount);
        if (genericArguments != null)
            methRef.GetGenericMethodRef(genericArguments);
        // and finally... do the actual mapping to CLR body

        // If it's not static, put the instance on the stack
        if (isInstanceMethod) {
            mapToClrMethodBody(e, gen.CurrentMethodDef, null, callerType);
        }
        // put the parameters on the stack
        for (i in 0...params.length) {
            var param = params[i];
            var paramType = paramTypes[i];
            var arg = funcArgs[i];
            var argType = arg.t;
            params[i] = doConversion(param, paramType, argType);
            mapToClrMethodBody(params[i], gen.CurrentMethodDef, null, argType);
        }
        methodInstr(MethodOp.call, methRef, e.location());
    }

    // so, to reference a field in CLR, you need to know if
    // its a local or a field (there's different instructions for
    // accessing locals and fields)
    // also, any locals that get captured/referenced inside a closure have to be
    // moved into the closure as member fields.
    function handleIdent(e:String, location:Location) {
        if (locals.exists(e)) {
            localIdInstr(IntOp.ldloc_s, e, location);
        } else if (closure != null && closure.locals.exists(e)) {
            localIdInstr(IntOp.ldloc_s, getClosureLocal(), location);
            var owner = gen.GetTypeRef(closure.name);
            var type = toClrTypeRef(types.checker.check(EIdent(e).mk(null)));
            fieldInstr(FieldOp.ldfld, type, owner, e, location);
        }
    }

    function afterType() {
        for (cb in beforeNextType)
            cb();
    }

    function defineClosure() {
        if(closure == null) return;
        gen.BeginTypeDef(TypeAttr.Private, closure.name, null, null, new Location(0,0), null);
        for(name => type in closure.locals) {
            var attr = FieldAttr.Public;
            var fieldDef = new FieldDef(attr, name, type);
            gen.CurrentTypeDef.AddFieldDef(fieldDef);
        }

        for(name => decl in closure.methods) {
            var flags:Int = cast MethAttr.Public;
            flags |= cast MethAttr.HideBySig;
            if (isSpecialName(name))
                flags |= cast MethAttr.RTSpecialName;

            var conv= peapi.CallConv.Instance;
            if (name == 'new') {
                flags |= cast MethAttr.SpecialName;
                flags |= cast MethAttr.RTSpecialName;
            }

            var implAttr = ImplAttr.IL;
            var retType = types.checker.makeType(decl.ret);
            var retClrType = toClrTypeRef(retType);
            var paramList ={
                var ret = new ArrayList();
                for(arg in decl.args) ret.Add(toClrTypeRef(types.checker.makeType(arg.t))); 
                ret;
            };
            var startLocation = new Location(0,0);
            var genParams = getGenericParameters(decl);
            var method = new MethodDef(gen, cast flags, conv, implAttr, name, retClrType, paramList, startLocation, genParams, gen.CurrentTypeDef);
            method.SetMaxStack(8); 
            mapToClrMethodBody(decl.expr, method, retType);

        }

        
    }

    function setVar(varName:String, type:BaseTypeRef, location:Location, ?pos:haxe.PosInfos) {
        trace(pos);
        trace(varName);
        if (locals.exists(varName)) {
            localIdInstr(IntOp.stloc_s, varName, location);
        } else if (closure != null && closure.locals.exists(varName)) {
            fieldInstr(FieldOp.stfld, type, gen.GetTypeRef(closure.name), varName, location);
        }
    }

    function mkClosure():Closure {
        var name = '$$${gen.CurrentTypeDef.Name}_${gen.CurrentMethodDef.Name}_HxClosure';
        return {
            name: name,
            methods: [],
            locals: [],
            clrTypeRef: gen.GetTypeRef(name)
        };
    }

    function isLocal(v:String):Bool
        return locals.exists(v);

    function moveToClosure(v:String) {
        var local = locals[v];
        if (locals.remove(v)) {
            closure.locals.set(v, local);
        }
    }

    function convertToException(e:Expr):Expr {
        throw new haxe.exceptions.NotImplementedException();
    }

    function generateConstructor() {
        var constructor:MethodDecl = if (currentConstructor == null) {
            var decl:FunctionDecl = {
                ret: null,
                expr: EBlock([]).mk({
                    e: null,
                    pmin: 0,
                    pmax: 0,
                    origin: null,
                    line: 0
                }),
                args: []
            };
            {
                field: {
                    name: 'new',
                    meta: [],
                    kind: KFunction(decl),
                    access: [APrivate]
                },
                decl: decl
            };
        } else currentConstructor;
        function mk(c:Expr)
            return switch c.expr() {
                case EBlock(exprs):
                    for (field in constructorFields) {
                        var fieldReference = 'this.${field.field.Name}';

                        exprs.unshift(EBinop('=', EIdent('$fieldReference').mk(c), field.decl.expr).mk(c));
                    }
                    EBlock(exprs).mk(c);
                case v: mk(EBlock([c]).mk(c));
            }
        constructor.decl.expr = mk(constructor.decl.expr);
        constructor.field.name = '.ctor';
        trace(printer.exprToString(constructor.decl.expr));
        generateMethod('', constructor.field, constructor.decl);
        constructorFields = [];
    }

    var currentConstructor:MethodDecl;

    // @formatter:off
    static var specialNames = [
        '.ctor',            // CONSTRUCTOR
        '.cctor'            // CLASS CONSTRUCTOR
    ];
    // @formatter:on
    function isSpecialName(arg0:String)
        return specialNames.contains(arg0);

    // @formatter:off
    static var valueTypes = [
        // "System.String", // I think? actually its not, a Char is though
        // "System.Int64",
        // "System.Int32",
        // "System.Boolean",
        // "System.Single",
        // "System.Double",
        // "System.Int16",
        // "System.Char",
        // "System.Byte", wait I'm pretty sure only structs are considered valuetypes...
    ];
    // @formatter:on
    function isValueType(type:String)
        return valueTypes.contains(type);

    function getTypeRef(arg0:String):BaseClassRef {
        var asm = peTypes.getAssembly(arg0);
        var valueType = isValueType(arg0);
        return if (asm != null) gen.ExternTable.GetTypeRef(asm, arg0, valueType); else gen.GetTypeRef(arg0);
    }

    function init()
        throw new NotImplementedException();

    function getClosureLocal():String {
        throw new haxe.exceptions.NotImplementedException();
    }

    function setLocalSlots() {
        var index = 0;
        for (local => type in locals) {
            localSlots[local] = index++;
        }
    }

    function declareLocal(n:String, type:TType, ?throwOnDup) {
        trace('declaring local $n');
        trace(type);
        types.checker.setGlobal(n, type);
        if (closure == null) {
            if (!locals.exists(n)) {
                locals.set(n, toClrTypeRef(type));
            } else if (throwOnDup)
                throw 'duplicate local variable $n redefined as $type';
        } else {
            if (!closure.locals.exists(n))
                closure.locals.set(n, toClrTypeRef(type));
            else if (throwOnDup)
                throw 'duplicate closure variable $n redefined as $type';
        }
    }

    function generateLocals() {
        gen.CurrentMethodDef.AddLocals({
            var arrList = new ArrayList();
            for (local => clrType in locals)
                arrList.Add(new Local(localSlots[local], local, clrType));
            arrList;
        });
    }

    // check if conversion is necessary, and if so, perform it
    function doConversion(param:Expr, paramType:TType, argType:TType):Expr {
        throw new haxe.exceptions.NotImplementedException();
    }

    var externAssemblies = ['mscorlib'];

    function loadAssemblies() {
        for (asm in externAssemblies) {
            gen.BeginAssemblyRef(asm, new AssemblyName(asm), peapi.AssemAttr.Retargetable);
            peTypes.loadAssembly(asm);
            gen.EndAssemblyRef();
        }
    }

    function getAnonClosureName():String {
        if (closure == null)
            return null;
        else
            return '$$anon_${closure.methods.list().length}';
    }

    function addToClosure(kind:FunctionKind, decl:FunctionDecl) {
        if (closure == null) {
            closure = mkClosure();
        }
        var name:String = switch kind {
            case FAnonymous | FArrow: getAnonClosureName();
            case FNamed(name, inlined): // do something about inlining
                name;
        }
        closure.methods.set(name, decl);
        return name;
    }

	function getGenericParameters(f:FunctionDecl) {
		var ret = new GenericParameters();
        for(param in f.params) {
            var constraints = new ArrayList();
            for(constraint in param.constraints) constraints.Add(toClrTypeRef(types.checker.makeType(constraint)));
            var param = new GenericParameter(param.name, GenericParamAttributes.Covariant, constraints);
            
            ret.Add(param);
        }
        return ret;
	}
}

typedef MethodDecl = {field:FieldDecl, decl:FunctionDecl};

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
    var BEGIN_LOOP = "BEGIN_LOOP_";
    var END_LOOP = "END_LOOP_";
    var END_TRY = "END_TRY_";
}

typedef Closure = {
    var name:String;
    var locals:Map<String, BaseTypeRef>;
    var methods:Map<String,FunctionDecl>;

    var clrTypeRef:BaseTypeRef;
}
