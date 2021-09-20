package haxe.compiler.backends.pe;

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
import cs.system.reflection.SignatureGenericParameterType;
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
    var printer = new hscript.Printer();
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
        gen.BeginAssemblyRef("mscorlib", new AssemblyName("mscorlib"), peapi.AssemAttr.Retargetable);
        gen.EndAssemblyRef();
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

                    gen.BeginTypeDef(cast flags, c.name.split('.').pop(), null, null, type.location(), null);
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

        var ret = types.checker.check(f.expr, if (f.ret != null) WithType(types.toTType(f.ret)) else WithType(TVoid));
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
        generateLocals();
        if (retType.FullName == "System.Void") {
            noneInstr(peapi.Op.ret, f.expr.location());
        }
        if (closure != null)
            beforeNextType.push(() -> defineClosure(closure));
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
        var type = if (v.expr != null) types.checker.check(v.expr, WithType(types.toTType(v.type))) else types.toTType(v.type);
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
    function mapToClrMethodBody(expr:hscript.Expr, method:MethodDef, ret:TType, ?withType:TType, ?_after:Void->Void) {
        inline function doNext(?done)
            if (next != null) {
                next();
                if (done)
                    next = null;
            }
        inline function after()
            if (_after != null)
                _after();
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
                    var type = types.checker.check(e, if (t != null) WithType(types.toTType(t)) else null);
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
                    var catchType = types.toTType(t);
                    declareLocal(v, catchType);
                    firstPass(e);
                    firstPass(ecatch);
                case EFunction(args, e, name, ret):
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
                    var type = types.checker.check(e, if (t != null) WithType(types.toTType(t)) else null);
                    mapToClrMethodBody(e, method, ret, type, () -> setVar(n, toClrTypeRef(types.toTType(t)), e.location()));
                    doNext();
                case EBlock(e):
                    var from = gen.CurrentMethodDef.AddLabel();
                    gen.CurrentMethodDef.BeginLocalsScope();
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
                        // well, until we have abstracts this is impossible anywho
                        default: /* throw '${printer.exprToString(e)} is not callable (${e.location()})'; */
                    }
                    // i think potentially withType below should be the ret from TFun above..
                    handleCall(e, callerType, params, paramTypes, withType);
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
                case EFunction(args, e, name, ret):
                    if (closure == null) {
                        closure = mkClosure();
                        closure.name = '${COMPILER_GENERATED_PREFIX}${gen.CurrentTypeDef.Name}_${gen.CurrentMethodDef.Name}_hx_ClosureState';
                    }

                    closure.methods.push({
                        name: name,
                        f: {
                            ret: ret,
                            args: args,
                            expr: e
                        }
                    });
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
                    var catchType = toClrTypeRef(types.toTType(t));
                    var catchType = toClrTypeRef(types.toTType(t));
                    mapToClrMethodBody(e, method, ret, withType, () -> brTargetIdInstr(BranchOp.leave_s, referenceLabel(LabelRefs.END_TRY), e.location()));
                    
                    var tryBlock = new TryBlock(handlerBlock, e.location());
                    mapToClrMethodBody(ecatch, method, ret, withType, _after);
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
                    mapToClrMethodBody(e, method, ret, types.toTType(t), _after);
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
                throw 'invalid unary operator: $op (near ${new Printer().exprToString(e)})';
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
            if (prefix) {
                noneInstr(peapi.Op.ldc_i4_1, e.location());
                noneInstr(opCode, e.location());
                setVar(ident, toClrTypeRef(types.checker.check(e)), e.location());
                handleIdent(ident, e.location());
            }
        }
        function post() {
            if (!prefix) {
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
        }
        switch type {
            case TInt | TFloat: // lets only handle numbers for now...
                switch op {
                    case '++': opCode = peapi.Op.add;
                    case '--': opCode = peapi.Op.sub;
                }
            default:
        }
        pre();
        next = post;
    }

    function handleBinop(op:String, arg1:Array<Expr>, arg2:Array<TType>) {}

    function handleCall(e:Expr, callerType:TType, params:Array<Expr>, paramTypes:Array<TType>, retType) {
        for (param in params) {
            mapToClrMethodBody(param, gen.CurrentMethodDef, null);
        }
        switch e.expr() {
            case EIdent('trace'):
                // just a quick hack to make trace do console log with one arg...
                // don't blame me I want to test FFS
                var type = types.checker.check(params[0]);
                trace(type);
                var argType = toClrTypeRef(type);
                trace(argType);
                var methodRef = gen.ExternTable.GetTypeRef("mscorlib", "System.Console", false)
                    .GetMethodRef(Primitives.VOID, CallConv.Default, "WriteLine", NativeArray.make((argType : BaseTypeRef)), 0);
                callInstr(CallConv.Default, methodRef, e.location());
                trace('?w0t');
            default:
                trace(e);
        }
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
            var type = toClrTypeRef(types.checker.check(EIdent(e).mk({
                pmin: 0,
                pmax: 0,
                e: null,
                origin: null,
                line: 0
            })));
            fieldInstr(FieldOp.ldfld, type, owner, e, location);
        }
    }

    function afterType() {
        for (cb in beforeNextType)
            cb();
    }

    function defineClosure(closure:Closure) {}

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
        throw new haxe.exceptions.NotImplementedException();
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
        trace(new Printer().exprToString(constructor.decl.expr));
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

    function init() {
        types.addType(DClass({
            name: "System.String",
            params: [],
            fields: [],
            meta: [
                {
                    name: "netLib",
                    params: [
                        EConst(CString("mscorlib")).mk({
                            pmin: 0,
                            pmax: 0,
                            origin: null,
                            line: 0,
                            e: null
                        })
                    ]
                }
            ],
            isExtern: true,
            isPrivate: false,
            extend: null,
            implement: null
        }));
    }

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
}

typedef MethodDecl = {field:FieldDecl, decl:FunctionDecl};

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
    var BEGIN_LOOP = "BEGIN_LOOP_";
    var END_LOOP = "END_LOOP_";
    var END_TRY = "END_TRY_";
}

interface Closure {
    var name:String;
    var locals:Map<String, BaseTypeRef>;
    var methods:Array<{name:String, f:FunctionDecl}>;
}
