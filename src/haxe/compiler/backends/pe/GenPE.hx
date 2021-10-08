package haxe.compiler.backends.pe;

import mono.ilasm.IInstr;
import mono.ilasm.TypeSpecMethodRef;
import mono.ilasm.MethodPointerTypeRef;
import mono.ilasm.ParamDef;
import peapi.GenericParamAttributes;
import mono.ilasm.GenericParameter;
import mono.ilasm.GenericParameters;
import haxe.compiler.backends.Core.Gen;
import cs.system.reflection.AssemblyName;
import mono.ilasm.CatchBlock;
import mono.ilasm.TryBlock;
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
import peapi.IntOp;
import mono.ilasm.IntInstr;
import mono.ilasm.SimpInstr;
import mono.ilasm.BaseClassRef;
import mono.ilasm.GenericArguments;
import haxe.exceptions.NotImplementedException;

using haxe.compiler.backends.pe.Tools;

import haxe.compiler.backends.pe.Macros.beforeNextInstructionSet;
import haxe.compiler.backends.pe.Macros.afterNextInstructionSet;

using hscript.Tools;

// import haxe.compiler.backends.pe.Macros.bump;
import hscript.Interp;
import haxe.io.Bytes;
import mono.ilasm.LdstrInstr;
import mono.ilasm.MethodInstr;

using hscript.Tools;

import mono.ilasm.BaseTypeRef;
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
import mono.ilasm.CodeGen;

using Lambda;
using StringTools;
using hscript.Checker;
using haxe.compiler.backends.pe.GenPE;

typedef Deferred = Array<Void->Void>;

class GenPE extends Gen {
    static var TYPE_ONLY_SLOT = -2;
    static var EMPTY_LOC = new Location(0, 0);

    public var gen:CodeGen;
    public var peTypes:PECheckerTypes;

    static var COMPILER_GENERATED_PREFIX = "<>_";

    var localMeta:Map<String, LocalMetadata> = [];

    var handlerBlock:HandlerBlock;
    var locals:Map<String, BaseTypeRef> = [];
    var instructions:Array<Instruction> = [];
    var localTypes:Map<String, TType> = [];
    var inClosure = false;
    var localSlots:Map<String, Int> = [];
    var beforeNextType:Array<Void->Void> = [];
    var closure:Closure;
    var functionPointerTypes:Map<String, BaseTypeRef> = [];
    var beforeBody:Array<Expr> = [];
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
    var lastExprWasRet = false;

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
            types.declareType(decl);
        firstPassComplete = true;
    }

    var currentTypePath:String;
    var currentTypeName:String;
    var superClass:TType;

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
                    var name = currentTypePath = currentPackage + '.${c.name}';
                    currentTypeName = c.name;
                    var flags = 0;
                    var isStruct = c.meta.exists(m -> m.name == ":struct");
                    // TypeAttr.
                    flags |= cast TypeAttr.BeforeFieldInit;
                    if (c.isPrivate)
                        flags = flags | cast TypeAttr.Private;
                    else
                        flags = flags | cast TypeAttr.Public;
                    var superCl = switch c.extend {
                        case CTPath(path, params): '${path.join('.')}';
                        default: null;
                    }
                    if (superCl != null && isStruct)
                        throw 'Cannot extend struct';
                    if (isStruct) {
                        flags |= cast TypeAttr.SequentialLayout;
                        flags |= cast TypeAttr.Sealed;
                    }
                    var superType = if (isStruct) getValueType() else if (superCl != null) getTypeRef(superCl); else getBaseType();
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
                    gen.EndTypeDef();
                    afterType();
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
        trace('generating method');
        var flags = getClrFlags(field, [cast MethAttr.Public, cast MethAttr.Static, cast MethAttr.Private]);
        flags |= cast MethAttr.HideBySig;
        if (isSpecialName(field.name)) {
            flags |= cast MethAttr.SpecialName;
            flags |= cast MethAttr.RTSpecialName;
        }

        var conv:Int = cast peapi.CallConv.Default;
        var isInstanceMethod = !field.access.contains(AStatic);
        if (isInstanceMethod) {
            conv |= cast CallConv.Instance;
        }
        if (false) { // check if f has params...
            conv |= cast CallConv.Generic;
        }

        var implAttr = ImplAttr.IL;
        trace('flags?');
        var ret = types.checker.check(f.expr, if (f.ret != null) WithType(types.checker.makeType(f.ret, f.expr)) else WithType(TVoid));
        trace('ret?');
        var retType = toClrTypeRef(ret);
        trace('ret type got');
        var ownerType = gen.CurrentTypeDef; // lookupType(owner).toClrTypeDef(this);
        var paramList = new cs.system.collections.ArrayList();
        for (arg in f.args) {
            var argType = if (arg.value == null) types.checker.makeType(arg.t,
                arg.value) else types.checker.check(arg.value, WithType(types.checker.makeType(arg.t, arg.value)));
            var clrType = toClrTypeRef(argType);
            var paramDef = new ParamDef(peapi.ParamAttr.Default, arg.name, clrType);
            paramList.Add(paramDef);
        }
        var genericParameters = getGenericParameters(f);
        labelRefCount = [];
        var expr = beforeMethod(f, field.name, false, isInstanceMethod);
        var method = new MethodDef(gen, cast flags, cast conv, implAttr, field.name, retType, paramList, f.expr.location(), genericParameters, ownerType);

        if (field.name == "main" && owner == mainClass)
            method.EntryPoint();
        method.SetMaxStack(8); // apparently this only matters for PE verification and has nothing to do with runtime
        // so, unless this value is intelligently set, there's a chance the PE won't be verifiable
        // BUT by default .NET allows unverifiable PEs so.... is it worth the bother? Not right now
        if (beforeBody.length != 0) {
            var b = beforeBody;
            beforeBody = [];
            generate(EBlock(b).mk(null));
        }
        generate(expr);
        endMethod(ret, expr.location());
        closure = null;
        generateLocals();

        gen.EndMethodDef(f.expr.location());
    }

    function beforeMethod(decl:FunctionDecl, methodName:String, closure = false, isInstanceMethod = false) {
        if (methodName == 'new' || methodName == '.ctor') {
            beforeNextInstructionSet({
                callBaseConstructor();
            });
        }
        argNames = [for (arg in decl.args) arg.name];
        if (isInstanceMethod) {
            locals.set('this', getTypeRef(getCurrentNestedName()));
            localSlots.set('this', TYPE_ONLY_SLOT);
            argNames.unshift('this');
        }
        return if (decl.expr.expr().match(EBlock(_))) {
            upcomingMethodName = methodName;
            var newBody = analyze(decl.expr);
            setLocalSlots();
            if (!closure) {
                trace('defining closure for $methodName');
                addClosureToChecker();
                defineClosure();
            }
            newBody;
        } else decl.expr;
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

    function toClrTypeRef(t:TType, resolveRawFuncPtr = false):BaseTypeRef {
        // trace(t);
        return // this of course can only be called after the first pass
            if (!firstPassComplete) throw 'First pass incomplete; cannot convert types'; else switch types.checker.follow(t) {
                case TUnresolved(name):
                    trace('type ref $name expected');
                    var ret = getTypeRef(name);
                    ret;
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
                case TFun(args, ret):
                    args = [for (arg in args) {name: arg.name, t: types.checker.follow(arg.t), opt: arg.opt}];
                    ret = types.checker.follow(ret);

                    if (!resolveRawFuncPtr) getFunctionTypeRef(args, ret); // These need to capture generics?
                    else getFunctionPointerTypeRef(args, ret);
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

    var varName:String;

    function analyze(e:Expr) {
        var ret = e;
        // trace(e.expr());
        switch e.expr() {
            case EIf(cond, e1, e2):
                analyze(cond);
                analyze(e1);
                if (e2 != null)
                    analyze(e2);

            case EBlock(e):
                ret = EBlock([
                    for (expr in e)
                        analyze(expr)
                ]).mk(null);
            case EVar(n, t, e):
                var type = types.checker.check(e, if (t != null) WithType(types.checker.makeType(t, e)) else null);
                declareLocal(n, type);
                varName = n;
                ret = EVar(n, t, analyze(e)).mk(null);
                varName = null;
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
                ret = EFor(v, it, analyze(e)).mk(null);
            case ETry(e, v, t, ecatch):
                var catchType = types.checker.makeType(t, ecatch);

                ret = ETry(analyze(e), v, t, analyze(ecatch)).mk(null);
                declareLocal(v, catchType);
            case EFunction(kind, decl):
                defineFuncPtrType(decl);
                var n = addToClosure(kind, decl);
                kind = FNamed(n, false);
                ret = EFunction(kind, decl).mk(null);
                var fnName = if (!kind.match(FNamed(_)) && varName != null) varName else n;
                varName = null;
                declareLocal(fnName, TFun([
                    for (arg in decl.args)
                        {name: arg.name, t: types.checker.makeType(arg.t), opt: arg.opt}
                ], types.checker.makeType(decl.ret)));
                // moveToClosure(n);
                decl.expr.iter(e -> switch e.expr() {
                    case EIdent(v):
                        if (isLocal(v)) {
                            moveToClosure(v);
                        }
                    default: analyze(e);
                });
                decl.expr = analyze(decl.expr);
                ret = EFunction(kind, decl).mk(null);
            default:
        }
        return ret;
    }

    var beforeNext:Array<Instruction> = [];
    var afterNext:Array<Instruction> = [];

    function prepVar(n:String, location:Location) {
        if (closure != null && closure.locals.exists(n)) {
            handleIdent(closure.localName, location);
        }
    }

    // da meat
    function generate(?expr:hscript.Expr, ?withType:TType, ?topLevelExpr = true) {
        inline function prepVar(n:String, location:Location) {
            if (!topLevelExpr && closure != null && closure.locals.exists(n)) {
                handleIdent(closure.localName, location);
            }
        }
        if (expr == null)
            return; // noop
        trace('mapping ${printer.exprToString(expr)}');

        inline function runDeferred(?done)
            if (next != null) {
                next();
                if (done)
                    next = null;
            }
        inline function doIf(e:Expr, cond:Expr, e1:Expr, e2:Null<Expr>) {
            // var branchEnd:LabelInfo = null;
            beginSet();
            var tail = getSetTail();
            generate(cond, withType, false);
            brTargetIdInstr(BranchOp.brfalse_s, referenceLabel(LabelRefs.END_COND), cond.location());
            runDeferred();
            generate(e1, withType, false);
            runSet(tail);
            brTargetIdInstr(BranchOp.br_s, referenceLabel(LabelRefs.END_IF), e1.location());
            noneInstr(peapi.Op.nop, e1.location());
            putInstr(() -> {
                var endOfConditionLabel = placeLabelRef(LabelRefs.END_COND);
                if (e2 != null) {
                    putInstr(() -> generate(e2, withType, false));
                    runSet(tail);
                }
                noneInstr(peapi.Op.nop, e.location());
                var endIfLabel = placeLabelRef(LabelRefs.END_IF);
            });
        }

        var with = if (withType != null) WithType(withType) else null;
        // make sure we move locals to closure as necessary, also define locals up front

        lastExprWasRet = false;
        var e = expr;
        switch e.expr() {
            case EField(e, f):
                // generate(e, withType, after, false,)

                if (!topLevelExpr) {
                    beginSet();
                    handleField(e, f);
                    endSet();
                }

            case EConst(c):
                switch c {
                    case CInt(v):
                        intInt32Instr(IntOp.ldc_i4_s, v, e.location());
                    case CFloat(f):
                        r8Float64Instr(MiscInstr.ldc_r8, f, e.location());
                    case CString(s):
                        loadStringInstr(s, e.location());
                }
            case EVar(n, t, e): // should support an array... e.g. var a,b,c;
                // technically after should be null at this point.. not sure if should throw

                types.checker.allowGlobalsDefine = true;
                var type = types.checker.check(e, if (t != null) WithType(types.checker.makeType(t, expr)) else null);
                trace('were saying type of ${printer.exprToString(e)} is ${type.typeStr()}');

                expectedPtrSigType = type;
                var loc = e.location();
                beginSet();
                topLevelExpr = false;
                prepVar(n, loc);
                afterNextInstructionSet(setVar(n, toClrTypeRef(if (t != null) types.checker.makeType(t, expr) else type), loc, getLocalMetadata(e)));
                generate(e, type, false);
                endSet();
                expectedPtrSigType = null;
                runDeferred();
            // trace('setting it back from ${expectedPtrSigType.typeStr()} to ${previousexpectedPtrSigType == null ? null : previousexpectedPtrSigType.typeStr()}');
            case EBlock(e):
                var from = gen.CurrentMethodDef.AddLabel();
                gen.CurrentMethodDef.BeginLocalsScope();
                beginSet();
                var setTail = getSetTail();
                for (expr in e)
                    generate(expr, withType, true);
                runSet(setTail);
                handlerBlock = new HandlerBlock(from, gen.CurrentMethodDef.AddLabel());
                gen.CurrentMethodDef.EndLocalsScope();

            case EReturn(e):
                beginSet();
                generate(e, withType, false);
                noneInstr(peapi.Op.ret, e.location());
                lastExprWasRet = true;
                endSet();
            case EIdent(i):
                if (!topLevelExpr) {
                    beginSet();
                    switch i {
                        case 'null':
                            noneInstr(peapi.Op.ldnull, e.location());
                        case 'true': // this should be EConst.CBool(v)...
                            noneInstr(peapi.Op.ldc_i4_1, e.location());
                        case 'false': // this should be EConst.CBool(v)...
                            noneInstr(peapi.Op.ldc_i4_0, e.location());
                        case i:
                            trace(locals);
                            handleIdent(i, e.location());
                    }
                    endSet();
                }
            case EUnop(op, prefix, e):
                var type = types.checker.check(e, with);
                handleUnop(op, prefix, e, type);
            case EBinop(op, e1, e2):
                var type1 = types.checker.check(e1, with);
                var type2 = types.checker.check(e2, WithType(type1));
                var previousType = expectedPtrSigType;
                if (e2.expr().match(EField(_, _))) {
                    isRhsOfOp = true;
                    expectedPtrSigType = type2;
                }
                beginSet();
                handleBinop(op, [e1, e2], [type1, type2]);
                endSet();
                runDeferred();
            case ECall(e, params):
                var funcType = types.checker.check(e, with);
                trace(e);

                // i think potentially withType below should be the ret from TFun above..
                beginSet();
                handleCall(e, funcType, params, [for (param in params) types.checker.check(param)], withType);
                endSet();
            case EIf(cond, e1, e2):
                doIf(e, cond, e1, e2);
            case EWhile(cond, e):
                var beginLoop = placeLabelRef(LabelRefs.BEGIN_LOOP);
                beginSet();
                var tail = getSetTail();
                generate(cond, withType, false);
                brTargetIdInstr(BranchOp.brfalse_s, referenceLabel(LabelRefs.END_LOOP), cond.location());
                generate(e, withType, false);
                brTargetIdInstr(BranchOp.br_s, beginLoop.Name, e.location());
                noneInstr(peapi.Op.nop, e.location());
                runSet(tail);
                placeLabelRef(LabelRefs.END_LOOP);

            case EFor(v, it, e):
                // handle int in 0...n with a more optimized approach than obj in iterable
                var loopVar = v;

                switch it.expr() {
                    case EBinop('...', min, max) if (types.checker.check(min).match(TInt) && types.checker.check(max).match(TInt)):
                        // TODO: first pass over method body for closures
                        // intInt32Instr(IntOp.ldc_i4, min, it.location());

                        var loc = e.location();
                        var tail = getSetTail();
                        prepVar(loopVar, loc);
                        generate(min, false);
                        setVar(loopVar, Primitives.INT, loc, []);
                        var beginLoop = placeLabelRef(LabelRefs.BEGIN_LOOP);
                        beginSet();
                        generate(max, false);
                        handleIdent(loopVar, loc);
                        brTargetIdInstr(BranchOp.beq, referenceLabel(LabelRefs.END_LOOP), it.location());
                        generate(e, withType, false);
                        prepVar(loopVar, loc);
                        noneInstr(peapi.Op.ldc_i4_1, loc);
                        handleIdent(loopVar, loc);
                        noneInstr(peapi.Op.add, loc);
                        setVar(loopVar, Primitives.INT, loc, []);
                        brTargetIdInstr(BranchOp.br_s, beginLoop.Name, loc);
                        runSet(tail);
                        placeLabelRef(LabelRefs.END_LOOP);
                    default:
                        // TODO: crap... forgot the checker needs locals loaded to it...
                        // TODO: crap, forgot again
                }
            case EFunction(_.getFunctionName() => closureMethod, decl):
                // declareLocal(closure.localName, types.resolve(closure.name));
                var args = [
                    for (arg in decl.args)
                        {
                            name: '',
                            opt: false,
                            t: types.checker.follow(types.checker.makeType(arg.t))
                        }
                ];
                var ret = types.checker.follow(types.checker.makeType(decl.ret));
                expectedPtrSigType = TFun(args, ret);
                var newFuncPtrExpr = ENew(getNestedFullName(getFunctionTypeRef(args, ret)), [EField(closure.local, closureMethod).mk(e)]).mk(e);
                beginSet();
                var tail = getSetTail();
                generate(newFuncPtrExpr, withType, topLevelExpr);
                runSet(tail);
            // <closureLocal>.<closureMethod>;

            case ENew(cl, params):
                if (!topLevelExpr) {
                    var type = types.resolve(cl);
                    trace('resolved type: ${type.typeStr()}');
                    var clrType = toClrTypeRef(type);
                    trace('clr type: ${clrType.FullName}');

                    for (param in params)
                        generate(param, null, false);
                    var method = clrType.GetMethodRef(Primitives.VOID, CallConv.Instance, ".ctor", cs.Lib.nativeArray([
                        for (param in params) {
                            var ret = toClrTypeRef(types.checker.check(param), true);
                            trace('param type? ${ret.FullName}');
                            ret;
                        }
                    ], true), 0);
                    var tail = getSetTail();
                    beginSet();
                    methodInstr(MethodOp.newobj, method, e.location());
                    runSet(tail);
                }
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
            // if it's not an abstract, nothing fancy here
            // otherwise bang head on keyboard
            case EThrow(e):
                var exception:Expr = convertToException(e);
                beginSet();
                generate(exception, false);
                noneInstr(peapi.Op.throwOp, e.location());
            case ETry(e, v, t, ecatch): // TODO: hscript: catch should be an array....
                // similar semantics as try/catch in c#
                putInstr(() -> {
                    if (!e.expr().match(EBlock(_))) {
                        e.e = EBlock([e]);
                    }
                    if (!ecatch.expr().match(EBlock(_))) {
                        ecatch.e = EBlock([e]);
                    }
                    var catchType = toClrTypeRef(types.checker.makeType(t, expr));
                    beginSet();
                    afterNextInstructionSet(brTargetIdInstr(BranchOp.leave_s, referenceLabel(LabelRefs.END_TRY), e.location()));
                    generate(e, withType, true);
                    var catchLoc = ecatch.location();
                    putInstr(() -> {
                        tryBlock = new TryBlock(handlerBlock, e.location());
                    });

                    beforeNextInstructionSet({
                        prepVar(v, catchLoc);
                        setVar(v, catchType, catchLoc, getLocalMetadata(ecatch));
                    });
                    afterNextInstructionSet(brTargetIdInstr(BranchOp.leave_s, referenceLabel(LabelRefs.END_TRY), e.location()));
                    generate(ecatch, withType, true);
                    putInstr(() -> {
                        catchBlock = new CatchBlock(catchType);
                        catchBlock.SetHandlerBlock(handlerBlock);
                        tryBlock.AddSehClause(catchBlock);
                        putInstr(tryBlock);
                        trace('do block');
                        tryBlock = null;
                        catchBlock = null;
                    });
                    endSet();
                    putInstr(() -> placeLabelRef(LabelRefs.END_TRY));
                });
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
                putInstr(() -> {
                    var beginLoop = placeLabelRef(LabelRefs.BEGIN_LOOP);
                    beginSet();
                    var tail = getSetTail();
                    generate(e, withType, false);
                    generate(cond, withType, false);
                    brTargetIdInstr(BranchOp.brfalse_s, referenceLabel(LabelRefs.END_LOOP), cond.location());
                    brTargetIdInstr(BranchOp.br_s, beginLoop.Name, e.location());
                    noneInstr(peapi.Op.nop, e.location());
                    runSet(tail);
                    placeLabelRef(LabelRefs.END_LOOP);
                });
            case EMeta(name, args, e):
                meta = {name: name, args: args, expr: e};
                generate(e, withType, false);
            case ECheckType(e, t): // or not
                generate(e, types.checker.makeType(t, expr), false);
            default:
        }
    }

    // begin shameless ripping from ILParser.jay
    inline function noneInstr(opcode:peapi.Op, loc:Location)
        putInstr(new SimpInstr(opcode, loc));

    inline function localIntInstr(opcode:IntOp, int:Int, loc:Location)
        putInstr(new IntInstr(opcode, int, loc));

    inline function localIdInstr(opcode:IntOp, id:String, loc:Location) {
        var slot = localSlots[id];
        trace('slot for $id is $slot');
        if (slot < 0)
            throw 'Undeclared identifier $id';
        putInstr(new IntInstr(opcode, slot, loc));
    }

    inline function paramInt32Instr(opcode:IntOp, int:Int, loc:Location)
        return localIntInstr(opcode, int, loc);

    inline function paramIdInstr(opcode:IntOp, id:String, loc:Location) {
        var pos = gen.CurrentMethodDef.GetNamedParamPos(id);
        if (pos < 0)
            throw 'Undeclared identifier $id';
        putInstr(new IntInstr(opcode, pos, loc));
    }

    inline function intInt32Instr(opcode:IntOp, int:Int, loc:Location)
        return localIntInstr(opcode, int, loc);

    inline function intIdInstr(opcode:IntOp, id:String, loc:Location)
        return localIdInstr(opcode, id, loc);

    inline function r8Int64Instr(instr:MiscInstr, long:Int64, loc:Location)
        switch instr {
            case MiscInstr.ldc_i8:
                putInstr(new LdcInstr(instr, long, loc));
            default:
        }

    inline function r8Float64Instr(instr:MiscInstr, double:Float, loc:Location)
        switch instr {
            case MiscInstr.ldc_r4 | MiscInstr.ldc_r8:
                putInstr(new LdcInstr(instr, double, loc));
            default:
        }

    inline function brTargetInt32Instr(branchOp:BranchOp, int:Int, loc:Location) {
        var target = gen.CurrentMethodDef.AddLabel(int);
        putInstr(new BranchInstr(branchOp, target, loc));
    }

    inline function brTargetIdInstr(branchOp:BranchOp, id:String, loc:Location) {
        var target = gen.CurrentMethodDef.AddLabelRef(id);
        putInstr(new BranchInstr(branchOp, target, loc));
    }

    inline function runSet(set:Array<Instruction>)
        while (set.length != 0)
            putInstr(set.shift());

    inline function getSetHead()
        return beforeNext.splice(0, beforeNext.length);

    inline function getSetTail()
        return afterNext.splice(0, afterNext.length);

    inline function beginSet()
        while (beforeNext.length != 0)
            putInstr(beforeNext.shift());

    inline function endSet()
        while (afterNext.length != 0)
            putInstr(afterNext.shift());

    inline function putInstr(instr:Instruction) {
        if (!writingBody)
            instructions.push(instr)
        else
            commitInstruction(instr);
    }

    inline function methodInstr(methodOp:MethodOp, methRef:BaseMethodRef, loc:Location)
        putInstr(new MethodInstr(methodOp, methRef, loc));

    inline function fieldInstr(fieldOp:FieldOp, type:BaseTypeRef, owner:BaseTypeRef, name:String, loc:Location) {
        // not sure if this is right
        var gpr = Std.downcast(type, GenericParamRef);
        if (gpr != null && gen.CurrentMethodDef != null)
            gen.CurrentMethodDef.ResolveGenParam(cast gpr.PeapiType);
        var fieldref = owner.GetFieldRef(type, name);
        putInstr(new FieldInstr(fieldOp, fieldref, loc));
    }

    inline function basicFieldInstr(fieldOp:FieldOp, type:BaseTypeRef, name:String, loc:Location) {
        var fieldRef = gen.GetGlobalFieldRef(type, name);
        putInstr(new FieldInstr(fieldOp, fieldRef, loc));
    }

    inline function typeInstr(typeOp:TypeOp, type:BaseTypeRef, loc:Location)
        putInstr(new TypeInstr(typeOp, type, loc));

    inline function loadStringInstr(string:String, loc:Location)
        putInstr(new LdstrInstr(string, loc));

    inline function callInstr(callingConvention:CallConv, methodRef:BaseMethodRef, loc:Location) {
        putInstr(new MethodInstr(MethodOp.call, methodRef, loc));
    }

    inline function tokenInstr(instr:MiscInstr, tokenType:Either<IFieldRef, Either<BaseMethodRef, BaseTypeRef>>, loc:Location)
        putInstr(switch tokenType {
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
        putInstr(new SwitchInstr({
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
                generate(e, false);
                noneInstr(peapi.Op.ldc_i4_0, e.location());
                noneInstr(peapi.Op.ceq, e.location());
            case '~' if (prefix):
                generate(e, false);
                noneInstr(peapi.Op.not, e.location());
            default:
                throw 'invalid unary operator: $op (near ${printer.exprToString(e)})';
        }
    }

    function handleIncOrDec(op:String, prefix:Bool, e:Expr, type:TType) {
        var ident = '';
        var opCode:peapi.Op = peapi.Op.nop;
        inline function pre() {
            prepVar(ident, e.location());
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
            prepVar(ident, e.location());
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

        var ftnPtr = false;
        // trace('caller type: ${printer.exprToString(e)} ${callerType.typeStr()}');
        var callerType:TType = null;
        var callerExpr:Expr = null;
        var isInstanceMethod = true;
        var implicitThisCall = false;
        var isStruct = false;
        var callerClrType = switch e.expr() {
            case EField(e, f): // method call
                isStruct = switch e.expr() {
                    case EIdent(v) if (locals.exists(v)):
                        var type = localTypes[v];
                        peTypes.isStruct(type);
                    default: false;
                }
                callerExpr = e;
                expectedPtrSigType = TFun([for (type in paramTypes) {name: '', opt: false, t: type}], if (retType == null) TVoid else retType);
                methodName = f;
                callerType = types.checker.check(e);
                isInstanceMethod = !types.checker.isFieldStatic(callerType, methodName, paramTypes, retType);
                toClrTypeRef(callerType);
            case EIdent(v): // closure
                // I think this will make delegates callable...
                //  https://github.com/HaxeFoundation/haxe/blob/6eb36b2aa38591203005ea30f8334e41de292111/src/codegen/dotnet.ml#L1196
                if (peTypes.delegateTypes.exists(type -> types.checker.tryUnify(funcType, type))) {
                    trace('${printer.exprToString(e)} ${funcType.typeStr()} unifies with ${peTypes.delegateTypes.find(type -> types.checker.tryUnify(funcType, type)).typeStr()}');
                    handleCall(EField(e, 'Invoke').mk(null), funcType, params, paramTypes, retType);
                    return;
                } else if (!inClosure) { // I think?
                    trace('${printer.exprToString(e)} ${funcType.typeStr()}');
                    handleCall(EField(e, 'invoke').mk(null), funcType, params, paramTypes, retType);
                    return;
                }
                var meta = localMeta[v];
                isInstanceMethod = inClosure || (meta != null && meta.indexOf(LMInstance) != -1);
                implicitThisCall = true;
                ftnPtr = !inClosure && types.checker.check(e).match(TFun(_, _));
                methodName = v;
                closure.clrTypeRef();
            case EFunction(k, f): // IIFE
                ftnPtr = true;
                methodName = k.getFunctionName();
                closure.clrTypeRef();
            default: throw 'invalid call expression: ${printer.exprToString(e)]}';
        }
        if (ftnPtr)
            trace('${printer.exprToString(e)} is a function pointer');

        var funcArgs = switch funcType {
            case TFun(args, ret):
                args;
            default: throw 'invalid funcType: ${funcType.typeStr()}';
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

        var genericParamCount = getGenericParamCount(funcArgs);

        // yes... all of that just to set up state for generic parameters (and also handle optional params)
        var genericArguments = if (genericParamCount != 0) new GenericArguments() else null;
        if (genericArguments != null)
            for (i in 0...genericParamMap.list().length) {
                genericArguments.Add(genericParamMap[i]);
            }

        var callConv = if (isInstanceMethod) CallConv.Instance else CallConv.Default;

        var retClrType = toClrTypeRef(retType);
        var clrParamTypes = cs.Lib.nativeArray([for (funcArg in funcArgs) toClrTypeRef(funcArg.t)], true);
        var methRef = callerClrType.GetMethodRef(retClrType, callConv, methodName, clrParamTypes, genericParamCount);
        if (genericArguments != null)
            methRef.GetGenericMethodRef(genericArguments);
        // and finally... do the actual mapping to CLR body
        // If it's not static, put the instance on the stack
        if (isInstanceMethod) {
            trace('putting instance expr: ${printer.exprToString(callerExpr)}');
            generate(callerExpr, false);
        }
        if (implicitThisCall) {
            intInt32Instr(IntOp.ldarg_s, 0, e.location());
        }
        if (ftnPtr)
            generate(e, false);
        var methodOp = if (isInstanceMethod && !isStruct) {
            MethodOp.callvirt;
        } else MethodOp.call;
        // put the parameters on the stack
        for (i in 0...params.length) {
            var param = params[i];
            var paramType = paramTypes[i];
            var arg = funcArgs[i];
            var argType = arg.t;
            params[i] = doConversion(param, paramType, argType);
            generate(params[i], argType, false);
        }

        expectedPtrSigType = null;

        methodInstr(methodOp, methRef, e.location());
    }

    var argNames:Array<String> = [];

    // so, to reference a field in CLR, you need to know if
    // its a local or a field (there's different instructions for
    // accessing locals and fields)
    // also, any locals that get captured/referenced inside a closure have to be
    // moved into the closure as member fields.
    function handleIdent(id:String, location:Location, ?pos:haxe.PosInfos) {
        inline function closureField()
            generate(EField(closure.local, id).mk(null), false);
        trace('getting $id @$pos $argNames $inClosure');
        var argIndex = argNames.indexOf(id);
        trace(locals);

        if (argIndex != -1)
            intInt32Instr(IntOp.ldarg_s, argIndex, location);
        else if (inClosure)
            if (id == '$$closure')
                intInt32Instr(IntOp.ldarg_s, 0, location);
            else
                closureField();
        else if (locals.exists(id)) {
            localIdInstr(IntOp.ldloc_s, id, location);
        } else if (closure != null && closure.locals.exists(id)) {
            // if(inClosure) {
            //     handleField(EIdent('$$closure').mk(null), id);
            // }
            // else
            closureField();
        }
    }

    var isRhsOfOp = false;
    var expectedPtrSigType:Null<TType>;

    function handleField(e:Expr, f:String) {
        // var expectedPtrSigType = expectedPtrSigType;
        // generate(e); d

        var type = types.checker.check(e);

        var clrType = toClrTypeRef(type);
        var ret:TType = null;
        var args = switch expectedPtrSigType {
            case TFun(args, r):
                ret = r;
                args;
            default: null;
        };
        trace('1 $args');
        var argTypes = if (args == null) null else [for (arg in args) arg.t];
        trace('2 $expectedPtrSigType $f $e $isRhsOfOp $argTypes $ret');
        var fieldType = toClrTypeRef(types.checker.getField(type, f, e, isRhsOfOp, argTypes, ret));
        trace('3 ${fieldType.FullName}');
        generate(e, false); // map to clr method body..
        if (args != null) { // assigning to method pointer..
            trace('getting $f from ${type.typeStr()} with args ${[for (arg in args) arg.t.typeStr()]} and ret ${ret.typeStr()}');
            var isInstanceField = !types.checker.isFieldStatic(type, f, argTypes, ret);
            trace('clrType: $clrType');
            var methodRef = clrType.GetMethodRef(toClrTypeRef(ret), if (isInstanceField) CallConv.Instance else CallConv.Default, f,
                cs.Lib.nativeArray([for (argType in argTypes) toClrTypeRef(argType)], true), getGenericParamCount(args));
            
            methodInstr(if(isInstanceField) MethodOp.ldvirtfn else MethodOp.ldftn, methodRef, e.location());
        } else {
            trace('args of ${printer.exprToString(e)}.$f is null');
            var fieldOp = if (isRhsOfOp) FieldOp.stsfld else FieldOp.ldfld;
            fieldInstr(fieldOp, fieldType, clrType, f, e.location());
        }
        isRhsOfOp = false;
    }

    function afterType() {
        for (cb in beforeNextType)
            cb();
        beforeNextType = [];
    }

    function defineClosure() {
        var td = gen.CurrentTypeDef;
        trace('trying to define... ${td.NestedFullName} ${td.Name} ${td.FullName}');
        if (closure == null)
            return;
        var typeName = currentTypePath;
        // currentTypePath = gen.CurrentTypeDef.NestedFullName;
        inClosure = true;
        var externalLocal = closure.local;
        closure.local = EIdent('this').mk(null);
        for (name => type in closure.locals) {
            var attr = FieldAttr.Public;
            var fieldDef = new FieldDef(attr, name, type);
            gen.CurrentTypeDef.AddFieldDef(fieldDef);
        }
        trace('w0t');
        for (name => decl in closure.methods) {
            trace('DECLARING: $name $decl');
            trace(closure.methods.list().length);
            var flags:Int = cast MethAttr.Public;
            flags |= cast MethAttr.HideBySig;
            if (isSpecialName(name)) {
                flags |= cast MethAttr.SpecialName;
                flags |= cast MethAttr.RTSpecialName;
            }

            var conv = peapi.CallConv.Instance;

            var implAttr = ImplAttr.IL;
            var retType = types.checker.makeType(decl.ret);
            var retClrType = toClrTypeRef(retType);
            var paramList = {
                var ret = new ArrayList();
                for (arg in decl.args)
                    ret.Add(new ParamDef(peapi.ParamAttr.Default, arg.name, toClrTypeRef(types.checker.makeType(arg.t))));
                ret;
            };
            var startLocation = EMPTY_LOC;
            var genParams = getGenericParameters(decl);
            var expr = beforeMethod(decl, name, true, true);
            var method = new MethodDef(gen, cast flags, conv, implAttr, name, retClrType, paramList, startLocation, genParams, gen.CurrentTypeDef);
            method.SetMaxStack(8);
            generate(expr);
            endMethod(retType, expr.location());
            gen.EndMethodDef(EMPTY_LOC);
        }
        trace('defined ${gen.CurrentTypeDef.FullName}');
        gen.EndTypeDef();
        currentTypePath = typeName;
        inClosure = false;
        closure.local = externalLocal;
    }

    function setVar(varName:String, type:BaseTypeRef, location:Location, ?lm:LocalMetadata, ?pos:haxe.PosInfos) {
        trace('setting $varName @$pos');
        if (lm != null)
            localMeta[varName] = lm;
        if (locals.exists(varName)) {
            localIdInstr(IntOp.stloc_s, varName, location);
        } else if (closure != null && closure.locals.exists(varName)) {
            fieldInstr(FieldOp.stfld, type, closure.clrTypeRef(), varName, location);
        }
    }

    var upcomingMethodName:String;

    function mkClosure():Closure {
        var name = '${currentTypePath.split('.').toClrPath()}/$$${gen.CurrentTypeDef.Name}_${upcomingMethodName}_HxClosure';
        gen.BeginTypeDef(TypeAttr.NestedPrivate, name.split('/').pop().split('.').pop(), null, null, EMPTY_LOC, null);
        var localName = '$$closure';
        declareLocal(localName, types.resolve(name));
        var newExpr = ENew(name, []).mk(null);
        var varDecl = EVar(localName, CTPath([name]), newExpr).mk(null);
        beforeBody.push(varDecl);
        return {
            name: name,
            methods: [
                ".ctor" => {
                    expr: EBlock([]).mk(null),
                    args: []
                }
            ],
            locals: [],
            localTypes: [],
            clrTypeRef: () -> gen.GetTypeRef(name.split('.').toClrPath()),
            local: EIdent(localName).mk(null),
            localName: localName
        };
    }

    function isLocal(v:String):Bool
        return locals.exists(v);

    function moveToClosure(v:String) {
        var local = locals[v];
        var localType = localTypes[v];
        if (locals.remove(v)) {
            localTypes.remove(v);
            closure.locals.set(v, local);
            closure.localTypes.set(v, localType);
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
                        exprs.unshift(EBinop('=', EField(EIdent('this').mk(null), field.field.Name).mk(c), field.decl.expr).mk(c));
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
        '.cctor',            // CLASS CONSTRUCTOR
        'new',                  // HAXE CONSTRUCTOR
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
        arg0 = arg0.split('.').toClrPath();
        var asm = peTypes.getAssembly(arg0);
        var valueType = isValueType(arg0);

        return if (asm != null) gen.ExternTable.GetTypeRef(asm, arg0, valueType); else gen.GetTypeRef(arg0);
    }

    function init() {}

    function setLocalSlots() {
        var index = 0;
        for (local => type in locals) {
            if (!localSlots.exists(local))
                localSlots[local] = index++;
        }
    }

    function declareLocal(n:String, type:TType, ?slot:Int, ?throwOnDup) {
        if (slot == -1)
            slot = localSlots.list().length;
        trace('declaring local $n');
        trace(type.getName());
        types.checker.setGlobal(n, type);

        if (!locals.exists(n)) {
            localTypes.set(n, type);
            locals.set(n, toClrTypeRef(type));
            if (slot != null)
                localSlots[n] = slot;
        } else if (throwOnDup)
            throw 'duplicate local variable $n redefined as $type';
    }

    function generateLocals() {
        if (locals.list().length > 0)
            gen.CurrentMethodDef.InitLocals();
        gen.CurrentMethodDef.AddLocals({
            var arrList = new ArrayList();
            for (local => clrType in locals) {
                trace('adding local $local ${localSlots[local]}');
                if (localSlots[local] != TYPE_ONLY_SLOT)
                    arrList.Add(new Local(localSlots[local], local, clrType));
            }
            arrList;
        });
        locals = [];
        localSlots = [];
        localTypes = [];
    }

    // check if conversion is necessary, and if so, perform it
    function doConversion(param:Expr, paramType:TType, argType:TType):Expr {
        // trace(paramType);
        // trace(argType);
        return param;
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
        trace('adding {$kind} to closure');
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
        if (f.params == null || f.params.length == 0)
            return null;
        var ret = new GenericParameters();
        for (param in f.params) {
            var constraints = new ArrayList();
            for (constraint in param.constraints)
                constraints.Add(toClrTypeRef(types.checker.makeType(constraint)));
            var param = new GenericParameter(param.name, GenericParamAttributes.Covariant, constraints);

            ret.Add(param);
        }
        return ret;
    }

    /**
     * handle void returns.
     */
    function endMethod(ret:TType, location:Location) {
        noneInstr(peapi.Op.nop, location);
        if (!lastExprWasRet)
            if (types.checker.follow(ret).match(TVoid)) {
                noneInstr(peapi.Op.ret, location);
            } else {
                throw 'Missing return of type ${ret.typeStr()}';
            }
        commitBody();
    }

    var writingBody = false;

    function commitBody() {
        writingBody = true;
        for (instr in instructions)
            commitInstruction(instr);
        instructions = [];
        writingBody = false;
    }

    function commitInstruction(instruction:Instruction)
        switch instruction {
            case OpCode(op):
                gen.CurrentMethodDef.AddInstr(op);
            case Task(t):
                t();
        }

    function getGenericParamCount(funcArgs:Array<{t:TType}>) {
        var genericParamCount = 0;
        var genericParamNames:Map<Int, String> = [];
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
        return genericParamCount;
    }

    function addClosureToChecker() {
        if (closure == null)
            return;
        trace('adding closure to checker ${closure == null ? null : closure.name}');
        var decl:CClass = {
            name: closure.name,
            fields: [
                for (name => method in closure.methods)
                    {
                        name: name,
                        t: types.checker.makeType(CTFun([for (arg in method.args) arg.t], method.ret)),
                        canWrite: false,
                        complete: false,
                        isPublic: true,
                        params: if (method.params != null) {
                            var paramIndex = 0;
                            [for (param in method.params) TParam(param.name, paramIndex++)];
                        } else null
                    }
            ].concat([
                for (name => local in closure.localTypes)
                    {
                        name: name,
                        t: local,
                        canWrite: true,
                        complete: false,
                        isPublic: true,
                        params: null
                    }
                ]),
            statics: [],
            params: []
        };
        types.addType(decl.name, CTClass(decl));
    }

    function getLocalMetadata(expr:Expr):Null<LocalMetadata> {
        return switch expr.expr() {
            case EIdent(v): localMeta[v];
            case EField(expr, f):
                var name = printer.exprToString(expr);
                var g = types.checker.getGlobals()[name];
                if (g != null) { // static
                    [LMStatic];
                } else [LMInstance];
            default: [];
        }
    }

    var valueType:BaseClassRef;

    function getValueType():BaseClassRef
        return if (valueType == null) valueType = gen.ExternTable.GetTypeRef("mscorlib", "System.ValueType", true); else valueType;

    var baseType:BaseClassRef;

    function getBaseType():BaseClassRef
        return if (baseType == null) baseType = gen.ExternTable.GetTypeRef("mscorlib", "System.Object", false); else baseType;

    // haxe.lang.Function? Right? Wrapping a delegate?
    // nope, its a function pointer! F#$K C# :D
    function getFunctionTypeRef(args:Array<{name:String, opt:Bool, t:TType}>, ret:TType):BaseTypeRef {
        // trace('resolving function type with args ${args.map(arg -> arg.t.typeStr())}, ret ${ret.typeStr()} ${[for (key in functionPointerTypes.keys()) key].join(', ')}');
        if (ret == null || ret.match(TMono({r: null})))
            ret = TVoid;
        var typeStr = getFuncPtrTypeStr(args, ret);
        var r = functionPointerTypes.get(typeStr);
        // trace('getting func pointer type: ${r.FullName}');
        if (typeStr == null || r == null)
            throw 'unable to resolve function type with args [${args.map(arg -> arg.t.typeStr())}], ret (${ret.typeStr()}) [${[for (key in functionPointerTypes.keys()) key].join(', ')}]';
        return r;
    }

    function defineFuncPtrType(decl:FunctionDecl) {
        var args = decl.args.map(arg -> {
            opt: false,
            t: types.checker.follow(types.checker.makeType(arg.t)),
            name: ''
        });
        var ret = types.checker.follow(types.checker.makeType(decl.ret));
        var typeStr = TFun(args, ret).typeStr();

        if (!functionPointerTypes.exists(typeStr))
            functionPointerTypes.set(typeStr, defineFuncPtrWrapper(toClrTypeRef(ret), [for (arg in args) toClrTypeRef(arg.t)]));
        else
            trace('$typeStr pointer type already exists');
    }

    /**
        * Should generate (as example, for a niladic-ish):
        * ```cs
        * unsafe struct hx_FuncPtr_$index {
            delegate*<void> _ptr;

            hx_FuncPtr_$index(delegate*<void> ptr) => _ptr = ptr;
            public void invoke() => _ptr();
        }
        * ```
        * @param ret - Return CLR type
        * @param args  - CLR types of args
        * @return BaseTypeRef
     */
    function defineFuncPtrWrapper(ret:BaseTypeRef, args:Array<BaseTypeRef>):BaseTypeRef {
        var index = functionPointerTypes.list().length;
        var name = '${currentTypePath.split('.').toClrPath()}/$$hx_FuncPtr_$index';
        var flags = 0;
        flags |= cast TypeAttr.NestedPrivate;
        // flags |= cast TypeAttr.Sealed;
        // flags |= cast TypeAttr.SequentialLayout;
        flags |= cast TypeAttr.BeforeFieldInit;

        trace('Current typedef? ${gen.CurrentTypeDef.NestedFullName}');
        gen.BeginTypeDef(cast flags, name.split('/').pop().split('.').pop(), getBaseType(), null, EMPTY_LOC, null);
        var td = gen.CurrentTypeDef;
        inline function addField(f:FieldDef)
            td.AddFieldDef(f);
        var argLength = args.length;
        var argArray = cs.Lib.nativeArray(args, true);
        var args = {
            var ret = new ArrayList();
            var i = 0;
            for (arg in args)
                ret.Add(new ParamDef(peapi.ParamAttr.Default, 'arg${i++}', arg));
            ret;
        }
        flags = 0;
        flags |= cast FieldAttr.Private;
        var methPtrCallConv = CallConv.Default;
        var methPtrType = new MethodPointerTypeRef(methPtrCallConv, ret, args);
        // see https://github.com/mono/mono/blob/d4a369b3e651ca92e27ae711c37177e1f42fa300/mcs/ilasm/codegen/MethodPointerTypeRef.cs#L119
        var methPtrTypeRef = new TypeSpecMethodRef(methPtrType, methPtrCallConv, ret, "anon", argArray, 0);
        // _ptr
        addField(new FieldDef(cast flags, '_ptr', methPtrType));
        flags = 0;
        flags |= cast MethAttr.Public;
        flags |= cast MethAttr.HideBySig;

        var ctorCallConv = CallConv.Instance;
        var ctorArgs = {
            var ret = new ArrayList();
            ret.Add(new ParamDef(peapi.ParamAttr.Default, 'ptr', methPtrType));
            ret;
        };
        var thisTypeRef = gen.GetTypeRef(name);
        var implAttr:Int = 0;
        implAttr |= cast ImplAttr.IL;
        inline function endMethod() {
            noneInstr(peapi.Op.nop, EMPTY_LOC);
            noneInstr(peapi.Op.ret, EMPTY_LOC);
            commitBody();
        }

        // constructor
        trace('defining constructor for ${td.NestedFullName} ${td.Name} ${td.FullName}');
        var method = new MethodDef(gen, cast flags, ctorCallConv, cast implAttr, '.ctor', Primitives.VOID, ctorArgs, EMPTY_LOC, null, td);
        callBaseConstructor();
        intInt32Instr(IntOp.ldarg_s, 0, EMPTY_LOC);
        intInt32Instr(IntOp.ldarg_s, 1, EMPTY_LOC);
        fieldInstr(FieldOp.stfld, methPtrType, thisTypeRef, '_ptr', EMPTY_LOC);
        endMethod();
        gen.EndMethodDef(EMPTY_LOC);

        // invoke
        method = new MethodDef(gen, cast flags, ctorCallConv, cast implAttr, 'invoke', ret, args, EMPTY_LOC, null, td);
        for (i in 1...argLength + 1) { // skip this arg...
            intInt32Instr(IntOp.ldarg_s, i, EMPTY_LOC);
        }
        intInt32Instr(IntOp.ldarg_s, 0, EMPTY_LOC);
        fieldInstr(FieldOp.ldfld, methPtrType, thisTypeRef, "_ptr", EMPTY_LOC);
        putInstr(new CalliInstr(methPtrCallConv, ret, argArray, EMPTY_LOC));
        endMethod();
        gen.EndMethodDef(EMPTY_LOC);

        gen.EndTypeDef();
        var ret = getTypeRef(name);
        trace('returning wrapper: ${ret.FullName} ${thisTypeRef.FullName} ${ret == thisTypeRef}');
        @:privateAccess peTypes.structTypes.push(name);
        return ret;
    }

    function getFuncPtrTypeStr(args:Array<{name:String, opt:Bool, t:TType}>, ret:TType)
        return TFun(args.map(arg -> {
            arg.opt = false;
            arg.name = '';
            arg;
        }), ret).typeStr();

    var tryBlock:TryBlock;

    var catchBlock:CatchBlock;

    function getNestedFullName(arg0:BaseTypeRef):String {
        var n = arg0.FullName.split('/').pop();
        var r = '${currentTypePath.split('.').toClrPath()}/$n';
        trace('nested full name: $r');
        return r;
    }

    function getFunctionPointerTypeRef(args:Array<{name:String, opt:Bool, t:TType}>, ret:TType):BaseTypeRef {
        return new MethodPointerTypeRef(CallConv.Default, toClrTypeRef(ret), {
            var ret = new ArrayList();
            for (arg in args)
                ret.Add(new ParamDef(peapi.ParamAttr.Default, arg.name, toClrTypeRef(arg.t)));
            ret;
        });
    }

    function isStruct(type:Null<BaseTypeRef>):Bool {
        return type.FullName.indexOf('$$hx_FuncPtr_') != -1;
    }

    function getCurrentNestedName():String {
        var clrTypePath = currentTypePath.split('.').toClrPath();
        if (gen.CurrentTypeDef.FullName == clrTypePath)
            return clrTypePath;
        else
            return '${clrTypePath == null ? '' : clrTypePath.split('.').toClrPath() + '/'}${gen.CurrentTypeDef.FullName.split('.').pop()}';
    }

    function callBaseConstructor() {
        intInt32Instr(IntOp.ldarg_s, 0, EMPTY_LOC);
        var baseType = getTypeRef("System.Object");
        var ctor = baseType.GetMethodRef(Primitives.VOID, CallConv.Instance, ".ctor", cs.Lib.nativeArray([], true), 0);
        methodInstr(MethodOp.call, ctor, EMPTY_LOC);
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
    var localTypes:Map<String, TType>;
    var methods:Map<String, FunctionDecl>;

    var clrTypeRef:Void->BaseTypeRef;
    var local:Expr;
    var localName:String;
}

typedef LocalMetadata = Array<LocalMetaEntry>;

enum LocalMetaEntry {
    LMInstance;
    LMStatic;
}

enum InstructionBase {
    OpCode(op:IInstr);
    Task(task:Void->Void);
}

abstract Instruction(InstructionBase) from InstructionBase to InstructionBase {
    @:from public static function fromTask(t:Void->Void):Instruction
        return Task(t);

    @:from public static function fromOpCode(op:IInstr):Instruction
        return OpCode(op);
}
