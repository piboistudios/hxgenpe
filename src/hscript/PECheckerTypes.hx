package hscript;

import hscript.Checker;
import hscript.Expr;

using Lambda;
using hscript.PECheckerTypes;

class CheckerBase implements CheckerTypes {
    var types:Map<String, CTypedecl> = new Map();

    public var checker:Checker;

    var localParams:Map<String, TType>;

    public function resolve(name:String, ?args:Array<TType>):TType {
        if (name == "Null") {
            if (args == null || args.length != 1)
                throw "Missing Null<T> parameter";
            return TNull(args[0]);
        }
        var t = types.get(name);
        if (t == null)
            return null;
        if (args == null)
            args = [];
        return switch (t) {
            case CTClass(c): TInst(c, args);
            case CTEnum(e): TEnum(e, args);
            case CTTypedef(t): TType(t, args);
            case CTAbstract(a): TAbstract(a, args);
            case CTAlias(t): t;
        }
    }

    public function getType(name:String, ?args:Array<TType>):TType {
        if (localParams != null) {
            var t = localParams.get(name);
            if (t != null)
                return t;
        }
        var t = resolve(name, args);
        if (t == null) {
            var pack = name.split(".");
            if (pack.length > 1) {
                // bugfix for some args reported as pack._Name.Name while they are not private
                var priv = pack[pack.length - 2];
                if (priv.charCodeAt(0) == "_".code) {
                    pack.remove(priv);
                    return getType(pack.join("."), args);
                }
            }
            return TUnresolved(name); // most likely private class
        }
        return t;
    }

    public var t_string:TType;
}

class PECheckerTypes extends CheckerBase {
    var typeAssemblies:Map<String, String> = new Map();
    var currentPack = '';

    public function new() {
        t_string = TDynamic;
    }

    public function setPack(pk) {
        currentPack = pk;
    }

    public function addType(decl:ModuleDecl) {
        var name = currentPack;
        var type:CTypedecl = switch decl {
            case DClass(c):
                name += c.name;
                addDeclAssembly(c);
                var cclass:CClass = {
                    name: name,
                    fields: [
                        for (f in c.fields.filter(field -> !field.access.contains(FieldAccess.AStatic)).map(toCField))
                            f.name => f
                    ],
                    statics: [
                        for (f in c.fields.filter(field -> field.access.contains(FieldAccess.AStatic)).map(toCField))
                            f.name => f
                    ],
                    params: []
                };
                CTClass(cclass);

            case DTypedef(c):
                name += c.name;
                var ctypedef:CTypedef = {
                    name: name,
                    params: [],
                    t: toTType(c.t)
                };
                CTTypedef(ctypedef);
            // TODO: hscript abstracts and enums
            // case DAbstract(c):
            // case DEnum(c):
            default:
                null;
        }
        if (type != null)
            types.set(name, type);
    }

    public function getAssembly(type:String):String
        return typeAssemblies[type];

    public function toTType(t:CType):TType
        return if (t == null) TVoid else switch t {
            case CTPath(pack, params):
                resolve(pack.join('.'), [for (param in params) toTType(param)]);
            case CTAnon(fields):
                var fields = [
                    for (field in fields)
                        {
                            t: toTType(field.t),
                            opt: isOpt(field.t), // TODO: only works for function args in hscript, not anon fields atm.
                            name: field.name
                        }
                ];
                TAnon(fields);
            case CTParent(t): toTType(t);
            case CTOpt(t): toTType(t);
            case CTNamed(_, t): toTType(t);
            case CTFun(args, ret):
                var args = [
                    for (arg in args)
                        switch arg {
                            case CTNamed(n, t):
                                {name: n, t: toTType(t), opt: isOpt(t)}
                            default:
                                throw 'invalid hscript argument type';
                        }
                ];
                TFun(args, toTType(ret));
                // default:
                //     null;
        }

    public function toCField(f:FieldDecl):CField {
        return {
            name: f.name,
            params: [], // TODO: add params to hscript fields
            t: switch f.kind {
                case KFunction(f):
                    var args = f.args.map(arg -> {name: arg.name, opt: arg.opt, t: toTType(arg.t)});
                    TFun(args, if (f.ret != null) toTType(f.ret) else TVoid);
                case KVar(v):
                    checker.check(v.expr);
            },
            isPublic: f.access.contains(APublic),
            canWrite: true,
            complete: !f.meta.exists(m -> m.name == ':noCompletion')
        }
    }

    function addDeclAssembly(d:ClassDecl) {
        if (d.isExtern) {
            var assemblyMeta = d.meta.array().find(m -> m.name == "netLib");
            if (assemblyMeta.params[0].e.match(EConst(CString(_)))) {
                var const:hscript.Expr.Const = assemblyMeta.params[0].e.getParameters()[0];
                var assemblyName = const.getParameters()[0];
                typeAssemblies[d.name] = assemblyName;
            }
        }
    }

    function isOpt(arg0:CType):Bool {
        return arg0.match(CTOpt(_));
    }
}
