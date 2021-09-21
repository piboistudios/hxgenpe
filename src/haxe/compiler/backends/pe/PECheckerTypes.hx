package haxe.compiler.backends.pe;

import hscript.Checker;
import hscript.Expr;

using hscript.Tools;
using Lambda;

class PECheckerTypes extends CheckerBase {
    var typeAssemblies:Map<String, String> = new Map();
    var currentPack = '';

    public function new() {
        t_string = TInst({
            name: "System.String",
            statics: [],
            params: [],
            fields: []
        }, []);
    }

    public override function setPack(pk) {
        currentPack = pk;
    }

    public override function addType(decl:ModuleDecl) {
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
                    t: checker.makeType(c.t)
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


    public function toCField(f:FieldDecl):CField {
        return {
            name: f.name,
            params: [], // TODO: add params to hscript fields
            t: switch f.kind {
                case KFunction(f):
                    var args = f.args.map(arg -> {name: arg.name, opt: arg.opt, t: checker.makeType(arg.t, arg.value)});
                    TFun(args, if (f.ret != null) checker.makeType(f.ret, f.expr) else TVoid);
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
            var assemblyMeta = d.meta.find(m -> m.name == "netLib");
            if (assemblyMeta.params[0].e.match(EConst(CString(_)))) {
                var const:hscript.Expr.Const = assemblyMeta.params[0].expr().getParameters()[0];
                var assemblyName = const.getParameters()[0];
                typeAssemblies[d.name] = assemblyName;
            }
        }
    }

    
}
