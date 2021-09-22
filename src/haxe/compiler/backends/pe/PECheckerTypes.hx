package haxe.compiler.backends.pe;

import cs.system.reflection.MemberInfo;
import cs.system.reflection.MemberInfo;
import haxe.exceptions.NotImplementedException;
import cs.system.reflection.Assembly;
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

    public function loadAssembly(path) {
        var asm = Assembly.LoadFrom(path);
        var asmName = asm.GetName().Name;
        var types = asm.GetTypes();
        var allTypes = [];
        for (type in types) {
            allTypes.push(type);
            var nestedTypes = cs.Lib.array(type.GetNestedTypes());
            do {
                for (type in nestedTypes) {
                    allTypes.push(type);
                    nestedTypes = nestedTypes.concat(cs.Lib.array(type.GetNestedTypes()));
                }
            } while (nestedTypes.length != 0);
        }
        for (type in allTypes) {
            var decl:ModuleDecl = if (type.IsClass) {
                var nameParts = type.FullName.split('.').map(part -> part.toLowerCase());
                nameParts[nameParts.length - 1] = nameParts[nameParts.length - 1].substr(0, 1).toUpperCase() + nameParts[nameParts.length - 1].substr(1);
                var typeName = nameParts.join('.');
                var cclass:ClassDecl = {
                    name: typeName,
                    params: {}, // TODO: hscript generic params
                    isPrivate: type.IsNotPublic,
                    isExtern: true,
                    implement: [for (intface in type.GetInterfaces()) CTPath(toHxTypeName(intface.FullName))],
                    meta: [
                        {
                            name: "netLib",
                            params: [
                                EConst(CString(asmName)).mk({
                                    e: null,
                                    origin: null,
                                    line: 0,
                                    pmin: 0,
                                    pmax: 0
                                })
                            ]
                        }
                    ],
            
                    fields: [for(field in type.GetMembers()) {
                        name: field.Name,
                        meta: getClrFieldMeta(field),
                        kind: KVar(getClrVarDecl(field)),
                        access: getClrFieldAccess(field)
                    }],
                    extend: CTPath(toHxTypeName(type.BaseType.FullName))
                };
                DClass(cclass);
            } else if (type.IsEnum) {
                throw new NotImplementedException();
            } else if (type.IsInterface) {
                throw new NotImplementedException();
            } else if (type.IsValueType) {
                throw new NotImplementedException();
            } else {
                throw new NotImplementedException();
            }
            addType(decl);
        }
    }

    function toHxTypeName(arg0:String):Array<String> {
        throw new haxe.exceptions.NotImplementedException();
    }

	function getClrFieldMeta(field:MemberInfo):Metadata {
		throw new haxe.exceptions.NotImplementedException();
	}

	function getClrFieldAccess(field:MemberInfo):Array<FieldAccess> {
		throw new haxe.exceptions.NotImplementedException();
	}

	function getClrVarDecl(field:MemberInfo):VarDecl {
		throw new haxe.exceptions.NotImplementedException();
	}
}
