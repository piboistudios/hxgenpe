package haxe.compiler.backends.pe;

import cs.system.reflection.PropertyInfo;
import cs.system.reflection.MethodInfo;
import cs.system.reflection.MethodInfo;
import cs.system.reflection.FieldInfo;
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
        if (type != null) {
            trace('Adding type: $type\r\n$type');
            types.set(name, type);
        }
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
                    if(v.expr != null) checker.check(v.expr, WithType(checker.makeType(v.type, v.expr)));
                    else checker.makeType(v.type);
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

    function mkNetLibMeta(asm)
        return {
            name: "netLib",
            params: [
                EConst(CString(asm)).mk({
                    e: null,
                    origin: null,
                    line: 0,
                    pmin: 0,
                    pmax: 0
                })
            ]
        };

    public function loadAssembly(asmName) {
        var asm = Assembly.LoadFrom('$asmName.dll');
        trace('loading $asm $asmName');
        var types = asm.GetTypes();
        trace('types: ${types.length}');
        var allTypes = [];
        for (type in types) {
            if (type.IsClass) {
                trace(type.FullName);
                var cclass:ClassDecl = {
                    name: toHxTypeName(type.FullName).join('.'),
                    params: {}, // TODO: hscript generic params
                    isPrivate: type.IsNotPublic,
                    isExtern: true,
                    implement: [
                        for (intface in type.GetInterfaces())
                            CTPath(toHxTypeName(intface.FullName))
                    ],
                    meta: [mkNetLibMeta(asmName)],

                    fields: [
                        // variables
                        for (field in cs.Lib.array(type.GetFields()).filter(f -> f.IsPublic))
                            {
                                name: field.Name,
                                meta: getClrFieldMeta(field),
                                kind: KVar(getClrVarDecl(field)),
                                access: getClrFieldAccess(field)
                            }
                    ] // properties
                        .concat(cs.Lib.array(type.GetProperties()).fold((prop, a) -> {
                            var array:Array<FieldDecl> = a;
                            trace(prop);
                            var varDecl:FieldDecl = {
                                name: prop.Name,
                                meta: getClrPropertyMeta(prop),
                                kind: KVar(getClrPropertyDecl(prop)),
                                access: getClrPropertyAccess(prop)
                            };
                            array.push(varDecl);
                            var getter = prop.GetMethod, setter = prop.SetMethod;
                            if(getter != null) {

                                var getDecl:FieldDecl = {
                                    name: getter.Name,
                                    meta: getClrMethodMeta(getter),
                                    kind: KFunction(getClrMethodDecl(getter)),
                                    access: getClrMethodAccess(getter)
                                };
                                array.push(getDecl);
                            }
                            if(setter != null) {

                                var setDecl:FieldDecl = {
                                    name: setter.Name,
                                    meta: getClrMethodMeta(setter),
                                    kind: KFunction(getClrMethodDecl(setter)),
                                    access: getClrMethodAccess(setter)
                                };
                                array.push(setDecl);
                            }
                            
                            return array;
                        }, [])) // methods
                        .concat([
                            for (method in type.GetMethods())
                                {
                                    name: method.Name,
                                    meta: getClrMethodMeta(method),
                                    kind: KFunction(getClrMethodDecl(method)),
                                    access: getClrMethodAccess(method)
                                }
                        ]),
                    extend: CTPath(toHxTypeName(type.BaseType.FullName))
                };
                addType(DClass(cclass));
            } else if (type.IsEnum) {} else if (type.IsInterface) {} else if (type.IsValueType) {} else {}
            // if(decl != null) addType(decl);
        }
    }

    function toHxTypeName(arg0:String, ?pos:haxe.PosInfos):Array<String> {
        trace(pos);
        var parts = arg0.split('.');
        if (parts[0] == "System")
            parts.unshift('cs');
        for (i in 0...parts.length - 1) {
            var part = parts[i];
            part = part.substr(0, 1).toLowerCase() + part.substr(1);
        }
        return parts;
    }

    function getClrFieldMeta(field:FieldInfo):Metadata {
        return [];
    }

    function getClrFieldAccess(field:FieldInfo):Array<FieldAccess> {
        var access = [];
        if (field.IsStatic)
            access.push(AStatic);
        if (field.IsPublic)
            access.push(APublic);
        if (field.IsPrivate)
            access.push(APrivate);
        return access;
    }

    function getClrVarDecl(field:FieldInfo):VarDecl {
        // TODO: something with... field.FieldType.IsGenericType 
        var decl:VarDecl = {
            type: CTPath(toHxTypeName(if (field.FieldType.FullName == null) field.FieldType.Name else field.FieldType.FullName)),
            set: null,
            get: null,
            expr: null
        };
        return decl;
    }

    function getClrMethodMeta(method:MethodInfo):Metadata {
        return [];
    }

    function getClrMethodAccess(method:MethodInfo):Array<FieldAccess> {
        var access = [];
        if (method.IsStatic)
            access.push(AStatic);
        if (method.IsPublic)
            access.push(APublic);
        if (method.IsPrivate)
            access.push(APrivate);
        return access;
    }

    function getClrMethodDecl(method:MethodInfo):FunctionDecl {
        var decl:FunctionDecl = {
            ret: CTPath(toHxTypeName(if (method.ReturnType != null) if (method.ReturnType.FullName != null) method.ReturnType.FullName else method.ReturnType.Name else "System.Void")),
            args: [
                for (parameter in cs.Lib.array(method.GetParameters()).map(p -> {type: p.ParameterType, name: p.Name, opt: p.IsOptional}))
                    ({
                        t:CTPath(toHxTypeName(if (parameter.type.FullName == null) parameter.type.Name else parameter.type.FullName)), name:parameter.name,
                        opt:parameter.opt
                    } : hscript.Argument)
            ],
            expr: null
        }
        return decl;
    }

    function getClrPropertyMeta(property:PropertyInfo):Metadata {
        return [];
    }

    function getClrPropertyDecl(property:PropertyInfo):VarDecl {
        trace(property.PropertyType.FullName);
        var name = if (property.PropertyType.FullName == null) property.PropertyType.Name else property.PropertyType.FullName;
        trace(name);
        var decl:VarDecl = {
            type: CTPath(toHxTypeName(name)),
            set: if (property.GetMethod != null) property.GetMethod.Name else null,
            get: if (property.SetMethod != null) property.SetMethod.Name else null,
            expr: null
        };
        return decl;
    }

    function getClrPropertyAccess(property:PropertyInfo):Array<FieldAccess> {
        var access = [];
        return access;
    }
}
