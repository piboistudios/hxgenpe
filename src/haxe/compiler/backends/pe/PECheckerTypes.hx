package haxe.compiler.backends.pe;

import cs.system.collections.generic.IEnumerable_1;
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
using haxe.compiler.backends.pe.PECheckerTypes;

class PECheckerTypes extends CheckerBase {
    var typeAssemblies:Map<String, String> = new Map();
    var currentPack = '';

    public function new() {
        t_string = TUnresolved("cs.system.String");
    }

    public override function setPack(pk) {
        currentPack = pk;
    }
    public static function getName(t:cs.system.Type) {
        var ret =  if(t.FullName == null) t.Name else t.FullName;
        return if(ret == null) "<Unknown>" else ret;
    }
    public override function addType(decl:ModuleDecl) {
        // trace('Adding $decl');
        var name = currentPack;
        var type:CTypedecl = switch decl {
            case DClass(c):
                name += c.name;
                addDeclAssembly(c);
                var cclass:CClass = {
                    name: name,
                    fields: [
                        for (f in c.fields.filter(field -> !field.access.contains(FieldAccess.AStatic)).map(toCField))
                            f
                    ],
                    statics: [
                        for (f in c.fields.filter(field -> field.access.contains(FieldAccess.AStatic)).map(toCField))
                            f
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
            // trace('Adding type: $type\r\n$type');
            types.set(name, type);
            decls.set(name, decl);
            checker.setGlobal(name, TType({name: name, t: resolve(name), params:[]}, []));
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
                    if (v.expr != null) checker.check(v.expr, WithType(checker.makeType(v.type, v.expr))); else checker.makeType(v.type);
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
        // so.. I guess we need TypeInfo and not Type... just look at the implementation of Type.GetEnumValues vs. TypeInfo.GetEnumValues in mscorlib
        var definedTypes = asm.DefinedTypes.GetEnumerator();
        var current = null;
        var types:Iterable<cs.system.reflection.TypeInfo> = {
            iterator: () -> {
                next: () -> current,
                hasNext: () -> {
                    var ret = definedTypes.MoveNext();
                    current = if(ret) definedTypes.Current else null;
                    ret;
                }
            }
        };
        var allTypes = [];
        var asmMeta = mkNetLibMeta(asmName);
        for (type in types) {
            if(!type.IsVisible) {
                // trace('Skipping ${type.Name}');
                continue;
            }
            var isPrivate = type.IsNotPublic;
            if (type.IsClass) {
                // trace(type.getName());
                var cclass:ClassDecl = {
                    name: toHxTypeName(type.getName()).join('.'),
                    params: [for(typeParam in type.GenericTypeArguments) {
                        name: typeParam.Name
                        // I guess just ignore constraints or whatever and see what chaos ensues
                    }], // TODO: hscript generic params
                    isPrivate: isPrivate,
                    isExtern: true,
                    implement: [
                        for (intface in type.GetInterfaces()) {
                            // trace(intface.Name);
                            // trace(intface.getName());
                            // trace(intface);
                            CTPath(toHxTypeName(intface.getName()));
                        }
                    ],
                    meta: [asmMeta],

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
                            // trace(prop);
                            var varDecl:FieldDecl = {
                                name: prop.Name,
                                meta: getClrPropertyMeta(prop),
                                kind: KVar(getClrPropertyDecl(prop)),
                                access: getClrPropertyAccess(prop)
                            };
                            array.push(varDecl);
                            var getter = prop.GetMethod, setter = prop.SetMethod;
                            if (getter != null) {
                                var getDecl:FieldDecl = {
                                    name: getter.Name,
                                    meta: getClrMethodMeta(getter),
                                    kind: KFunction(getClrMethodDecl(getter)),
                                    access: getClrMethodAccess(getter)
                                };
                                array.push(getDecl);
                            }
                            if (setter != null) {
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
                    extend: if(type.BaseType != null) CTPath(toHxTypeName(type.BaseType.getName())) else null
                };
                addType(DClass(cclass));
            } else if (type.IsEnum) {
                var eenum:EnumDecl = {
                    params: [], // no GADTs  in CLR, thank god :)
                    name: type.FullName,
                    meta: [asmMeta],
                    isPrivate: isPrivate,
                    isExtern: true,
                    fields: mkEnumFields(type)
                };
                addType(DEnum(eenum));
            } else if (type.IsInterface) {} else if (type.IsValueType) {} else {}
            // if(decl != null) addType(decl);
        }
    }

    function toHxTypeName(arg0:String, ?pos:haxe.PosInfos):Array<String> {
        // trace(pos);
        
        var parts = arg0.split('.');
        // trace(parts);
        for (i in 0...parts.length - 1) {
            var part = parts[i];
            parts[i] = part.substr(0, 1).toLowerCase() + part.substr(1);
        }
        if (parts[0] == "system")
            parts.unshift('cs');
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
    // @formatter:off
    function getClrVarDecl(field:FieldInfo):VarDecl {
        // TODO: something with... field.FieldType.IsGenericType
        
        var type = if (field.FieldType.IsGenericParameter) CTParam(field.FieldType.Name, field.FieldType.GenericParameterPosition) 
                    else CTPath(toHxTypeName(if (field.FieldType.getName() == null)
                            field.FieldType.Name else field.FieldType.getName())
                    );
        var decl:VarDecl = {
            type:type,
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
        var retType = if (method.ReturnType.IsGenericParameter) CTParam(method.ReturnType.Name, method.ReturnType.GenericParameterPosition) 
        else CTPath(toHxTypeName(if (method.ReturnType != null) 
                                    if (method.ReturnType.getName() != null) method.ReturnType.getName() 
                                    else method.ReturnType.Name 
                                else "System.Void"
                    )
            );
        var decl:FunctionDecl = {
            ret: retType,
            args: [
                for (parameter in cs.Lib.array(method.GetParameters()).map(p -> {type: p.ParameterType, name: p.Name, opt: p.IsOptional})) {
                    var paramType = if(parameter.type.IsGenericParameter) CTParam(parameter.type.Name, parameter.type.GenericParameterPosition) 
                                    else CTPath(toHxTypeName(
                                        if (parameter.type.getName() == null) parameter.type.Name 
                                        else parameter.type.getName())
                                    );
                    ({
                        t:paramType, 
                        name:parameter.name,
                        opt:parameter.opt
                    } : hscript.Argument);
                }
            ],
            expr: null
        }
        return decl;
    }

    function getClrPropertyMeta(property:PropertyInfo):Metadata {
        return [];
    }

    function getClrPropertyDecl(property:PropertyInfo):VarDecl {
        // trace(property.PropertyType.getName());
        var type = if(property.PropertyType.IsGenericParameter) CTParam(property.PropertyType.Name, property.PropertyType.GenericParameterPosition) 
                    else CTPath(toHxTypeName(property.PropertyType.getName()));
        var decl:VarDecl = {
            type: type,
            set: if (property.GetMethod != null) property.GetMethod.Name else null,
            get: if (property.SetMethod != null) property.SetMethod.Name else null,
            expr: null
        };
        return decl;
    }
    // @formatter:on
    function getClrPropertyAccess(property:PropertyInfo):Array<FieldAccess> {
        var access = [];
        return access;
    }

	function mkEnumFields(type:cs.system.Type):Array<FieldDecl> {
		throw new haxe.exceptions.NotImplementedException();
	}
}
