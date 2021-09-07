package haxe.compiler.backends;


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
    public function new(gen) {
        this.gen = gen;
    }

    public function buildHaxe(types:Array<haxe.macro.Type>) {
        throw 'not implemented';
    }
    public function buildHscript(types:Array<hscript.Expr.ModuleDecl>) {
        for(type in types)
            switch type {
                case DPackage(path):
                    gen.set_CurrentNameSpace(path.toClrPath());
                // case DImport(parts, everything):
                case DClass(c):
                    var flags = 0;
                    if(c.isPrivate) flags = flags|cast TypeAttr.Private;
                    else flags = flags|cast TypeAttr.Public;
                    
                    gen.BeginTypeDef(cast flags, c.name.split('.').pop(), null, null, type.location(), null);
                    for(field in c.fields) {
                        switch field.kind {
                            case KFunction(f):
                                generateMethod(c.name, field, f);
                            case KVar(v):
                                generateVar(c.name, field, v);
                        }
                    }
                default:
            }
    }

	function generateMethod(owner:String, field:FieldDecl, f:FunctionDecl) {
        var flags = 0;
        for(access in field.access)
            switch access {
                case APublic: flags |= cast MethAttr.Public;
                case AStatic: flags |= cast MethAttr.Static;
                case APrivate: flags |= cast MethAttr.Private;
                default:
            }
        var conv = peapi.CallConv.Default;
        var implAttr = ImplAttr.IL;
        var retType = f.ret.toClrTypeRef(this);
        var ownerType = gen.CurrentTypeDef;//lookupType(owner).toClrTypeDef(this);
        var paramList = new cs.system.collections.ArrayList();
        for(arg in f.args)
            paramList.Add(arg.t.toClrTypeRef(this));
        var genericParameters = null;
        var fieldDef = new MethodDef(gen, cast flags, conv, implAttr, field.name, retType, paramList, field.location(), genericParameters, ownerType);
        
    }

	function generateVar(owner:String, field:FieldDecl, v:VarDecl) {}

	public function lookupType(path:String):CType {
		throw new haxe.exceptions.NotImplementedException();
	}

	public function lookupDecl(path:String):ModuleDecl {
		throw new haxe.exceptions.NotImplementedException();
	}
}

class AssemblyLookup {
    public static var table = new Map<String, String>();
}
class HscriptTypeTools {

    public static function toClrTypeRef(t:hscript.Expr.CType, pe:GenPE)
        return switch t {
            case CTPath(pack, params):
                var path = pack.join('.');
                var type:hscript.Expr.ModuleDecl = pe.lookupDecl(path);
                type.toClrTypeRef(pe);
            case CTFun(_, _):
                new TypeRef("haxe.lang.Function", false, null);
            case CTAnon(_):
                new TypeRef("haxe.lang.DynamicObject", false, null);
            case CTParent(t):
                t.toClrTypeRef(pe);
            case CTOpt(t):
                t.toClrTypeRef(pe);
            case CTNamed(_, t):
                t.toClrTypeRef(pe);
            default:
                null;
        }
}
class HscriptFieldDeclTools {
    public static function location(decl:hscript.Expr.FieldDecl) {
        // return cl.pos; TODO: add position tracking to hscript module decls
        return new Location(0,0);
    }
}
class HscriptModuleDeclTools {
    public static function location(cl:hscript.Expr.ModuleDecl) {
        // return cl.pos; TODO: add position tracking to hscript module decls
        return new Location(0,0);
    }
    public static function toClrTypeRef(t:hscript.Expr.ModuleDecl, pe:GenPE):BaseTypeRef
        return switch t {
            case DImport(pack, everything):
                var t = pe.lookupType(pack.join('.'));
                 t.toClrTypeRef(pe);
            case DClass(c):
                if(c.isExtern) pe.gen.ExternTable.GetTypeRef(AssemblyLookup.table[c.name], c.name, false);
                else {
                    new TypeRef(c.name, false, null);
                }
            default: 
                null;
        }
}
class HscriptModuleTools {
    // public static function resolveType(module:Array<ModuleDecl>, path:String)
    //     return module.find(type -> switch type {
    //         case DClass(c):
    //             c.name == path;
    //         case DTypedef(c):
    //             if(c.name == path)
    //             switch c.t {
    //                 case CTPath(pack, _): pack.join('.') == path;
                    
    //                 default:
    //                     null;
    //             }

    //     })
}
class PathTools {
    public static function toClrPath(path:String) {
        return path.split('.').map(part -> part.substr(0,1).toUpperCase() + part.substr(1)).join('.');
    }
}

class PackTools {
    public static function toClrPath(path:Array<String>) {
        return path.map(part -> part.substr(0,1).toUpperCase() + part.substr(1)).join('.');
    }
}