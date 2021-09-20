package haxe.compiler.backends;

import mono.ilasm.BaseClassRef;

class CheckerBase implements GenCheckerTypes {
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

    public function addType(decl:ModuleDecl) throw new haxe.exceptions.NotImplementedException();

    public function toTType(arg0:Null<CType>):TType throw new haxe.exceptions.NotImplementedException();

    public function setPack(currentPackage:String) throw new haxe.exceptions.NotImplementedException();
}
