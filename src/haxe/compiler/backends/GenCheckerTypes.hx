package haxe.compiler.backends;

interface GenCheckerTypes extends CheckerTypes {
    function addType(decl:ModuleDecl):Void;
    function toTType(arg0:Null<CType>):TType;
    function lookup(name:String):ModuleDecl;
    
    function setPack(currentPackage:String):Void;
    var checker:Checker;
}






