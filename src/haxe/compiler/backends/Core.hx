package haxe.compiler.backends;

import hscript.Printer;

// this needs to go in a separate library, only cross-platform code...

typedef AbstractInfo = {
    var implClass:String;
    var toConversions:Array<FieldDecl>;
    var fromConversions:Array<FieldDecl>;
    var args:Array<TType>;
};
enum FunctionKind {
    Closure;
    InstanceMethod;
    StaticMethod;
    Module;
}
// TODO: hscript add parsing of abstracts
class Gen {
    public var types:GenCheckerTypes;

    var printer = new Printer();

    // public var abstractMapping:Map<AbstractDecl, AbstractInfo> = [];
    // function mapAbstract(a:AbstractDecl) {
    // }


    
    
}
