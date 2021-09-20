package haxe.compiler.backends;

// this needs to go in a separate library, only cross-platform code...

typedef AbstractInfo = {
    var implClass:String;
    var toConversions:Array<FieldDecl>;
    var fromConversions:Array<FieldDecl>;
    var args:Array<TType>;
};
// TODO: hscript add parsing of abstracts
class Gen {
    public var types:GenCheckerTypes;
    // public var abstractMapping:Map<AbstractDecl, AbstractInfo> = [];

    // function mapAbstract(a:AbstractDecl) {
        
    // }
}
