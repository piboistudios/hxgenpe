package hscript;
import hscript.Checker;
import hscript.Expr;
class PECheckerTypes implements CheckerTypes {
	var typeAssemblies:Map<TType,String> = new Map();
    public function new() {
        
    }
	public function addType(decl:ModuleDecl) {
        throw new haxe.exceptions.NotImplementedException();
	}
	public function resolve(name:String, ?args:Array<TType>):TType {
		throw new haxe.exceptions.NotImplementedException();
	}

	public function getAssembly(type:TType):String {
        throw new haxe.exceptions.NotImplementedException();
    }
	@:noCompletion
	public function getType(name:String, ?args:Array<TType>):TType {
		throw new haxe.exceptions.NotImplementedException();
	}

	public var t_string:TType;
}