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


    // wait I'm an idiot this should deal resolve declarations not types...
    function resolveMethod(e:Expr, params:Array<Expr>):{
        ?caller:{type:TType, expr:Expr},
        field:{
            name:String,
            t:TType,
            params:Array<TType>,
            kind:FunctionKind
        }
    } {
        var caller:Expr = null;
        switch e.expr() {
            case EIdent(v):
                switch v.split('.') {
                    case [local, _]: caller = EIdent(local).mk(e);
                    case v if (v.length > 2):
                        var local = v.slice(0, v.length - 1).join('.');
                        caller = EIdent(local).mk(e);
                }
            default:
                caller = e;
        }
        
        var paramTypes = [for (param in params) types.checker.check(param)];
        
        function findMethod(field:{t:TType})
            return switch field.t {
                case TFun(funcArgs, ret):
                    for (i in 0...funcArgs.length) {
                        var funcArg = funcArgs[i];
                        var paramType = paramTypes[i];

                        @:privateAccess if (!types.checker.tryUnify(funcArg.t, paramType)) {
                            return false;
                        }
                    }
                    true;
                default: false;
            }
        if (caller == null) {
            var functionType = types.checker.check(e);
            switch functionType {
                case TFun(args, ret):
                    return {
                        field: {
                            name: "<>closure",
                            t: functionType,
                            kind: Closure,
                            params: []
                        }
                    }
                default: throw 'Invalid call expression $functionType $e';
            }
        } else {
            var callerType = types.checker.check(caller);
            var isStatic = false;
            var method:Dynamic = switch callerType {
                case TAnon(fields):
                    fields.find(findMethod);
                case TInst(c, args):
                    var ret = c.fields.find(findMethod);
                    if(ret == null) {
                        ret = c.statics.find(findMethod);
                        if(ret != null) isStatic = true;
                    }
                    ret;
                default: throw 'Unable to resolve function ${printer.exprToString(e)} with params (${paramTypes.map(p -> Checker.typeStr(p)).join(', ')}})';
            }
            method.kind = if(isStatic) StaticMethod else InstanceMethod;
            return {
                caller: {
                    type: callerType,
                    expr: caller
                },
                field: method
            }
        }
    }
}
