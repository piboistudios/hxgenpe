package haxe.compiler.backends;

import hscript.Printer;

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

    var printer = new Printer();

    // public var abstractMapping:Map<AbstractDecl, AbstractInfo> = [];
    // function mapAbstract(a:AbstractDecl) {
    // }
    function resolveMethod(e:Expr, params:Array<Expr>):{
        ?callerType:TType,
        field:{
            name:String,
            t:TType
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
        var callerType = types.checker.check(caller);
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
        if (callerType == null) {
            var functionType = types.checker.check(e);
            switch functionType {
                case TFun(args, ret):
                    return {
                        field: {
                            name: "closure",
                            t: functionType
                        }
                    }
                default: throw 'Invalid call expression';
            }
        } else {
            var method:{name:String, t:TType} = switch callerType {
                case TAnon(fields):
                    fields.find(findMethod);
                case TInst(c, args):
                    c.fields.find(findMethod);
                default: throw 'Unable to resolve function ${printer.exprToString(e)} with params (${paramTypes.map(p -> Checker.typeStr(p)).join(', ')}})';
            }
            return {
                callerType: callerType,
                field: method
            }
        }
    }
}
