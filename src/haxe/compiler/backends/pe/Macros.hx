package haxe.compiler.backends.pe;

class Macros {
    // public static macro function bump(e:haxe.macro.Expr) return macro {
    //     var lastBatch = this.instructions.slice(-this.lastInstrBatchSize);
    //     $e;
    //     for(instr in lastBatch) this.putInstr(instr);
    //     this.lastInstrBatchSize = 1;
    // }
    public static macro function beforeNextInstructionSet(e:haxe.macro.Expr)
        return macro this.beforeNext.push(() -> $e);

    public static macro function afterNextInstructionSet(e:haxe.macro.Expr)
        return macro this.afterNext.push(() -> $e);
}
