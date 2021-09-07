# hxgenPE

`hxgenPE` aims to be a Haxe -> PE compiler, written entirely in Haxe.

## Goals
1. __Should run at runtime__ - you should be able to compile Haxe into Portable Executable code at runtime, and also run that code at runtime; Haxe-in-Haxe; the time to output from Haxe source code will matter and directly impact applications relying on this functionality (which is as [trendy](https://en.wikipedia.org/wiki/Representational_state_transfer#Code_on_demand_(optional)) as it is [useful](https://en.wikipedia.org/wiki/Machine_learning))
2. __Don't deal with an IL__ - only in-memory IR (no MSIL/CIL generation); this is for performance reasons which leads me to
3. __Don't require translation of the IR__ - it should only be the result of parsing Haxe source (so we can't take the Haxe compiler's TAST and encode it; it's not serializable by default because it contains types that really only exist in OCaml and would have to be translated; this means we need a [parser](https://github.com/HaxeFoundation/hscript/blob/master/hscript/Parser.hx) and [type checker](https://github.com/HaxeFoundation/hscript/blob/master/hscript/Checker.hx)...)
4. __Be able to serialize an IR__ - for example, to send it to another service/application operating as a compiler back-end that can then generate the code on the corresponding platform; ideally haxe-in-haxe codegen back-ends should be written for the target language, as, much like with CLR, the language will probably have the best support for generating its own code; this is at least true for CLR, I assume JVM probably has a similar library for emitting raw JVM opcodes and packages/jars (whatever you call them), and this is definitely going to be true if ever there were an LLVM back-end for Haxe (every lib for producing LLVM that I know of is obviously written in an LLVM language)
5. __Modularity__ - `hscript` is by design, pretty modular (that is, you can use the parser without the checker, you can write plugins, like the Async module, etc...), this build system should be too, and in fact, this part is only concerned with generating PE code from hscript and haxe.macro AST, and would only be available on C# (and eventually PE) targets. In practice, to do this at runtime, you'd need hscript, and under the hood (at least starting out) this will use hscript.

This would allow you to, for example, opt in and out of optimizations/code transformations applied to Haxe code at runtime (you basically drive the compiler). This also means that, to write other targets, one needs only to write a back-end, and a back-end can basically be described as:
```haxe
typedef BackEnd = {
    function compileHaxe(in:Array<haxe.macro.Type>):haxe.io.Bytes; // <--- the executable code in binary; whether it be LLVM, JAR, or PE
    function compileHscript(in:Array<hscript.Expr.ModuleDecl>):haxe.io.Bytes; // <--- if hscript becomes able to support full Haxe language, this would support the same set of features as compileHaxe; this also makes it a pluggable part of hscript to compile code on the fly
}
```

This is a priority list.

### Implications 
- Starting out (and in the happy path, ending out), this will use `hscript` to parse and type-check code; there will be two versions of the code, one operating directly on the [hscript AST](https://github.com/HaxeFoundation/hscript/blob/master/hscript/Expr.hx) and one operating on the [haxe macro AST, translated from the hscript AST](https://github.com/HaxeFoundation/hscript/blob/master/hscript/Macro.hx) (this is so as to achieve goal#3)
 - This means `hxgenPE` will only support a subset of the Haxe language at first, and all testing will be done for both APIs using hscript to generate macro IR and its own IR.
 - In the happy path this means that:
    -  `hscript` will support parsing and typing of the full Haxe language (yes, this is a massive endeavour, but also one that would have had to happen in one way or another, whether for hscript or for hxparser, or you name it, in order to have a truly Haxe-in-Haxe compiler that isn't a direct port of the OCaml code we all know and love so much that we want a Haxe-in-Haxe compiler in the first place)
    - You'd be able to interpret, compile (into PE), then JIT (for portability) or AOT (for speed) compile (and then obviously execute) that PE all with hscript. 
    - Since this aims to become the first haxe target that utilizes a full haxe-in-haxe approach to building, it is kind of a prototype. This implies:
        - If this design makes sense and works, `hscript` would become the basis for Haxe-in-Haxe parsing/lexing/typing
        - We can finally eventually say goodbye to `haxe.macro` package (or at least soft deprecate it, with support only for legacy macro code), which, while it does nice things, and is one of the only of its kind, could be better to us developers.
 - In the sour path this means that:
    - Technically, work to continue writing the code to emit the proper PE code for the haxe macro AST can continue at this point (and the runtime code generation will be deficient, but still suitable to generate complex code; barring support for generics, so casting as the alternative)
    - Time will have been wasted contributing to `hscript` in order to make it support full Haxe, but `hxgenPE` will be partially functional (it'll be able to run on a subset of the haxe language), and able to fulfill goal#1
    
    - And we would need:
        - A solution to get the IR efficiently serialized/deserialized from the haxe eval/macro runtime into the hxcs (and eventually `hxgenpe`) runtime of `hxgenPE` (this comes at a cost and technically violates goal#2/3) _or_
        - A Haxe-written solution to parse/type Haxe (that _isn't_ a port of the OCaml code that we treasure)
            - As far as I can tell, this would be a similar effort to making `hscript` support the full Haxe language, so giving up on the `hscript` venture is not really ideal, so this is...
### How you can help
 - Contribute to `hscript`; here's what needs to happen:
    - Parsing:
        - Abstracts
        - Enums
        - Generics
    - Typing:
        - Everything that can't be parsed currently
        - Macros (have fun)
    - Optimizations (the haxe compiler is out of the picture at this point, so those are too)
    - Ecosystem:
        - Haxelib (although, technically)

Or
 - Write a Haxe-in-Haxe parser/typer that isn't a port of the OCaml code that we have pictures of on our walls at home.
 - Ask questions and give criticisms, I'm known to give direct answers, for example...
### Am I a Mad Man?
Yes.

# POC
Simple Hello, World executable assembly (generated by hxgenPE.. with no input :) so really just a test of Ilasm lib)
![](/raw/master/poc.png)