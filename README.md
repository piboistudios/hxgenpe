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
        - Haxelib

Or
 - Write a Haxe-in-Haxe parser/typer that isn't a port of the OCaml code that we have pictures of on our walls at home.
 - Ask questions and give criticisms, I'm known to give direct answers, for example...
### Am I a Mad Man?
Yes.


# POC
Simple Hello, World executable assembly (generated by hxgenPE.. with no input :) so really just a test of Ilasm lib)
![](https://github.com/piboistudios/hxgenpe/blob/master/poc.PNG?raw=true)


# How this relates to DLR/LINQ/System.Reflection.Emit
- This is not really different than LINQ, except that it uses ILASM to generate code whereas LINQ uses Reflection.Emit, and LINQ has its own in-memory IR, whereas this uses hscript/Haxe's TAST.
    - Also, this doubles as an actual static compiler.
    - Though, for runtime code generation... probably will end up using System.Reflection.Emit too, that or hijacking LibIlasm and making it able to generate instructions and types at runtime (right now it just exposes an API to build a payload and emit it all as an assembly)
- Dynamics will be implemented the same way they are in C#'s DLR bindings, except the base dynamic type will mimic the existing `haxe.lang.DynamicObject` in hxcs (to the best of my ability...). It would almost be like an ExpandoObject, except not (because you have to set properties on Expandos before reading from them or you get a runtime error.. `haxe.lang.DynamicObject` emits `null` values for "undefined" properties on an object.. this is obviously preferable behavior, if you want to crash the program, you can do that... if the value is `null`)
- Anonymous types will be implemented the same way as C# dynamics (using the CSharp RuntimeBinder stuff) _except_ the calling context will be the anonymous type itself
    - Basically, in C#, the calling context, e.g. the entry point to the callsite cache and/or the class where the static callsite member (which the C# compiler generates and injects in your code) gets defined, is bound to the method where the dynamic was operated on, this means if you pass the same dynamic object from one method to aonther, and they operate on the same properties in the exact same way, bindings are created twice (and different static members are generated by the compiler and injected into the class), because the methods still know nothing about the object up front, other than its dynamic, and each method will reference different static callsites (despite technically being able to reuse the callsites, though, the C# compiler obviously doesnt have this information, because _the concept of anonymous types does not exist in C#_). This obviously leads to really horrible generated code, just crack open a DLR binary and you'll see what I'm talking about. Obviously some of that ugliness is necessary, but if you _really_ look at it... some of it is not (if there were more information available to the compiler)
    - They (anonymous types) do exist in Haxe however, and we can essentially create them in DLR by:
        - Creating a Singleton for each anonymous type and making that the calling context (and also making the callsites that get injected into it public static members so that code can share callsites)
        - Referencing that callsite wherever that anonymous type is referenced and operated upon (the compiler will resolve the correct callsite, so there is no runtime cost for this change from how C# does it for dynamics)
    - As usual, I give you...
    - __The implications:__
        - We can greatly reduce the initial binding time (well, if you use anonymous types, obviously if you use dynamics it'll be the same as it is in C#, because a C# `dynamic` is a Haxe `Dynamic` but with some minor... differences, mostly because sometimes C# generates callsites when it doesn't need to even just for `dynamic`s, such as when you call a well-known, statically typed method on a `dynamic`... you don't actually need a callsite for that, C# creates one anyways though, not sure why)
        - Since DLR is actually kind of performant, dynamics and anonymous types will be almost, if not, as performant here as they are on hxnodejs (or at least they should be if DLR doesn't suck, but there are great similarities between the approach it takes and the approach v8 takes in that they use runtime caching/code generation based on callsites and runtime type inspection/reflection to handle dynamic objects, which may assume different forms/structure at runtime)
        - Any other CLR application will be able to interact with hxgenPE's anonymous types (although, they will have to type them as `dynamic`, because again, they don't have a concept of anonymous types)
            - The reverse is also true, so if you interact with, say, some IronPython/IronRuby library (or really any CLR library that deals in dynamics), you could take its dynamic object outputs and type them to optimize how the hxgenPE app interacts with it
        - This essentially makes the calling context the anonymous type itself (e.g. the Singleton represents the Anonymous Type... it essentially makes anonymous types real types by hosting the CallSites that will be used to operate on them; you can think of a callsite as being like helpers that turn operations on dynamics into type-safe ones, it's probably not the technical definition of what they are, but thats basically what they are, by having access to a callsite, you know how to interact with an object in a "type-safe" manner, hence, a Singleton with public static members, so the entire program can interact with the same anonymous types in a "type-safe" manner) 

# Other Notes:
- Haxe has TCE (Tail Call Elimination), but we can do TCE and TCO here. We can't eagerly use `tail.` ops, because apparently there's a non-negligible cost to those, but with some magic (maybe like some counter that determines when the cost of a tail op breaks even with the cost of managing the stack trace) we can do optimization.
    - Also, there's a planned `@:tail return` compiler macro for hxgenPE to allow the user to control when a `tail.` instruction is inserted before a `ret` instruction, and probably will add options/metadata for controlling whether TCO/TCE happens (this is because ideally, this should be as good as F# and C# and also better... I think `@:tail return` is better for the folks that want fine-grained control)
- Closures will probably work like they do with hxcs except perhaps an attempt will be made to make them unify with the `Action<T...>` and `Func<T...>` types.
