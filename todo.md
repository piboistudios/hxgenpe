# TODO

## Code Gen
= [X] Function pointers (this will be what Haxe uses for haxe.lang.Function or whatever)
- [ ] Binops/Field assignment
- [ ] DLR with duck-typing for anons (oh man this is going to be fun /s..)
  - [ ] Field access
  - [ ] Binops/Field assignment
  - [ ] Dynamic call sites (method bound call sites)
  - [ ] Anon call site (call sites bound to singleton type reification interface for anon types)
- [ ] Abstracts
  - [ ] Field access
  - [ ] Method calls
  - [ ] Member declaration
- [ ] Enums
  - [ ] Enum is a class
    - [ ] Constructors are subclasses
  - [ ] Pattern match using polymorphism
- [ ] Structs
- [ ] Inlining (the Haxe way)
- [ ] ???
- [ ] Profit

## Standard Library
- [ ] Delegates (God, I hate Microsoft)
  - [ ] Probably make func pointers abstracts that cast to/from respective delegate if possible...
    - [ ] I guess also they need a base class... `haxe.lang.Function`
- [ ] Native arrays (e.g. `cs.system.Object[]` -> `cs.NativeArray<cs.system.Object>`)
- [ ] Complex pattern matching
- [ ] ???
- [ ] Profit