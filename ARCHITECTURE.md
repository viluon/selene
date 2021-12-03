
For what it's worth, I'm scribbling down a few "architectural" insights I often forget about LMS.

### Inheritance hierarchy for language extensions

It goes like this:

- the high-level thing is whatever defines the core primitives of the lang extension, i.e. the types/functions/methods you get to use in DSL code. In js.scala, this is e.g. [`EventOps`](https://github.com/js-scala/js-scala/blob/0b5771853a35a37d011f0ddebf770fdcd7fc8bc1/core/src/main/scala/scala/js/language/dom/EventOps.scala). It only depends on `Rep` from `Base`, nothing else.
- the middle thing is the extension to the AST, which defines new nodes to represent the operations of the language extension. In js.scala, this would be [`EventOpsExp`](https://github.com/js-scala/js-scala/blob/0b5771853a35a37d011f0ddebf770fdcd7fc8bc1/core/src/main/scala/scala/js/exp/dom/EventOpsExp.scala). It depends on the core of the extension (`EventOps`) and the LMS AST core (`BaseExp`/`EffectExp`) and defines `case class`es `extend`ing `Def`.
  - AST extensions are responsible for specifying the graph properties of the nodes they introduce, such as the set of symbols bound in a node or the frequencies of symbol accesses (for code motion). See `WhileExp` or `FunctionsExp` for examples.
  - it's important to specify summaries when calling `reflectEffect()`. These define the dependencies of an expression as well as its side effects and are considered during code motion and other evaluation order -dependent transformations. The default `Simple()` summary is often imprecise.
  - optimisations are usually defined as AST extensions, which are at the right level of abstraction to access both high-level DSL information (they can literally invoke the DSL methods, and often do, for eager optimisation) and the sea-of-nodes IR graph.
- the "low-level" thing is the codegen for the new AST nodes. In js.scala, this would be [`GenEventOps`](https://github.com/js-scala/js-scala/blob/0b5771853a35a37d011f0ddebf770fdcd7fc8bc1/core/src/main/scala/scala/js/gen/js/dom/GenEventOps.scala). It depends on LMS codegen utilities (like `GenEffect`), narrows the type of its IR to the extension's AST (`EventOpsExp` in our running example), and pattern-matches on the AST nodes within when overriding `emitNode`.
  - codegen is simply a traversal that prints code. At this point, all the heavy lifting has already been done on the higher levels.

The codegen is tied together in a trait like [`GenDom`](https://github.com/js-scala/js-scala/blob/0b5771853a35a37d011f0ddebf770fdcd7fc8bc1/core/src/main/scala/scala/js/gen/js/dom/GenDom.scala#L5) or `CCCodegen`. This has to override the IR type to be a supertype of all the codegen mixins' IRs. Otherwise, we'd run into an error like

> overriding value `IR` in trait `FooGen` of type `FooExp`; value `IR` in trait `BarGen` of type `BarExp` has incompatible type

See the `EmbeddedControls` trait for an overview of what constructs are specially treated by the compiler.

### Roadmap

I'd like to get some example CC applications working. Event handling should now be possible thanks to unboxed tuple support.

- get some interactive FRP examples working
- come up with an inlining transformation to cut down on the number of locals
