
For what it's worth, I'm scribbling down a few "architectural" insights I often forget about LMS.

### Inheritance hierarchy for language extensions

It goes like this:

- the high-level thing is whatever defines the core primitives of the lang extension. In js.scala, this is e.g. [`EventOps`](https://github.com/js-scala/js-scala/blob/0b5771853a35a37d011f0ddebf770fdcd7fc8bc1/core/src/main/scala/scala/js/language/dom/EventOps.scala). It only depends on `Rep` from `Base`, nothing else.
- the middle thing is the extension to the AST, which defines the new nodes to represent the operations of the language extension. In js.scala, this would be [`EventOpsExp`](https://github.com/js-scala/js-scala/blob/0b5771853a35a37d011f0ddebf770fdcd7fc8bc1/core/src/main/scala/scala/js/exp/dom/EventOpsExp.scala). It depends on the core of the extension (`EventOps`) and the LMS AST core (`BaseExp`/`EffectExp`) and defines `case class`es `extend`ing `Def`.
- the "low-level" thing is the codegen for the new AST nodes. In js.scala, this would be [`GenEventOps`](https://github.com/js-scala/js-scala/blob/0b5771853a35a37d011f0ddebf770fdcd7fc8bc1/core/src/main/scala/scala/js/gen/js/dom/GenEventOps.scala). It depends on LMS codegen utilities (like `GenEffect`), narrows the type of its IR to the extension's AST (`EventOpsExp` in our running example), and pattern-matches on the AST nodes within when overriding `emitNode`.

The codegen is tied together in a trait like [`GenDom`](https://github.com/js-scala/js-scala/blob/0b5771853a35a37d011f0ddebf770fdcd7fc8bc1/core/src/main/scala/scala/js/gen/js/dom/GenDom.scala#L5) or `CCCodegen`. This has to override the IR type to be a supertype of all the codegen mixins' IRs. Otherwise, we'd run into an error like

> overriding value `IR` in trait `FooGen` of type `FooExp`; value `IR` in trait `BarGen` of type `BarExp` has incompatible type

See the `EmbeddedControls` trait for an overview of what constructs are specially treated by the compiler.
