package me.viluon.lua

// borrowed from https://github.com/js-scala/js-scala/blob/0b5771853a35a37d011f0ddebf770fdcd7fc8bc1/core/src/main/scala/scala/js/gen/QuoteGen.scala

import scala.lms.internal.GenericCodegen

/**
 * String interpolator aiming to simplify the implementation of code generators.
 * Basically, the following expression:
 * {{{
 *   q"foo ${bar} baz ${bah}"
 * }}}
 * Is equivalent to:
 * {{{
 *   "foo " + quote(bar) + " baz " + bah
 * }}}
 * Provided `bar` is a `Rep[_]` and `bah` is not.
 */
trait QuoteGen extends GenericCodegen {
  import language.implicitConversions

  trait Quote {
    def q(args: Any*): String
  }

  implicit def stringContextToQuote(ctx: StringContext): Quote = new Quote {
    def q(args: Any*): String = {
      ctx.checkLengths(args)
      val pi = ctx.parts.iterator
      val ai = args.iterator
      val builder = new java.lang.StringBuilder(StringContext.treatEscapes(pi.next()))
      while (ai.hasNext) {
        builder.append(ai.next() match {
          case e: IR.Exp[_] => quote(e)
          case a => a
        })
        builder.append(StringContext.treatEscapes(pi.next()))
      }
      builder.toString
    }
  }
}
