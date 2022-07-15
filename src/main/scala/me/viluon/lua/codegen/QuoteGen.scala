package me.viluon.lua.codegen

import me.viluon.lua.codegen.lowLevel.LLStmtOps

import scala.lms.internal.Expressions

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
trait QuoteGen extends DummyGen { self: LLStmtOps =>

  import language.implicitConversions

  trait Quote {
    def q(args: Any*): String
    def l(args: Any*): LLExpr
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

    override def l(args: Any*): LLExpr = {
      ctx.checkLengths(args)
      val pi = ctx.parts.iterator
      val ai = args.iterator
      val builder = new java.lang.StringBuilder(StringContext.treatEscapes(pi.next()))
      val uses = collection.mutable.ArrayBuffer[IR.Sym[Any]]()
      while (ai.hasNext) {
        builder.append(ai.next() match {
          case e: IR.Exp[_] =>
            uses ++= IR.syms(e)
            quote(e)
          case a => a
        })
        builder.append(StringContext.treatEscapes(pi.next()))
      }
      LLExpr(builder.toString, uses.toSet.toList)
    }
  }
}
