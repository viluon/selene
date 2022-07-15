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
      val components = collection.mutable.ArrayBuffer[Either[Int, String]](Right(
        StringContext.treatEscapes(pi.next())
      ))
      val uses = collection.mutable.ArrayBuffer[IR.Sym[Any]]()
      var arity = 0
      while (ai.hasNext) {
        components += (ai.next() match {
          case sym: IR.Sym[_] =>
            uses += sym
            Left(try arity finally arity += 1)
          case e: IR.Exp[_] => Right(quote(e))
          case a => Right(a.toString)
        })
        components += Right(StringContext.treatEscapes(pi.next()))
      }
      LLExpr(LLExpr.genericExprGen(arity, components.toList).andThen(_.mkString("")), uses.toSet.toList)
    }
  }
}
