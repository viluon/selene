package me.viluon.lua.codegen

import me.viluon.lua.ast.LuaTupledFunctionsExp

trait LuaTupleGen extends BaseGen with QuoteGen {
  val IR: LuaTupledFunctionsExp
  import IR._

  override def quote(x: Exp[Any]): String = x match {
    case UnboxedTuple(xs) => "{ " + xs.foldLeft((1, ""))({
      case ((n, acc), exp) => (n + 1, acc + q"_$n = $exp, ")
    })._2 + "}"
    case _ => super.quote(x)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case _ if sym.tp <:< pubManifestTyp[UnboxedTuple[Any]] =>
      super.emitNode(sym, rhs)
    case _ => super.emitNode(sym, rhs)
  }
}
