package me.viluon.lua.codegen

import scala.lms.common.TupledFunctionsExp

trait LuaTupleGen extends BaseGen with QuoteGen {
  val IR: TupledFunctionsExp

  import IR._

  override def quote(x: IR.Exp[Any]): String = x match {
    case UnboxedTuple(xs) => "{ " + xs.foldLeft((1, ""))({
      case ((n, acc), exp) => (n + 1, acc + q"_$n = $exp, ")
    })._2 + "}"
    case _ => super.quote(x)
  }
}
