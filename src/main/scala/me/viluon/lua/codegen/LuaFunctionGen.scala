package me.viluon.lua.codegen

import scala.lms.common.{BaseGenFunctions, TupledFunctionsExp}

trait LuaFunctionGen extends LuaEffectGen with QuoteGen {
  val IR: TupledFunctionsExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case Lambda(_, UnboxedTuple(args), body) =>
      emitValDef(sym, LLExpr.fromSeq(args, "function(", ", ", ")"))
      emitFunctionBody(body)
    case Lambda(_, arg, body) =>
      emitValDef(sym, l"function($arg)")
      emitFunctionBody(body)
    case Apply(fun, UnboxedTuple(args)) =>
      emitValDef(sym, LLExpr.fromSeqUnsafe(fun :: "(" :: args, "" , ", ", ")"))
    case Apply(fun, arg) =>
      emitValDef(sym, l"$fun($arg)")
    case _ => super.emitNode(sym, rhs)
  }
}
