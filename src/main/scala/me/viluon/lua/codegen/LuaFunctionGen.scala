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
      val commaSeparated =
        args.flatMap(arg => arg :: ", " :: Nil)
          .inits.drop(1).toList.headOption.toList.flatten
      emitValDef(sym, LLExpr.fromSeqUnsafe(fun :: "(" :: commaSeparated, "" , "", ")"))
    case Apply(fun, arg) =>
      emitValDef(sym, l"$fun($arg)")
    case _ => super.emitNode(sym, rhs)
  }
}
