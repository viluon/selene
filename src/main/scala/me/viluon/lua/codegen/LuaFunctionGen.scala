package me.viluon.lua.codegen

import scala.lms.common.{BaseGenFunctions, TupledFunctionsExp}

trait LuaFunctionGen extends LuaEffectGen with QuoteGen {
  val IR: TupledFunctionsExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case Lambda(_, UnboxedTuple(args), body) =>
      val uses = args.flatMap(syms)
      emitValDef(sym, LLExpr("function" + args.map(quote).mkString("(", ", ", ")"), uses))
      emitFunctionBody(body)
    case Lambda(_, arg, body) =>
      emitValDef(sym, l"function($arg)")
      emitFunctionBody(body)
    case Apply(fun, UnboxedTuple(args)) =>
      val uses = args.flatMap(syms) ++ syms(fun)
      emitValDef(sym, LLExpr(q"$fun" + args.map(quote).mkString("(", ", ", ")"), uses))
    case Apply(fun, arg) =>
      emitValDef(sym, l"$fun($arg)")
    case _ => super.emitNode(sym, rhs)
  }
}
