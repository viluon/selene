package me.viluon.lua.codegen

import scala.lms.common.{BaseGenFunctions, TupledFunctionsExp}

trait LuaFunctionGen extends BaseGenFunctions with LuaEffectGen with QuoteGen {
  val IR: TupledFunctionsExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case Lambda(_, UnboxedTuple(args), body) =>
      emitValDef(sym, "function" + args.map(quote).mkString("(", ", ", ")"))
      emitFunctionBody(body)
    case Lambda(_, arg, body) =>
      emitValDef(sym, q"function($arg)")
      emitFunctionBody(body)
    case Apply(fun, UnboxedTuple(args)) =>
      emitValDef(sym, q"$fun" + args.map(quote).mkString("(", ", ", ")"))
    case Apply(fun, arg) =>
      emitValDef(sym, q"$fun($arg)")
    case _ => super.emitNode(sym, rhs)
  }
}
