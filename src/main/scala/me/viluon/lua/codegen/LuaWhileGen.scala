package me.viluon.lua.codegen

import scala.lms.common.{BaseGenWhile, WhileExp}

trait LuaWhileGen extends LuaEffectGen with QuoteGen {
  val IR: WhileExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case While(Block(Const(c)), body) =>
      luaCode += LLWhile(LLExpr(c.toString, Nil))
      emitBlock(body)
      luaCode += LLEnd()
    case While(cond, body) =>
      val cond_fun = fresh[Boolean]
      luaCode += LLLocal(cond_fun, Some(LLFunctionHeader(Nil)))
      emitBlock(cond)
      luaCode += LLReturn(lowerExp(getBlockResult(cond)))
      luaCode += LLEnd()

      luaCode += LLWhile(LLExpr(q"$cond_fun()", syms(cond_fun)))
      emitBlock(body)
      luaCode += LLEnd()
    case _ => super.emitNode(sym, rhs)
  }
}
