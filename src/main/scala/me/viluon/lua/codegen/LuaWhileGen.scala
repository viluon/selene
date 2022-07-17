package me.viluon.lua.codegen

import scala.lms.common.{BaseGenWhile, WhileExp}

trait LuaWhileGen extends LuaEffectGen with QuoteGen {
  val IR: WhileExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case While(Block(Const(c)), body) =>
      val bodyCode = inScope(emitBlock(body))
      luaCode += LLWhile(LLExpr(_ => c.toString, Nil), bodyCode.size)
      luaCode ++= bodyCode
      luaCode += LLEnd()
    case While(cond, body) =>
      val cond_fun = fresh[Boolean]
      val condBodyCode = inScope(emitBlock(cond))
      luaCode += LLLocal(cond_fun, Some(LLFunctionHeader(Nil, condBodyCode.size + 1)))
      luaCode ++= condBodyCode
      luaCode += LLReturn(l"${getBlockResult(cond)}")
      luaCode += LLEnd()

      val bodyCode = inScope(emitBlock(body))
      luaCode += LLWhile(l"$cond_fun()", bodyCode.size)
      luaCode ++= bodyCode
      luaCode += LLEnd()
    case _ => super.emitNode(sym, rhs)
  }
}
