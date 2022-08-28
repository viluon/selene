package me.viluon.lua.codegen

import me.viluon.lua.codegen.lowLevel.LLStmtMixin

import scala.lms.common.{BaseGenIfThenElse, IfThenElseExp}

trait LuaIfThenElseGen extends LuaEffectGen with QuoteGen { self: LLStmtMixin =>
  val IR: IfThenElseExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case IfThenElse(cond, thn, els) if sym.tp <:< typ[Unit] =>
      luaCode += LLIf(l"$cond")
      emitBlock(thn)
      emitElse(els)
      luaCode += LLEnd()
    case IfThenElse(cond, thn, els) =>
      luaCode += LLLocal(sym)
      luaCode += LLIf(l"$cond")
      emitBlock(thn)
      emitAssignment(sym, getBlockResult(thn))
      emitElse(els, emitAssignment(sym, getBlockResult(els)))
      luaCode += LLEnd()
    case _ => super.emitNode(sym, rhs)
  }

  private def emitElse(els: Block[Any], post: => Unit = ()): Unit = els match {
    case Block(Const(())) => ()
    case _ =>
      luaCode += LLElse()
      emitBlock(els)
      post
  }
}
