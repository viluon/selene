package me.viluon.lua.codegen

import scala.lms.common.SeqOpsExp

trait LuaSeqOpsGen extends LuaEffectGen {
  val IR: SeqOpsExp
  import IR._

  override def emitNode(sym: IR.Sym[Any], rhs: IR.Def[Any]): Unit = rhs match {
    case SeqApply(xs, i) => emitValDef(sym, l"$xs[$i]")
    case _ => super.emitNode(sym, rhs)
  }
}
