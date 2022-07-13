package me.viluon.lua.codegen

import scala.lms.common.BooleanOpsExp

trait LuaBooleanOpsGen extends LuaBaseCodegen {
  val IR: BooleanOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case BooleanNegate(b) => emitValDef(sym, q"not $b")
    case BooleanAnd(a, b) => emitValDef(sym, q"$a and $b")
    case BooleanOr(a, b) => emitValDef(sym, q"$a or $b")
    case _ => super.emitNode(sym, rhs)
  }
}
