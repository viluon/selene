package me.viluon.lua.codegen

import scala.lms.common.{NumericOpsExp, PrimitiveOpsExp}

trait LuaNumericOpsGen extends LuaPrimitiveOpsGen {
  val IR: NumericOpsExp with PrimitiveOpsExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case NumericPlus(a, b) => emitValDef(sym, q"$a + $b")
    case NumericMinus(a, b) => emitValDef(sym, q"$a - $b")
    case NumericTimes(a, b) => emitValDef(sym, q"$a * $b")
    case NumericDivide(a, b) => emitValDef(sym, q"$a / $b")
    case _ => super.emitNode(sym, rhs)
  }
}
