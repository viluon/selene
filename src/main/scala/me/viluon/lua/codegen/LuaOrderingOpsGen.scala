package me.viluon.lua.codegen

import scala.lms.common.OrderingOpsExp

trait LuaOrderingOpsGen extends BaseGen with QuoteGen {
  val IR: OrderingOpsExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case OrderingGT(lhs, rhs) => emitValDef(sym, l"$lhs > $rhs")
    case OrderingLT(lhs, rhs) => emitValDef(sym, l"$lhs < $rhs")
    case OrderingGTEQ(lhs, rhs) => emitValDef(sym, l"$lhs >= $rhs")
    case OrderingLTEQ(lhs, rhs) => emitValDef(sym, l"$lhs <= $rhs")
    case OrderingEquiv(lhs, rhs) => emitValDef(sym, l"$lhs == $rhs")
    case _ => super.emitNode(sym, rhs)
  }
}
