package me.viluon.lua.codegen

import me.viluon.lua.codegen.lowLevel.LLStmtMixin

import scala.lms.common.ArrayOpsExp

trait LuaArrayOpsGen extends LuaEffectGen { self: LLStmtMixin =>
  val IR: ArrayOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case ArrayApply(arr, n) => emitValDef(sym, l"$arr[$n]")
    case ArrayFromSeq(xs) => emitValDef(sym, LLExpr.fromSeq(xs))
    case ArrayUpdate(arr, i, x) => emitAssignment(l"$arr[$i]", l"$x")
    case ArrayNew(size) => emitValDef(sym, l"{}") // TODO
    case ArrayLength(arr) => emitValDef(sym, l"#$arr")
    case _ => super.emitNode(sym, rhs)
  }
}
