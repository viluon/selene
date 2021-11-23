package me.viluon.lua.codegen

import scala.lms.common.ArrayOpsExp

trait LuaArrayOpsGen extends LuaEffectGen {
  val IR: ArrayOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case ArrayApply(arr, n) => emitValDef(sym, q"$arr[$n]")
    case ArrayFromSeq(xs) => emitValDef(sym, xs.map(quote).mkString("{ ", ", ", " }"))
    case _ => super.emitNode(sym, rhs)
  }
}
