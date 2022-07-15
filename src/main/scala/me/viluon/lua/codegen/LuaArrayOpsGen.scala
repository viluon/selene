package me.viluon.lua.codegen

import scala.lms.common.ArrayOpsExp

trait LuaArrayOpsGen extends LuaEffectGen {
  val IR: ArrayOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case ArrayApply(arr, n) => emitValDef(sym, q"$arr[$n]")
    case ArrayFromSeq(xs) => emitValDef(sym, xs.map(quote).mkString("{ ", ", ", " }"))
    case ArrayUpdate(arr, i, x) => emitAssignment(LLExpr(q"$arr[$i]", syms(arr) ++ syms(i)), lowerExp(x))
    case ArrayNew(size) => emitValDef(sym, "{}") // TODO
    case ArrayLength(arr) => emitValDef(sym, q"#$arr")
    case _ => super.emitNode(sym, rhs)
  }
}
