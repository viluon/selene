package me.viluon.lua.codegen

import scala.lms.common.ArrayOpsExp

trait LuaArrayOpsGen extends LuaEffectGen {
  val IR: ArrayOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case ArrayApply(arr, n) => emitValDef(sym, l"$arr[$n]")
    case ArrayFromSeq(xs) =>
      emitValDef(sym, LLExpr(xs.map(quote).mkString("{ ", ", ", " }"), xs.flatMap(syms).toList))
    case ArrayUpdate(arr, i, x) => emitAssignment(l"$arr[$i]", lowerExp(x))
    case ArrayNew(size) => emitValDef(sym, l"{}") // TODO
    case ArrayLength(arr) => emitValDef(sym, l"#$arr")
    case _ => super.emitNode(sym, rhs)
  }
}
