package me.viluon.lua.codegen

import scala.lms.common.StringOpsExp

trait LuaStringOpsGen extends BaseGen with QuoteGen {
  val IR: StringOpsExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case StringPlus(s1, s2) => emitValDef(sym, l"$s1 .. $s2")
    case StringContains(s1, s2) => emitValDef(sym, l"$s1:contains($s2)")
    case StringToDouble(s) => emitValDef(sym, l"tonumber($s)")
    case StringLength(s) => emitValDef(sym, l"#$s")
    case StringCharAt(s, i) => emitValDef(sym, l"$s:sub($i, $i)")
    case _ => super.emitNode(sym, rhs)
  }
}
