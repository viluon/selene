package me.viluon.lua.codegen

import scala.lms.common.EqualExp

trait LuaEqualGen extends BaseGen with QuoteGen {
  val IR: EqualExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case Equal(a, b) => emitValDef(sym, l"$a == $b")
    case NotEqual(a, b) => emitValDef(sym, l"$a ~= $b")
    case _ => super.emitNode(sym, rhs)
  }
}
