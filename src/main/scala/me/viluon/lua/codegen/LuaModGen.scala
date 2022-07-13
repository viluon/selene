package me.viluon.lua.codegen

import me.viluon.lua.ast.LuaModExp

trait LuaModGen extends LuaBaseCodegen {
  val IR: LuaModExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case Mod(x, y) => emitValDef(sym, q"$x % $y")
    case _ => super.emitNode(sym, rhs)
  }
}