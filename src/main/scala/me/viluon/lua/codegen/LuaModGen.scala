package me.viluon.lua.codegen

import me.viluon.lua.ast.LuaModExp

trait LuaModGen extends LuaCoreCodegen {
  val IR: LuaModExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case Mod(x, y) => emitValDef(sym, l"$x % $y")
    case _ => super.emitNode(sym, rhs)
  }
}
