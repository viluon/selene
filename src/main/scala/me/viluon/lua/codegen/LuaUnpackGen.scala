package me.viluon.lua.codegen

import me.viluon.lua.ast.LuaUnpackExp

trait LuaUnpackGen extends LuaFatCodegen {
  val IR: LuaUnpackExp
  import IR._

  override def emitFatNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case Unpack(xs) => emitMultiValDef(sym, q"unpack($xs)")
    case _ => super.emitNode(sym, rhs)
  }
}
