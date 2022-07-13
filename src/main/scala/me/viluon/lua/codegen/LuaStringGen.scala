package me.viluon.lua.codegen

import me.viluon.lua.ast.LuaStringExp

trait LuaStringGen extends LuaBaseCodegen {
  val IR: LuaStringExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case GlobalString() => emitValDef(sym, "string")
    case StringChar(string) => emitValDef(sym, q"$string.char")
    case StringByte(string) => emitValDef(sym, q"$string.byte")
    case _ => super.emitNode(sym, rhs)
  }
}
