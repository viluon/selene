package me.viluon.lua.codegen

import me.viluon.lua.ast.LuaStringExp

trait LuaStringGen extends LuaCoreCodegen {
  val IR: LuaStringExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case GlobalString() => emitValDef(sym, l"string")
    case StringChar(string) => emitValDef(sym, l"$string.char")
    case StringByte(string) => emitValDef(sym, l"$string.byte")
    case _ => super.emitNode(sym, rhs)
  }
}
