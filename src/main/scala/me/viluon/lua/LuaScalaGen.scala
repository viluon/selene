package me.viluon.lua

trait LuaScalaGen
  extends LuaNumericOpsGen
  with LuaPrimitiveOpsGen
  with LuaMiscOpsGen
  with LuaStringOpsGen {
  val IR: LuaScalaExp
}
