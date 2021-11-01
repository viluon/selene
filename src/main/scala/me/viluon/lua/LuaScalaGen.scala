package me.viluon.lua

trait LuaScalaGen
  extends LuaEffectGen
  with LuaNumericOpsGen
  with LuaPrimitiveOpsGen
  with LuaMiscOpsGen
  with LuaStringOpsGen {
  val IR: LuaScalaExp
}
