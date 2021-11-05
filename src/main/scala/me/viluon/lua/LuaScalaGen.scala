package me.viluon.lua

trait LuaScalaGen
  extends LuaEffectGen
  with LuaNumericOpsGen
  with LuaPrimitiveOpsGen
  with LuaOrderingOpsGen
  with LuaIfThenElseGen
  with LuaEqualGen
  with LuaMiscOpsGen
  with LuaStringOpsGen {
  val IR: LuaScalaExp
}
