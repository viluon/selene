package me.viluon.lua

/**
 * [[LuaScalaGen]] combines codegen functionality for features of [[LuaScalaExp]].
 */
trait LuaScalaGen
  extends LuaEffectGen
  with LuaNumericOpsGen
  with LuaPrimitiveOpsGen
  with LuaOrderingOpsGen
  with LuaIfThenElseGen
  with LuaWhileGen
  with LuaVariableGen
  with LuaFunctionGen
  with LuaEqualGen
  with LuaMiscOpsGen
  with LuaStringOpsGen {
  val IR: LuaScalaExp
}
