package me.viluon.lua.codegen

import me.viluon.lua.LuaScalaExp

/**
 * [[LuaCodegen]] combines codegen functionality for features of [[LuaScalaExp]].
 */
trait LuaCodegen
  extends LuaEffectGen
    with LuaNumericOpsGen
    with LuaPrimitiveOpsGen
    with LuaOrderingOpsGen
    with LuaIfThenElseGen
    with LuaWhileGen
    with LuaVariableGen
    with LuaFunctionGen
    with LuaStructGen
    with LuaObjOpsGen
    with LuaTupleGen
    with LuaEqualGen
    with LuaMiscOpsGen
    with LuaStringOpsGen
    with LuaArrayOpsGen {
  val IR: LuaScalaExp
}
