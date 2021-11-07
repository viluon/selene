package me.viluon.lua.codegen

import scala.lms.common.EffectExp

trait LuaEffectGen extends LuaNestedCodegen with BaseGen {
  val IR: EffectExp
}
