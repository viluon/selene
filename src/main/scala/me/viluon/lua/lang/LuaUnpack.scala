package me.viluon.lua.lang

import scala.lms.common.Base
import scala.reflect.SourceContext

trait LuaUnpack extends Base {
  type LuaUnboxedTuple[_]
  def unpack[T <: Product : Typ](arr: Rep[Array[_]])(implicit pos: SourceContext): Rep[LuaUnboxedTuple[T]]
}
