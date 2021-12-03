package me.viluon.lua.lang

import scala.lms.common.Base
import scala.reflect.SourceContext

trait LuaUnpack extends Base {
  def unpack[T: Typ](arr: Array[Rep[Any]])(implicit pos: SourceContext): Rep[T]
}
