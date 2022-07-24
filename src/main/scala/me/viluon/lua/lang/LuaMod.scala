package me.viluon.lua.lang

import scala.lms.common.Base
import scala.reflect.SourceContext

trait LuaMod extends Base {
  implicit class LuaModOps[T: Numeric : Typ](x: Rep[T]) {
    def %(y: Rep[T])(implicit ctx: SourceContext): Rep[T] = infix_mod(x, y)
  }
  def infix_mod[T: Numeric : Typ](x: Rep[T], y: Rep[T])(implicit pos: SourceContext): Rep[T]
}
