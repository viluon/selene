package me.viluon.lua.lang

import scala.lms.common.Base
import scala.reflect.SourceContext

trait LuaMod extends Base {
  def %(x: Rep[Double], y: Rep[Double])(implicit pos: SourceContext): Rep[Double]
}
