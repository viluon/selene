package me.viluon.lua.lang

import scala.lms.common.Base
import scala.reflect.SourceContext

trait LuaMath extends Base {
  def pow[T: Typ : Numeric](base: Rep[T], exp: Rep[T])(implicit pos: SourceContext): Rep[T]
  trait MathAPI
  def math(implicit pos: SourceContext): Rep[MathAPI]
  def math_sin(implicit pos: SourceContext): Rep[MathAPI] => Rep[Double] => Rep[Double]
}
