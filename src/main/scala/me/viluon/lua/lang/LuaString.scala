package me.viluon.lua.lang

import scala.lms.common.Base
import scala.reflect.SourceContext

trait LuaString extends Base {
  trait StringAPI

  def string_byte(implicit pos: SourceContext): Rep[StringAPI] => Rep[String] => Rep[Int]
  def string_char(implicit pos: SourceContext): Rep[StringAPI] => Rep[Int] => Rep[String]

  def string(implicit pos: SourceContext): Rep[StringAPI]
}
