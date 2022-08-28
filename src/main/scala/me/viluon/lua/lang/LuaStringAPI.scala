package me.viluon.lua.lang

import scala.lms.common.{Base, StringOps}
import scala.reflect.SourceContext

trait LuaStringAPI extends Base with StringOps {
  trait StringAPI

  def string_byte(implicit pos: SourceContext): Rep[StringAPI] => Rep[String] => Rep[Int]
  def string_char(implicit pos: SourceContext): Rep[StringAPI] => Rep[Int] => Rep[String]
  def string_length(implicit pos: SourceContext): Rep[StringAPI] => Rep[String] => Rep[Int]
  def string_rep(implicit pos: SourceContext): Rep[StringAPI] => Rep[String] => Rep[Int] => Rep[String]
  def string_reverse(implicit pos: SourceContext): Rep[StringAPI] => Rep[String] => Rep[String]
  def string_sub(implicit pos: SourceContext): Rep[StringAPI] => Rep[(String, Int, Int)] => Rep[String]

  def string(implicit pos: SourceContext): Rep[StringAPI]
}
