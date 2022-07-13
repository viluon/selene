package me.viluon.lua.computercraft.lang

import scala.lms.common.Base
import scala.reflect.SourceContext

trait Term extends Base {
  object Colours extends Enumeration {
    type Colour = Value
    val White    : Colour = Value(0)
    val Orange   : Colour = Value(1)
    val Magenta  : Colour = Value(2)
    val LightBlue: Colour = Value(3)
    val Yellow   : Colour = Value(4)
    val Lime     : Colour = Value(5)
    val Pink     : Colour = Value(6)
    val Grey     : Colour = Value(7)
    val LightGrey: Colour = Value(8)
    val Cyan     : Colour = Value(9)
    val Purple   : Colour = Value(10)
    val Blue     : Colour = Value(11)
    val Brown    : Colour = Value(12)
    val Green    : Colour = Value(13)
    val Red      : Colour = Value(14)
    val Black    : Colour = Value(15)
  }
  import Colours.Colour

  class TermLike
  trait TermAPI extends TermLike

  def term(implicit pos: SourceContext): Rep[TermAPI]

  def termLike_setCursorPos(implicit pos: SourceContext): Rep[TermLike] => Rep[(Int, Int)] => Rep[Unit]
  def termLike_write(implicit pos: SourceContext): Rep[TermLike] => Rep[String] => Rep[Unit]
  def termLike_setTextColour(implicit pos: SourceContext): Rep[TermLike] => Rep[Int] => Rep[Unit]
  def termLike_setBackgroundColour(implicit pos: SourceContext): Rep[TermLike] => Rep[Int] => Rep[Unit]
  def termLike_clear(implicit pos: SourceContext): Rep[TermLike] => Rep[Unit] => Rep[Unit]
  def termLike_getSize(implicit pos: SourceContext): Rep[TermLike] => Rep[Array[Int]]
}
