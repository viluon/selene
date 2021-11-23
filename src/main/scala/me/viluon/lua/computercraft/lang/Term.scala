package me.viluon.lua.computercraft.lang

import scala.lms.common.Base
import scala.reflect.SourceContext

trait Term extends Base {
  object Colours extends Enumeration {
    type Colour = Value
    val White    : Colour = Value(1)
    val Orange   : Colour = Value(2)
    val Magenta  : Colour = Value(4)
    val LightBlue: Colour = Value(8)
    val Yellow   : Colour = Value(16)
    val Lime     : Colour = Value(32)
    val Pink     : Colour = Value(64)
    val Grey     : Colour = Value(128)
    val LightGrey: Colour = Value(256)
    val Cyan     : Colour = Value(512)
    val Purple   : Colour = Value(1024)
    val Blue     : Colour = Value(2048)
    val Brown    : Colour = Value(4096)
    val Green    : Colour = Value(8192)
    val Red      : Colour = Value(16384)
    val Black    : Colour = Value(32768)
  }
  import Colours.Colour

  class TermLike
  trait TermAPI extends TermLike

  def globalTerm(implicit pos: SourceContext): Rep[TermAPI]

  def termLike_setCursorPos(implicit pos: SourceContext): Rep[TermLike] => Rep[(Int, Int)] => Rep[Unit]
  def termLike_write(implicit pos: SourceContext): Rep[TermLike] => Rep[String] => Rep[Unit]
  def termLike_setTextColour(implicit pos: SourceContext): Rep[TermLike] => Rep[Colour] => Rep[Unit]
  def termLike_setBackgroundColour(implicit pos: SourceContext): Rep[TermLike] => Rep[Colour] => Rep[Unit]
  def termLike_clear(implicit pos: SourceContext): Rep[TermLike] => Rep[Unit] => Rep[Unit]
  def termLike_getSize(implicit pos: SourceContext): Rep[TermLike] => Rep[Array[Int]]
}
