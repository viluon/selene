package me.viluon.lua.computercraft.lang

import scala.lms.common.Base

trait Term extends Base {
  object Colour extends Enumeration {
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
  import Colour._

  class TermLike

  implicit class TermLikeOps(t: Rep[TermLike]) {
    def setCursorPos(x: Rep[Int], y: Rep[Int]): Rep[Unit] = termLike_setCursorPos(t, x, y)
    def write(str: Rep[String]): Rep[Unit] = termLike_write(t, str)
    def setTextColour(c: Rep[Colour]): Rep[Unit] = termLike_setTextColour(t, c)
    def setBackgroundColour(c: Rep[Colour]): Rep[Unit] = termLike_setBackgroundColour(t, c)
  }

  def globalTerm: Rep[TermLike]

  def termLike_setCursorPos(t: Rep[TermLike], x: Rep[Int], y: Rep[Int]): Rep[Unit]
  def termLike_write(t: Rep[TermLike], str: Rep[String]): Rep[Unit]
  def termLike_setTextColour(t: Rep[TermLike], c: Rep[Colour]): Rep[Unit]
  def termLike_setBackgroundColour(t: Rep[TermLike], c: Rep[Colour]): Rep[Unit]
}
