package me.viluon.lua.computercraft.ast

import me.viluon.lua.LuaScalaExp
import me.viluon.lua.computercraft.lang.Term

import scala.lms.common.EffectExp
import scala.reflect.SourceContext

trait TermExp extends Term with LuaScalaExp {
  import Colour.Colour

  case class GlobalTerm() extends Def[TermLike]

  case class TermSetCursorPos(t: Exp[TermLike], x: Exp[Int], y: Exp[Int]) extends Def[Unit]
  case class TermWrite(t: Exp[TermLike], str: Exp[String]) extends Def[Unit]
  case class TermSetTextColour(t: Exp[TermLike], c: Exp[Colour]) extends Def[Unit]
  case class TermSetBackgroundColour(t: Exp[TermLike], c: Exp[Colour]) extends Def[Unit]

  implicit val termLikeManifest = ManifestTyp(implicitly[Manifest[TermLike]])
  implicit val colourManifest = ManifestTyp(implicitly[Manifest[Colour]])

  override def globalTerm: Exp[TermLike] = reflectEffect(GlobalTerm())

  override def termLike_setCursorPos(t: Exp[TermLike], x: Exp[Int], y: Exp[Int]): Exp[Unit] = reflectEffect(TermSetCursorPos(t, x, y))
  override def termLike_write(t: Exp[TermLike], str: Exp[String]): Exp[Unit] = reflectEffect(TermWrite(t, str))
  override def termLike_setTextColour(t: Exp[TermLike], c: Exp[Colour]): Exp[Unit] = reflectEffect(TermSetTextColour(t, c))
  override def termLike_setBackgroundColour(t: Exp[TermLike], c: Exp[Colour]): Exp[Unit] = reflectEffect(TermSetBackgroundColour(t, c))
}
