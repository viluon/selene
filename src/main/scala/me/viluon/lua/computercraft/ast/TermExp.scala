package me.viluon.lua.computercraft.ast

import me.viluon.lua.LuaScalaExp
import me.viluon.lua.ast.LuaFunctionUtils
import me.viluon.lua.computercraft.lang.Term

import scala.reflect.SourceContext

trait TermExp extends Term with LuaScalaExp with LuaFunctionUtils {
  import Colours.Colour

  implicit class TermLikeOps(t: Exp[TermLike]) {
    def setCursorPos(x: Exp[Int], y: Exp[Int])(implicit pos: SourceContext): Exp[Unit] = setCursorPos((x, y))(pos) // not a real error
    def setCursorPos(pos: Exp[(Int, Int)])(implicit ctx: SourceContext): Exp[Unit] = termLike_setCursorPos(ctx)(t)(pos)
    def write(txt: Exp[String])(implicit pos: SourceContext): Exp[Unit] = termLike_write(pos)(t)(txt)
    def setTextColour(c: Exp[Colour])(implicit pos: SourceContext): Exp[Unit] = termLike_setTextColour(pos)(t)(c)
    def setBackgroundColour(c: Exp[Colour])(implicit pos: SourceContext): Exp[Unit] = termLike_setBackgroundColour(pos)(t)(c)
    def clear()(implicit pos: SourceContext): Exp[Unit] = termLike_clear(pos)(t)(())
    def getSize()(implicit pos: SourceContext): Exp[Array[Int]] = termLike_getSize(pos)(t)
  }

  implicit def liftColour(c: Colour)(implicit pos: SourceContext): Rep[Colour] = unit(c)

  case class GlobalTerm() extends Def[TermAPI]

  case class TermSetCursorPos(t: Exp[TermLike]) extends Def[((Int, Int)) => Unit]
  case class TermWrite(t: Exp[TermLike]) extends Def[String => Unit]
  case class TermSetTextColour(t: Exp[TermLike]) extends Def[Colour => Unit]
  case class TermSetBackgroundColour(t: Exp[TermLike]) extends Def[Colour => Unit]
  case class TermClear(t: Exp[TermLike]) extends Def[Unit => Unit]
  case class TermGetSize(t: Exp[TermLike]) extends Def[Array[Int]]

  implicit val termLikeManifest = ManifestTyp(implicitly[Manifest[TermLike]])
  implicit val termApiManifest = ManifestTyp(implicitly[Manifest[TermAPI]])
  implicit val colourManifest = ManifestTyp(implicitly[Manifest[Colour]])

  override def term(implicit pos: SourceContext): Exp[TermAPI] = toAtom(GlobalTerm())

  override def termLike_setCursorPos(implicit pos: SourceContext): Exp[TermLike] => Exp[(Int, Int)] => Exp[Unit] =
    t => impureFun(TermSetCursorPos(t))
  override def termLike_write(implicit pos: SourceContext): Exp[TermLike] => Exp[String] => Exp[Unit] =
    t => impureFun(TermWrite(t))
  override def termLike_setTextColour(implicit pos: SourceContext): Exp[TermLike] => Exp[Colour] => Exp[Unit] =
    t => impureFun(TermSetTextColour(t))
  override def termLike_setBackgroundColour(implicit pos: SourceContext): Exp[TermLike] => Exp[Colour] => Exp[Unit] =
    t => impureFun(TermSetBackgroundColour(t))
  override def termLike_clear(implicit pos: SourceContext): Exp[TermLike] => Exp[Unit] => Exp[Unit] =
    t => impureFun(TermClear(t))
  override def termLike_getSize(implicit pos: SourceContext): Exp[TermLike] => Exp[Array[Int]] =
    t => reflectEffect(TermGetSize(t))
}
