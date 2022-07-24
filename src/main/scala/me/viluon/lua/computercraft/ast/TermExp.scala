package me.viluon.lua.computercraft.ast

import me.viluon.lua.LuaScalaExp
import me.viluon.lua.ast.LuaFunctionUtils
import me.viluon.lua.computercraft.lang.Term

import scala.reflect.SourceContext

trait TermExp extends Term with LuaScalaExp with LuaFunctionUtils {
  import Colours.Colour

  implicit class TermLikeOps(t: Exp[TermLike]) {
    def setCursorPos(x: Exp[Int], y: Exp[Int])(implicit pos: SourceContext): Exp[Unit] = setCursorPos((x, y))(pos)
    def setCursorPos(pos: Exp[(Int, Int)])(implicit ctx: SourceContext): Exp[Unit] = termLike_setCursorPos(ctx)(t)(pos)
    def write(txt: Exp[String])(implicit pos: SourceContext): Exp[Unit] = termLike_write(pos)(t)(txt)
    def setTextColour(c: Exp[Colour])(implicit pos: SourceContext): Exp[Unit] = termLike_setTextColour(pos)(t)(c.toCC)
    def setBackgroundColour(c: Exp[Colour])(implicit pos: SourceContext): Exp[Unit] = termLike_setBackgroundColour(pos)(t)(c.toCC)
    def clear()(implicit pos: SourceContext): Exp[Unit] = termLike_clear(pos)(t)(())
    def getSize()(implicit pos: SourceContext): LuaUnboxedTuple[(Exp[Int], Exp[Int])] = termLike_getSize(pos)(t)(())
  }

  implicit class ColourOps(c: Exp[Colour]) {
    def toInt(implicit pos: SourceContext): Exp[Int] = c match {
      case Const(x) => x.id
      case _ => ColourToInt(c)
    }

    def toCC(implicit pos: SourceContext): Exp[Int] = c match {
      case Const(x) => scala.math.pow(2.0, x.id.toDouble).toInt
      case _ => pow(2, c.toInt)
    }
  }

  implicit class DoubleToColourOp(x: Exp[Double]) {
    def toColour(implicit pos: SourceContext): Exp[Colour] =
      reflectEffect(UnsafeCoerce(x)(doubleTyp, manifestTyp[Colour]), Read(syms(x)))
  }

  implicit def liftColour(c: Colour)(implicit pos: SourceContext): Exp[Colour] = unit(c)

  // TODO: move elsewhere
  case class UnsafeCoerce[S: Typ, T: Typ](src: Exp[S]) extends Def[T]

  case class GlobalTerm() extends Def[TermAPI]

  case class TermSetCursorPos(t: Exp[TermLike]) extends Def[((Int, Int)) => Unit]
  case class TermWrite(t: Exp[TermLike]) extends Def[String => Unit]
  case class TermSetTextColour(t: Exp[TermLike]) extends Def[Int => Unit]
  case class TermSetBackgroundColour(t: Exp[TermLike]) extends Def[Int => Unit]
  case class TermClear(t: Exp[TermLike]) extends Def[Unit => Unit]
  case class TermGetSize(t: Exp[TermLike]) extends Def[Unit => LuaUnboxedTuple[(Int, Int)]]

  case class ColourToInt(c: Exp[Colour]) extends Exp[Int]

  implicit val termLikeManifest: Typ[TermLike] = manifestTyp
  implicit val colourManifest: Typ[Colour] = manifestTyp

  override def term(implicit pos: SourceContext): Exp[TermAPI] = toAtom(GlobalTerm())(manifestTyp, implicitly)

  override def termLike_setCursorPos(implicit pos: SourceContext): Exp[TermLike] => Exp[(Int, Int)] => Exp[Unit] =
    t => impureFun(TermSetCursorPos(t))
  override def termLike_write(implicit pos: SourceContext): Exp[TermLike] => Exp[String] => Exp[Unit] =
    t => impureFun(TermWrite(t))
  override def termLike_setTextColour(implicit pos: SourceContext): Exp[TermLike] => Exp[Int] => Exp[Unit] =
    t => impureFun(TermSetTextColour(t))
  override def termLike_setBackgroundColour(implicit pos: SourceContext): Exp[TermLike] => Exp[Int] => Exp[Unit] =
    t => impureFun(TermSetBackgroundColour(t))
  override def termLike_clear(implicit pos: SourceContext): Exp[TermLike] => Exp[Unit] => Exp[Unit] =
    t => impureFun(TermClear(t))
  override def termLike_getSize(implicit pos: SourceContext): Exp[TermLike] => Exp[Unit] => LuaUnboxedTuple[(Exp[Int], Exp[Int])] =
    t => _ => {
      // TODO make unboxed tuples more ergonomic
      val f = impureFun(TermGetSize(t))
      val LuaUnboxedTuple((w: Exp[Int], h: Exp[Int])) = f(()).asInstanceOf[Exp[LuaUnboxedTuple[(Any, Any)]]]
      LuaUnboxedTuple(w -> h)(manifestTyp)
    }
}
