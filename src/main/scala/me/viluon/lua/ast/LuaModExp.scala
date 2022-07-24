package me.viluon.lua.ast

import me.viluon.lua.lang.LuaMod

import scala.lms.common.EffectExp
import scala.reflect.SourceContext

trait LuaModExp extends LuaMod with EffectExp {
  case class Mod[T: Numeric : Typ](x: Exp[T], y: Exp[T]) extends Def[T]

  // reference: https://www.lua.org/manual/5.1/manual.html#2.5.1
  private def mod(x: Int, y: Int): Int = (x - (x / y).floor * y).toInt
  private def mod(x: Double, y: Double): Double = x - (x / y).floor * y

  override def infix_mod[T: Numeric : Typ](x: Exp[T], y: Exp[T])(implicit pos: SourceContext): Exp[T] = (x, y) match {
    case (Const(x: Int), Const(y: Int)) => Const(mod(x, y))(manifestTyp).asInstanceOf[Exp[T]]
    case (Const(x: Double), Const(y: Double)) => Const(mod(x, y))(manifestTyp).asInstanceOf[Exp[T]]
    case _ => reflectEffect(Mod(x, y), Read(syms(x) ++ syms(y)))
  }
}
