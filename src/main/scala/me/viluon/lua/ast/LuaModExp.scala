package me.viluon.lua.ast

import me.viluon.lua.lang.LuaMod

import scala.lms.common.EffectExp
import scala.reflect.SourceContext

trait LuaModExp extends LuaMod with EffectExp {
  case class Mod(x: Exp[Double], y: Exp[Double]) extends Def[Double]

  override def %(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext): Exp[Double] = (x, y) match {
    // reference: https://www.lua.org/manual/5.1/manual.html#2.5.1
    case (Const(x), Const(y)) => Const(x - (x / y).floor * y)(manifestTyp)
    case _ => reflectEffect(Mod(x, y), Read(syms(x) ++ syms(y)))(manifestTyp, implicitly)
  }
}
