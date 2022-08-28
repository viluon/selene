package me.viluon.lua.ast

import me.viluon.lua.lang.LuaMath

import scala.lms.common.FunctionsExp
import scala.reflect.SourceContext

trait LuaMathExp extends LuaMath with FunctionsExp with LuaUnpackExp with LuaFunctionUtils {
  case class LuaPow[T: Typ : Numeric](base: Exp[T], exp: Exp[T]) extends Def[T]
  case class MathSin(api: Exp[MathAPI]) extends Def[Double => Double]
  case class MathMax(api: Exp[MathAPI]) extends Def[((Double, Double)) => Double]
  case class MathMin(api: Exp[MathAPI]) extends Def[((Double, Double)) => Double]
  case class GlobalMath() extends Def[MathAPI]

  override def pow[T: Typ : Numeric](base: Exp[T], exp: Exp[T])(implicit pos: SourceContext): Exp[T] = (base, exp) match {
    case (Const(b), Const(e)) => Const(scala.math.pow(
      implicitly[Numeric[T]].toDouble(b), implicitly[Numeric[T]].toDouble(e)
    ))(manifestTyp).asInstanceOf[Exp[T]]
    case _ => reflectEffect(LuaPow(base, exp), Pure())
  }

  implicit class MathOps(api: Exp[MathAPI]) {
    def sin(x: Exp[Double])(implicit pos: SourceContext): Exp[Double] = x match {
      case Const(k) => Const(scala.math.sin(k))(manifestTyp)
      case _ => math_sin(implicitly)(api)(x)
    }
    def max(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext): Exp[Double] = (x, y) match {
      case (Const(k), Const(l)) => Const(scala.math.max(k, l))(manifestTyp)
      case _ => math_max(implicitly)(api)(x, y)
    }
    def min(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext): Exp[Double] = (x, y) match {
      case (Const(k), Const(l)) => Const(scala.math.min(k, l))(manifestTyp)
      case _ => math_min(implicitly)(api)(x, y)
    }
  }

  override def math(implicit pos: SourceContext): Exp[MathAPI] = toAtom(GlobalMath())(manifestTyp, implicitly)

  override def math_sin(implicit pos: SourceContext): Exp[MathAPI] => Exp[Double] => Exp[Double] =
    api => pureFun(MathSin(api))(manifestTyp, manifestTyp, implicitly)
  override def math_max(implicit pos: SourceContext): Exp[MathAPI] => Exp[(Double, Double)] => Exp[Double] =
    api => pureFun(MathMax(api))(manifestTyp, manifestTyp, implicitly)
  override def math_min(implicit pos: SourceContext): Exp[MathAPI] => Exp[(Double, Double)] => Exp[Double] =
    api => pureFun(MathMin(api))(manifestTyp, manifestTyp, implicitly)
}
