package me.viluon.lua.ast

import me.viluon.lua.lang.LuaMath

import scala.lms.common.FunctionsExp
import scala.reflect.SourceContext

trait LuaMathExp extends LuaMath with FunctionsExp with LuaUnpackExp with LuaFunctionUtils {
  case class LuaPow[T: Typ : Numeric](base: Exp[T], exp: Exp[T]) extends Def[T]
  case class MathSin(api: Exp[MathAPI]) extends Def[Double => Double]
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
  }

  override def math(implicit pos: SourceContext): Exp[MathAPI] = toAtom(GlobalMath())(manifestTyp, implicitly)

  override def math_sin(implicit pos: SourceContext): Exp[MathAPI] => Exp[Double] => Exp[Double] =
    api => pureFun(MathSin(api))(manifestTyp, manifestTyp, implicitly)
}
