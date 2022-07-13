package me.viluon.lua.ast

import me.viluon.lua.lang.LuaString

import scala.lms.common.FunctionsExp
import scala.reflect.SourceContext

trait LuaStringExp extends LuaString with FunctionsExp with LuaFunctionUtils {
  case class StringByte(string: Exp[StringAPI]) extends Def[String => Int]
  case class StringChar(string: Exp[StringAPI]) extends Def[Int => String]

  case class GlobalString() extends Def[StringAPI]

  implicit class StringOps(api: Exp[StringAPI]) {
    def byte(str: Exp[String])(implicit pos: SourceContext): Exp[Int] = str match {
      case Const(ch) => Const(ch.charAt(0).toByte.toInt)(manifestTyp)
      case _ => string_byte(implicitly)(api)(str)
    }
    def char[T: Typ : Numeric](b: Exp[T])(implicit pos: SourceContext): Exp[String] = b match {
      // FIXME this requires a general solution (lack of a double/int distinction in Lua 5.1)
      case Const(x: Double) => char(Const(x.toInt)(manifestTyp))(manifestTyp, implicitly, pos)
      case Const(x: Int) => Const(x.toChar.toString)(manifestTyp)
      case _ => string_char(implicitly)(api)(b.asInstanceOf[Exp[Int]])
    }
  }

  override def string_byte(implicit pos: SourceContext): Exp[StringAPI] => Exp[String] => Exp[Int] =
    s => pureFun(StringByte(s))(manifestTyp, manifestTyp, implicitly)
  override def string_char(implicit pos: SourceContext): Exp[StringAPI] => Exp[Int] => Exp[String] =
    s => pureFun(StringChar(s))(manifestTyp, manifestTyp, implicitly)

  override def string(implicit pos: SourceContext): Exp[StringAPI] =
    toAtom(GlobalString())(manifestTyp, implicitly)
}
