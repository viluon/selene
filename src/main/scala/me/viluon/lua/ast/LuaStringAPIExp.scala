package me.viluon.lua.ast

import me.viluon.lua.lang.LuaStringAPI

import scala.lms.common.FunctionsExp
import scala.reflect.SourceContext

trait LuaStringAPIExp extends LuaStringAPI with FunctionsExp with LuaUnpackExp with LuaFunctionUtils {
  case class StringByte(string: Exp[StringAPI]) extends Def[String => Int]
  case class StringChar(string: Exp[StringAPI]) extends Def[Int => String]
  case class StringLengthViaAPI(string: Exp[StringAPI]) extends Def[String => Int]
  case class StringRep(string: Exp[StringAPI]) extends Def[String => Int => String]
  case class StringReverse(string: Exp[StringAPI]) extends Def[String => String]
  case class StringSub(string: Exp[StringAPI]) extends Def[((String, Int, Int)) => String]

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
    def length(str: Exp[String])(implicit pos: SourceContext): Exp[Int] = str match {
      case Const(s) => Const(s.length)(manifestTyp)
      case _ => string_length(implicitly)(api)(str)
    }
    def sub(str: Exp[String], start: Exp[Int], end: Exp[Int])(implicit pos: SourceContext): Exp[String] = (str, start, end) match {
      case (Const(s), Const(start), Const(end)) => Const(s.substring(start, end))(manifestTyp)
      case _ => string_sub(implicitly)(api)(str, start, end)
    }
  }

  override def string_byte(implicit pos: SourceContext): Exp[StringAPI] => Exp[String] => Exp[Int] =
    s => pureFun(StringByte(s))
  override def string_char(implicit pos: SourceContext): Exp[StringAPI] => Exp[Int] => Exp[String] =
    s => pureFun(StringChar(s))
  override def string_length(implicit pos: SourceContext): Exp[StringAPI] => Exp[String] => Exp[Int] =
    s => pureFun(StringLengthViaAPI(s))
  override def string_rep(implicit pos: SourceContext): Exp[StringAPI] => Exp[String] => Exp[Int] => Exp[String] =
    s => pureFun2(StringRep(s))
  override def string_reverse(implicit pos: SourceContext): Exp[StringAPI] => Exp[String] => Exp[String] =
    s => pureFun(StringReverse(s))
  override def string_sub(implicit pos: SourceContext): Exp[StringAPI] => Exp[(String, Int, Int)] => Exp[String] =
    s => pureFun(StringSub(s))

  override def string(implicit pos: SourceContext): Exp[StringAPI] =
    toAtom(GlobalString())(manifestTyp, implicitly)
}
