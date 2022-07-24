package me.viluon.lua.ast

import scala.lms.common.{BaseExp, Equal, StringOpsExp}
import scala.reflect.SourceContext

trait LuaStringOpsExp extends BaseExp with StringOpsExp with Equal {
  implicit class StringOps(str: Exp[String]) {
    def isEmpty(implicit pos: SourceContext): Exp[Boolean] = str match {
      case Const(s) => Const(s.isEmpty)(manifestTyp)
      case _ => str == ""
    }
  }
}
