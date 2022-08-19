package me.viluon.lua.ast

import scala.lms.common.{BaseExp, Equal, LiftString, LiftVariables, OrderingOps, Variables, While}
import scala.reflect.SourceContext

trait LuaStringOpsExp extends BaseExp {
  self: LuaStringAPIExp
    with Equal
    with While
    with OrderingOps
    with lms.common.StringOps
    with LiftVariables =>

  implicit class StringOps(str: Exp[String]) {
    def isEmpty(implicit pos: SourceContext): Exp[Boolean] = str match {
      case Const(s) => Const(s.isEmpty)(manifestTyp)
      case _ => str == ""
    }

    def foldLeft[A: Typ](init: Rep[A])(f: (Rep[A], Rep[Char]) => Rep[A])(implicit pos: SourceContext): Rep[A] = {
      val len = string.length(str)
      var i = 0
      var acc = init
      while (i < len) {
        acc = f(acc, str.charAt(i))
        i += 1
      }
      acc
    }
  }
}
