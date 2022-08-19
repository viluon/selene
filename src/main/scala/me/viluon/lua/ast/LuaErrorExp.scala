package me.viluon.lua.ast

import me.viluon.lua.lang.LuaError

import scala.lms.common.{BaseExp, StringOps}
import scala.reflect.SourceContext

trait LuaErrorExp extends LuaError { self: BaseExp with StringOps with LuaFunctionUtils =>
  case class LuaErrorFn() extends Def[String => Nothing]

  override def error: Rep[String] => Rep[Nothing] =
    pureFun(LuaErrorFn())(manifestTyp, manifestTyp, implicitly)
}
