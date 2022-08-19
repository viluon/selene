package me.viluon.lua.lang

import scala.lms.common.{Base, StringOps}
import scala.reflect.SourceContext

trait LuaError extends Base { self: StringOps =>
  def error: Rep[String] => Rep[Nothing]
}
