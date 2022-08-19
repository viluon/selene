package me.viluon.lua.lang

import scala.language.higherKinds
import scala.lms.common.{Base, BooleanOps, IfThenElse, LiftBoolean, Record, StructOps}

trait LuaOption extends Base { self: StructOps with BooleanOps with LiftBoolean with IfThenElse =>
  // TODO move the interface here
  type Option[+T] <: Rep[scala.Option[T]]
}
