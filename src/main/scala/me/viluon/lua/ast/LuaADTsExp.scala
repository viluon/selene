package me.viluon.lua.ast

import me.viluon.lua.lang.{LuaADTs, LuaError}

import scala.lms.common.{BaseExp, BooleanOps, IfThenElse, StringOps, StructOps, TupleOps}

trait LuaADTsExp extends LuaADTs { self: BaseExp with IfThenElse with BooleanOps
  with StructOps with TupleOps with StringOps with LuaError =>
  override implicit def anyTyp: Typ[Any] = manifestTyp[Any]
}
