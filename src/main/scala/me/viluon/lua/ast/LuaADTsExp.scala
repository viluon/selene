package me.viluon.lua.ast

import me.viluon.lua.lang.{LuaADTs, LuaError}

import scala.lms.common.{BaseExp, BooleanOps, Equal, Functions, IfThenElse, LiftBoolean, LiftString, StringOps, StructOps, TupleOps}

trait LuaADTsExp extends LuaADTs { self: BaseExp with IfThenElse with BooleanOps
  with StructOps with TupleOps with StringOps with LuaError with LiftString
  with Equal with Functions =>
  override implicit val anyTyp: Typ[Any] = manifestTyp[Any]
  override implicit def eitherTyp[L, R]: Typ[Either[L, R]] =
    manifestTyp[Either[L, R]](Manifest.classType(classOf[Either[L, R]]))
  override def adtManifestTyp[T](implicit m: Manifest[T]): Typ[T] = manifestTyp
}
