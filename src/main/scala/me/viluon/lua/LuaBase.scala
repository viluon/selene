package me.viluon.lua

import scala.lms.common._

trait LuaBase extends Base
  with NumericOps
  with OrderingOps
  with Equal
  with IfThenElse
  with While
  with BooleanOps
  with StringOps
  with Variables
  with PrimitiveOps
  with MiscOps

trait LuaScala extends LuaBase
  with LiftVariables
  with LiftEquals
  with LiftNumeric
  with LiftString
  with LiftBoolean
  with LiftPrimitives
