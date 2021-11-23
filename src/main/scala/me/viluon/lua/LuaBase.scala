package me.viluon.lua

import scala.lms.common._

/**
 * The core of the Lua DSL.
 */
trait LuaBase extends Base
  with NumericOps
  with OrderingOps
  with Equal
  with IfThenElse
  with While
  with BooleanOps
  with StringOps
  with Variables
  with StructOps
  with ObjectOps
  with TupledFunctions
  with PrimitiveOps
  with MiscOps
  with SeqOps
  with ArrayOps

/**
 * Provides Lua DSL functionality based on [[LuaBase]] with lifting.
 */
trait LuaScala extends LuaBase
  with LiftVariables
  with LiftEquals
  with LiftNumeric
  with LiftString
  with LiftBoolean
  with LiftPrimitives
  with LiftArrays
