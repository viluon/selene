package me.viluon.lua

import me.viluon.lua.lang.{LuaADTs, LuaDynamics, LuaError, LuaMap, LuaMath, LuaMod, LuaStringAPI, LuaUnpack}

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
  with TupleOps
  with TupledFunctions
  with PrimitiveOps
  with MiscOps
  with SeqOps
  with ArrayOps
  with ArrayBufferOps
  with ListOps
  with RangeOps
  with ImplicitOps
  with IOOps
  with LuaUnpack
  with LuaStringAPI
  with LuaMap
  with LuaMod
  with LuaMath
  with LuaDynamics
  with LuaError

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
  with LuaADTs
