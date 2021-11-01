package me.viluon.lua

import scala.lms.common._

trait LuaScalaExp extends LuaScala
  with NumericOpsExp
  with OrderingOpsExp
  with EqualExp
  with IfThenElseExp
  with WhileExp
  with BooleanOpsExp
  with VariablesExp
  with PrimitiveOpsExp
  with StringOpsExp
  with MiscOpsExp
