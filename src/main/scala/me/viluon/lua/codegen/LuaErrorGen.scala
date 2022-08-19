package me.viluon.lua.codegen

import me.viluon.lua.ast.LuaErrorExp

import scala.lms.common.{BaseExp, StringOps}

trait LuaErrorGen extends LuaCoreCodegen {
  val IR: LuaErrorExp with BaseExp with StringOps
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case LuaErrorFn() => emitValDef(sym, l"error")
    case _ => super.emitNode(sym, rhs)
  }
}
