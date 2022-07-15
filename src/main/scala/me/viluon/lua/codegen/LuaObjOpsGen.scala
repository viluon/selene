package me.viluon.lua.codegen

import scala.lms.common.ObjectOpsExp

trait LuaObjOpsGen extends LuaCoreCodegen {
  val IR: ObjectOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case ObjectToString(obj) => emitValDef(sym, l"tostring($obj)")
    case _ => super.emitNode(sym, rhs)
  }
}
