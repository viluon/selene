package me.viluon.lua.codegen

import scala.lms.common.ObjectOpsExp

trait LuaObjOpsGen extends LuaBaseCodegen {
  val IR: ObjectOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case ObjectToString(obj) => emitValDef(sym, q"tostring($obj)")
    case _ => super.emitNode(sym, rhs)
  }
}
