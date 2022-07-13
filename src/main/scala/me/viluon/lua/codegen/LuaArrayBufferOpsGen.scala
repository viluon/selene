package me.viluon.lua.codegen

import me.viluon.lua.ast.LuaArrayBufferOpsExp

import scala.lms.common.BaseGenArrayBufferOps

// FIXME: most codegen traits ignore the BaseGen... traits
trait LuaArrayBufferOpsGen extends LuaBaseCodegen with BaseGenArrayBufferOps {
  val IR: LuaArrayBufferOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case ArrayBufferToArray(arr) => emitValDef(sym, q"$arr")
    case ArrayBufferNew(xs) => emitValDef(sym, xs.map(quote).mkString("{", ", ", "}"))
    case _ => super.emitNode(sym, rhs)
  }
}
