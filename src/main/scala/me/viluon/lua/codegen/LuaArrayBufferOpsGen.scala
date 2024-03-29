package me.viluon.lua.codegen

import me.viluon.lua.ast.LuaArrayBufferOpsExp
import me.viluon.lua.codegen.lowLevel.LLStmtMixin

import scala.lms.common.BaseGenArrayBufferOps

// FIXME: most codegen traits ignore the BaseGen... traits
trait LuaArrayBufferOpsGen extends LuaCoreCodegen { self: LLStmtMixin =>
  val IR: LuaArrayBufferOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case ArrayBufferToArray(arr) => emitValDef(sym, l"$arr")
    case ArrayBufferNew(xs) => emitValDef(sym, LLExpr.fromSeq(xs))
    case ArrayBufferAppend(arr, x) => emitAssignment(l"$arr[#$arr + 1]", l"$x")
    case _ => super.emitNode(sym, rhs)
  }
}
