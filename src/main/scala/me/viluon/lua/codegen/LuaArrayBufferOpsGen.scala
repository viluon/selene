package me.viluon.lua.codegen

import me.viluon.lua.ast.LuaArrayBufferOpsExp

import scala.lms.common.BaseGenArrayBufferOps

// FIXME: most codegen traits ignore the BaseGen... traits
trait LuaArrayBufferOpsGen extends LuaCoreCodegen {
  val IR: LuaArrayBufferOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case ArrayBufferToArray(arr) => emitValDef(sym, l"$arr")
    case ArrayBufferNew(xs) =>
      emitValDef(sym, LLExpr(xs.map(quote).mkString("{", ", ", "}"), xs.flatMap(syms).toList))
    case ArrayBufferAppend(arr, x) => emitAssignment(l"$arr[#$arr + 1]", lowerExp(x))
    case _ => super.emitNode(sym, rhs)
  }
}
