package me.viluon.lua.codegen

import scala.lms.common.VariablesExp

trait LuaVariableGen extends LuaEffectGen with QuoteGen {
  val IR: VariablesExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case ReadVar(Variable(a)) => emitValDef(sym, q"$a")
    case NewVar(init) => emitValDef(sym.asInstanceOf[Sym[Variable[Any]]], q"$init")
    case Assign(Variable(a), b) => emitAssignment(lowerExp(a), LLExpr(q"$b", syms(b)))
    case VarPlusEquals(Variable(a), b) => emitAssignment(lowerExp(a), LLExpr(q"$a + $b", syms(a) ++ syms(b)))
    case VarMinusEquals(Variable(a), b) => emitAssignment(lowerExp(a), LLExpr(q"$a - $b", syms(a) ++ syms(b)))
    case _ => super.emitNode(sym, rhs)
  }
}
