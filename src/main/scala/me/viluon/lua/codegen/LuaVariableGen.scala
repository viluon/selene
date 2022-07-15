package me.viluon.lua.codegen

import scala.lms.common.VariablesExp

trait LuaVariableGen extends LuaEffectGen with QuoteGen {
  val IR: VariablesExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case ReadVar(Variable(a)) => emitValDef(sym, l"$a")
    case NewVar(init) => emitValDef(sym.asInstanceOf[Sym[Variable[Any]]], l"$init")
    case Assign(Variable(a), b) => emitAssignment(lowerExp(a), l"$b")
    case VarPlusEquals(Variable(a), b) => emitAssignment(lowerExp(a), l"$a + $b")
    case VarMinusEquals(Variable(a), b) => emitAssignment(lowerExp(a), l"$a - $b")
    case _ => super.emitNode(sym, rhs)
  }
}
