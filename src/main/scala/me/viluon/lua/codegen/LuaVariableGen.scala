package me.viluon.lua.codegen

import scala.lms.common.VariablesExp

trait LuaVariableGen extends LuaEffectGen with QuoteGen {
  val IR: VariablesExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case ReadVar(Variable(a)) => emitValDef(sym, q"$a")
    case NewVar(init) => emitValDef(sym.asInstanceOf[Sym[Variable[Any]]], q"$init")
    case Assign(Variable(a), b) => stream.println(q"$a = $b")
    case VarPlusEquals(Variable(a), b) => stream.println(q"$a = $a + $b")
    case VarMinusEquals(Variable(a), b) => stream.println(q"$a = $a - $b")
    case _ => super.emitNode(sym, rhs)
  }
}
