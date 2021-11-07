package me.viluon.lua.codegen

import scala.lms.common.{BaseGenIfThenElse, IfThenElseExp}

trait LuaIfThenElseGen extends BaseGenIfThenElse with LuaEffectGen with QuoteGen {
  val IR: IfThenElseExp

  import IR._

  override def emitNode(sym: IR.Sym[Any], rhs: IR.Def[Any]): Unit = rhs match {
    case IfThenElse(cond, thn, els) =>
      stream.println(q"local $sym")
      stream.println(q"if $cond then")
      emitBlock(thn)
      //      emitAssignment(sym, quote(getBlockResult(thn)))
      stream.println(q"$sym = ${getBlockResult(thn)}")
      stream.println("else")
      emitBlock(els)
      //      emitAssignment(sym, quote(getBlockResult(els)))
      stream.println(q"$sym = ${getBlockResult(els)}")
      stream.println("end")
    case _ => super.emitNode(sym, rhs)
  }
}
