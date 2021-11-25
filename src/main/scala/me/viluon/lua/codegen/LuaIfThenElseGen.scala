package me.viluon.lua.codegen

import scala.lms.common.{BaseGenIfThenElse, IfThenElseExp}

trait LuaIfThenElseGen extends BaseGenIfThenElse with LuaEffectGen with QuoteGen {
  val IR: IfThenElseExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case IfThenElse(cond, thn, els) if sym.tp <:< typ[Unit] =>
      stream.println(q"if $cond then")
      emitBlock(thn)
      emitElse(els)
      stream.println("end")
    case IfThenElse(cond, thn, els) =>
      stream.println(q"local $sym")
      stream.println(q"if $cond then")
      emitBlock(thn)
      stream.println(q"$sym = ${getBlockResult(thn)}")
      emitElse(els, stream.println(q"$sym = ${getBlockResult(els)}"))
      stream.println("end")
    case _ => super.emitNode(sym, rhs)
  }

  private def emitElse(els: Block[Any], post: => Unit = ()): Unit = els match {
    case Block(Const(())) => ()
    case _ =>
      stream.println("else")
      emitBlock(els)
      post
  }
}
