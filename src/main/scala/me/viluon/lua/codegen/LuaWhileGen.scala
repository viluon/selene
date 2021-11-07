package me.viluon.lua.codegen

import scala.lms.common.{BaseGenWhile, WhileExp}

trait LuaWhileGen extends BaseGenWhile with LuaEffectGen with QuoteGen {
  val IR: WhileExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case While(cond, body) =>
      emitValDef(sym, q"${Const(())}")
      val cond_fun = q"cond_$sym"
      stream.println(s"function $cond_fun()")
      emitBlock(cond)
      stream.println(q"return ${getBlockResult(cond)}")
      stream.println("end")
      stream.println(s"while $cond_fun() do")
      emitBlock(body)
      stream.println("end")
    case _ => super.emitNode(sym, rhs)
  }
}
