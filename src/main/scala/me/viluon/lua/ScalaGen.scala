package me.viluon.lua

import scala.lms.common._

trait ScalaGen extends ScalaGenEffect with ScalaGenNumericOps with ScalaGenOrderingOps
  with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenWhile with ScalaGenBooleanOps with ScalaGenStringOps
  with ScalaGenVariables with ScalaGenPrimitiveOps with ScalaGenMiscOps {
  val IR: LuaScalaExp

  import IR._

  override def quote(x: Exp[Any]) = x match {
    case Const('\n') if x.tp == typ[Char] => "'\\n'"
    case Const('\t') if x.tp == typ[Char] => "'\\t'"
    case Const(0) if x.tp == typ[Char] => "'\\0'"
    case _ => super.quote(x)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case IfThenElse(c, Block(Const(true)), Block(Const(false))) =>
      emitValDef(sym, quote(c))
    case PrintF(f: String, xs) =>
      emitValDef(sym, src"printf(${Const(f) :: xs})")
    case _ => super.emitNode(sym, rhs)
  }
}
