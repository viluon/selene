package me.viluon.lua.codegen

import scala.lms.internal.{Effects, Expressions, NestedBlockTraversal}

trait DummyNestedCodegen extends NestedBlockTraversal with DummyGen {
  val IR: Expressions with Effects
  import IR._

  abstract override def traverseStm(stm: Stm): Unit = super[DummyGen].traverseStm(stm)

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    //    case Read(s) =>
    //      emitValDef(sym, quote(s))
    case Reflect(s, u, effects) =>
      emitNode(sym, s)
    case Reify(s, u, effects) =>
    // just ignore -- effects are accounted for in emitBlock
    case _ => super.emitNode(sym, rhs)
  }

  case class NestedBlock(b: Block[Any])
  def nestedBlock(b: Block[Any]) = NestedBlock(b)
}