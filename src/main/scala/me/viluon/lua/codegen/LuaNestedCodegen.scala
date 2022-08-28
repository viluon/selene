package me.viluon.lua.codegen

import me.viluon.lua.codegen.lowLevel.LLStmtMixin

import scala.lms.common.EffectExp
import scala.lms.internal.GenericNestedCodegen

// FIXME copied verbatim from js.scala
trait LuaNestedCodegen extends DummyNestedCodegen with LuaCoreCodegen with QuoteGen {
  self: LLStmtMixin =>
  val IR: EffectExp

  import IR._

  override def quote(x: Exp[Any]) = x match { // TODO: quirk!
    case Sym(-1) => sys.error("Sym(-1) not supported")
    case _ => super.quote(x)
  }

  /**
   * Taken from [[scala.lms.internal.ScalaNestedCodegen]]
   */
  // emit forward decls for recursive vals
  override def traverseStmsInBlock[A](stms: List[Stm]): Unit = {
    recursive foreach emitForwardDef
    super.traverseStmsInBlock(stms)
  }

  def emitForwardDef(sym: Sym[Any]): Unit = {
    luaCode += LLLocal(sym)
  }

  // TODO: do we need this?
//  // special case for recursive vals
//  def emitValDef(sym: Sym[Any], rhs: Exp[Any]): Unit = {
//    if (recursive contains sym)
//      emitAssignment(sym, rhs) // we have a forward declaration above.
//    else
//      super.emitValDef(sym, l"$rhs")
//  }
}
