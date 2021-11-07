package me.viluon.lua.codegen

import scala.lms.common.EffectExp
import scala.lms.internal.GenericNestedCodegen

// FIXME copied verbatim from js.scala
trait LuaNestedCodegen extends GenericNestedCodegen with LuaBaseCodegen with QuoteGen {
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
    stream.println(q"local $sym")
  }

  // special case for recursive vals
  override def emitValDef(sym: Sym[Any], rhs: String): Unit = {
    if (recursive contains sym)
      stream.println(q"$sym = $rhs") // we have a forward declaration above.
    else
      super.emitValDef(sym, rhs)
  }
}
