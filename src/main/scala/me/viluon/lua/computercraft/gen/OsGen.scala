package me.viluon.lua.computercraft.gen

import me.viluon.lua.codegen.{LuaEffectGen, QuoteGen}
import me.viluon.lua.computercraft.ast.OsExp

import scala.lms.common.StructExp

trait OsGen extends LuaEffectGen with QuoteGen {
  val IR: OsExp with StructExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case OsClock() => emitValDef(sym, "os.clock()")
    case OsPullEvent() => emitValDef(sym, "{ os.pullEvent() }")
    case OsQueueEvent(e) => emitValDef(sym, q"os.queueEvent(unpack($e))")
    case _ => super.emitNode(sym, rhs)
  }
}
