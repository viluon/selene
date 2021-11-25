package me.viluon.lua.computercraft.gen

import me.viluon.lua.codegen.{LuaEffectGen, QuoteGen}
import me.viluon.lua.computercraft.ast.OsExp

import scala.lms.common.StructExp

trait OsGen extends LuaEffectGen with QuoteGen {
  val IR: OsExp with StructExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case GlobalOs() => emitValDef(sym, "os")
    case OsClock(os) => emitValDef(sym, q"$os.clock")
    case OsPullEvent(os) => emitValDef(sym, q"$os.pullEvent")
    case OsQueueEvent(os) => emitValDef(sym, q"$os.queueEvent")
    case _ => super.emitNode(sym, rhs)
  }
}
