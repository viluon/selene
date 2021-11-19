package me.viluon.lua.computercraft.gen

import me.viluon.lua.codegen.{LuaEffectGen, QuoteGen}
import me.viluon.lua.computercraft.ast.TermExp

trait TermGen extends LuaEffectGen with QuoteGen {
  val IR: TermExp
  import IR._

  override def quote(x: Exp[Any]): String = x match {
    case Const(c: Colour.Colour) => quote(c.id)
    case _ => super.quote(x)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case GlobalTerm() => emitValDef(sym, "term")
    case TermWrite(t, str) => emitValDef(sym, q"$t.write($str)")
    case TermSetTextColour(t, c) => emitValDef(sym, q"$t.setTextColour($c)")
    case TermSetBackgroundColour(t, c) => emitValDef(sym, q"$t.setBackgroundColour($c)")
    case _ => super.emitNode(sym, rhs)
  }
}
