package me.viluon.lua.computercraft.gen

import me.viluon.lua.codegen.{LuaEffectGen, QuoteGen}
import me.viluon.lua.computercraft.ast.TermExp

trait TermGen extends LuaEffectGen with QuoteGen {
  val IR: TermExp
  import IR._

  override def quote(x: Exp[Any]): String = x match {
    case Const(c: Colours.Colour) => quote(c.id)
    case ColourToInt(c) => quote(c)
    case _ => super.quote(x)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case GlobalTerm() => emitValDef(sym, "term")
    case TermWrite(t) => emitValDef(sym, q"$t.write")
    case TermSetCursorPos(t) => emitValDef(sym, q"$t.setCursorPos")
    case TermSetTextColour(t) => emitValDef(sym, q"$t.setTextColour")
    case TermSetBackgroundColour(t) => emitValDef(sym, q"$t.setBackgroundColour")
    case TermGetSize(t) => emitValDef(sym, q"{ $t.getSize() }")
    case TermClear(t) => emitValDef(sym, q"$t.clear")
    case UnsafeCoerce(src) => emitValDef(sym, q"$src")
    case _ => super.emitNode(sym, rhs)
  }
}
