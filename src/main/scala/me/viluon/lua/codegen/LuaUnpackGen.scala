package me.viluon.lua.codegen

import me.viluon.lua.ast.LuaUnpackExp

trait LuaUnpackGen extends LuaBaseCodegen {
  val IR: LuaUnpackExp
  import IR._

  override def quote(x: Exp[Any]): String = x match {
    case UnboxedSym(realSym, components) =>
      components.map(quote).map(s => q"unbx_${realSym.id}_$s").mkString(", ")
    case _ => super.quote(x)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case Unpack(x) => emitValDef(sym, x.map(quote).mkString("unpack(", ", ", ")"))
    case _ => super.emitNode(sym, rhs)
  }
}
