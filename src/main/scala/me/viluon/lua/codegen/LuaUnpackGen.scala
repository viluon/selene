package me.viluon.lua.codegen

import me.viluon.lua.ast.LuaUnpackExp

trait LuaUnpackGen extends LuaCoreCodegen {
  val IR: LuaUnpackExp
  import IR._

  override def quote(x: Exp[Any]): String = x match {
    case UnboxedSym(id, components) =>
      components.map(quote).mkString(q"--[[ unboxed[${components.size}] $id ]] ", ", ", "")
//    case LuaUnboxedTuple(t) =>
//      t.asInstanceOf[Product].productIterator.map(x => quote(x.asInstanceOf[Exp[Any]])).mkString(", ")
    case _ => super.quote(x)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case Unpack(x) => emitValDef(sym, l"unpack($x)")
    case DummyRead() => ()
    case _ => super.emitNode(sym, rhs)
  }
}
