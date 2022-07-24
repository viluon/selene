package me.viluon.lua.codegen

import me.viluon.lua.ast.LuaDynamicsExp

trait LuaDynamicsGen extends LuaEffectGen {
  val IR: LuaDynamicsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case DynamicCall(receiver, method, args) =>
      ???
      emitValDef(sym, quote(receiver) + "." + method + args.map(quote).mkString("(", ",", ")"))
    case DynamicSelect(receiver, field) =>
      ???
      emitValDef(sym, quote(receiver) + "." + field)
    case DynamicUpdate(receiver, field, value) =>
      ???
      emitValDef(sym, quote(receiver) + "." + field + " = " + quote(value))
    case DynamicNew(constructor, args) =>
      ???
      emitValDef(sym, "new " + constructor + args.map(quote).mkString("(", ",", ")"))
    case DynamicInline(code) =>
      ???
      emitValDef(sym, code)
    case _ => super.emitNode(sym, rhs)
  }

  override def quote(x: Exp[Any]): String = x match {
    case DynamicExp(receiver) => quote(receiver)
    case _ => super.quote(x)
  }
}
