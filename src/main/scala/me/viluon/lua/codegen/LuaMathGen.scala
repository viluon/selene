package me.viluon.lua.codegen

import me.viluon.lua.ast.LuaMathExp

trait LuaMathGen extends LuaBaseCodegen {
  val IR: LuaMathExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case GlobalMath() => emitValDef(sym, "math")
    case MathSin(math) => emitValDef(sym, q"$math.sin")
    case LuaPow(base, exp) => emitValDef(sym, q"$base ^ $exp")
    case _ => super.emitNode(sym, rhs)
  }
}
