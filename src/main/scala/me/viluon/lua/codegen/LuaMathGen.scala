package me.viluon.lua.codegen

import me.viluon.lua.ast.LuaMathExp

trait LuaMathGen extends LuaCoreCodegen {
  val IR: LuaMathExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case GlobalMath() => emitValDef(sym, l"math")
    case MathSin(math) => emitValDef(sym, l"$math.sin")
    case LuaPow(base, exp) => emitValDef(sym, l"$base ^ $exp")
    case _ => super.emitNode(sym, rhs)
  }
}
