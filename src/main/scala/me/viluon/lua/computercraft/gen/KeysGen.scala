package me.viluon.lua.computercraft.gen

import me.viluon.lua.codegen.{LuaEffectGen, QuoteGen}
import me.viluon.lua.computercraft.ast.KeysExp

trait KeysGen extends LuaEffectGen with QuoteGen {
  val IR: KeysExp
  import IR._

  override def quote(x: IR.Exp[Any]): String = x match {
    case Const(k: Keys.Key) => quote(k.id)
    case KeyToInt(k) => quote(k)
    case _ => super.quote(x)
  }
}
