package me.viluon.lua.codegen

import scala.lms.common.MiscOpsExp

trait LuaMiscOpsGen extends BaseGen with QuoteGen {
  val IR: MiscOpsExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case PrintLn(s) => emitValDef(sym, l"print($s)")
    case Print(s) => emitValDef(sym, l"io.write($s)")
    case Error(s) => emitValDef(sym, l"error($s)")
    case _ => super.emitNode(sym, rhs)
  }
}
