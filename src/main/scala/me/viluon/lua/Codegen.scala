package me.viluon.lua

import scala.lms.common.{BaseExp, MiscOpsExp, NumericOpsExp, PrimitiveOpsExp, StringOpsExp}
import scala.lms.internal.GenericCodegen

trait Codegen extends GenericCodegen

trait BaseGen extends Codegen {
  val IR: BaseExp
}

trait LuaPrimitiveOpsGen extends BaseGen with QuoteGen {
  val IR: PrimitiveOpsExp

  import IR._

  // FIXME: not all primitive types are supported by Lua (e.g. integers aren't)
  // FIXME: untranslated JS
  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case ObjDoubleParseDouble(s) => emitValDef(sym, q"parseFloat($s)")
    case ObjDoublePositiveInfinity() => emitValDef(sym, "Infinity")
    case ObjDoubleNegativeInfinity() => emitValDef(sym, "-Infinity")
    case ObjDoubleMinValue() => emitValDef(sym, "Number.MIN_VALUE")
    case ObjDoubleMaxValue() => emitValDef(sym, "Number.MAX_VALUE")
    case DoubleFloatValue(lhs) => emitValDef(sym, q"$lhs")
    case ObjIntegerParseInt(s) => emitValDef(sym, q"parseInt($s, 10)")
    case ObjIntMaxValue() => emitValDef(sym, "Number.MAX_VALUE")
    case ObjIntMinValue() => emitValDef(sym, "Number.MIN_VALUE")
    case IntDivide(lhs, rhs) => emitValDef(sym, q"Math.floor($lhs / $rhs)")
    case IntMod(lhs, rhs) => emitValDef(sym, q"$lhs % $rhs")
    case IntBinaryOr(lhs, rhs) => emitValDef(sym, q"$lhs | $rhs")
    case IntBinaryAnd(lhs, rhs) => emitValDef(sym, q"$lhs & $rhs")
    case IntBinaryXor(lhs, rhs) => emitValDef(sym, q"$lhs ^ $rhs")
    case IntDoubleValue(lhs) => emitValDef(sym, q"$lhs")
    case IntFloatValue(lhs) => emitValDef(sym, q"$lhs")
    case IntBitwiseNot(lhs) => emitValDef(sym, q"~$lhs")
    case IntToLong(lhs) => emitValDef(sym, q"$lhs")
    case LongBinaryOr(lhs, rhs) => emitValDef(sym, q"$lhs | $rhs")
    case LongBinaryAnd(lhs, rhs) => emitValDef(sym, q"$lhs & $rhs")
    case LongShiftLeft(lhs, rhs) => emitValDef(sym, q"$lhs << $rhs")
    case LongShiftRightUnsigned(lhs, rhs) => emitValDef(sym, q"$lhs >>> $rhs")
    case LongToInt(lhs) => emitValDef(sym, q"$lhs")
    case IntPlus(lhs, rhs) => emitValDef(sym, q"$lhs + $rhs")
    case IntTimes(lhs, rhs) => emitValDef(sym, q"$lhs * $rhs")
    case IntMinus(lhs, rhs) => emitValDef(sym, q"$lhs - $rhs")
    case _ => super.emitNode(sym, rhs)
  }
}

trait LuaNumericOpsGen extends LuaPrimitiveOpsGen {
  val IR: NumericOpsExp with PrimitiveOpsExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case NumericPlus(a, b) => emitValDef(sym, q"$a + $b")
    case NumericMinus(a, b) => emitValDef(sym, q"$a - $b")
    case NumericTimes(a, b) => emitValDef(sym, q"$a * $b")
    case NumericDivide(a, b) => emitValDef(sym, q"$a / $b")
    case _ => super.emitNode(sym, rhs)
  }
}

trait LuaMiscOpsGen extends BaseGen with QuoteGen {
  val IR: MiscOpsExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case PrintLn(s) => emitValDef(sym, q"print($s)")
    case Print(s) => emitValDef(sym, q"io.write($s)")
    case Error(s) => emitValDef(sym, q"error($s)")
    case _ => super.emitNode(sym, rhs)
  }
}

trait LuaStringOpsGen extends BaseGen with QuoteGen {
  val IR: StringOpsExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case StringPlus(s1, s2) => emitValDef(sym, q"$s1 .. $s2")
    case StringContains(s1, s2) => emitValDef(sym, q"$s1:contains($s2)")
    case StringToDouble(s) => emitValDef(sym, q"tonumber($s)")
    case _ => super.emitNode(sym, rhs)
  }
}
