package me.viluon.lua.codegen

import scala.lms.common.PrimitiveOpsExp

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
    case IntDivide(lhs, rhs) => emitValDef(sym, q"math.floor($lhs / $rhs)")
    case IntMod(lhs, rhs) => emitValDef(sym, q"$lhs % $rhs")
    case IntBinaryOr(lhs, rhs) => emitValDef(sym, q"$lhs | $rhs")
    case IntBinaryAnd(lhs, rhs) => emitValDef(sym, q"$lhs & $rhs")
    case IntBinaryXor(lhs, rhs) => emitValDef(sym, q"$lhs ^ $rhs")
    case IntDoubleValue(lhs) => emitValDef(sym, q"$lhs")
    case IntFloatValue(lhs) => emitValDef(sym, q"$lhs")
    case IntBitwiseNot(lhs) => emitValDef(sym, q"~$lhs")
    case IntToLong(lhs) => emitValDef(sym, q"$lhs")
    case IntToFloat(lhs) => emitValDef(sym, q"$lhs")
    case IntToDouble(lhs) => emitValDef(sym, q"$lhs")
    case FloatToDouble(lhs) => emitValDef(sym, q"$lhs")
    case LongBinaryOr(lhs, rhs) => emitValDef(sym, q"$lhs | $rhs")
    case LongBinaryAnd(lhs, rhs) => emitValDef(sym, q"$lhs & $rhs")
    case LongShiftLeft(lhs, rhs) => emitValDef(sym, q"$lhs << $rhs")
    case LongShiftRightUnsigned(lhs, rhs) => emitValDef(sym, q"$lhs >>> $rhs")
    case LongToInt(lhs) => emitValDef(sym, q"$lhs")
    case IntPlus(lhs, rhs) => emitValDef(sym, q"$lhs + $rhs")
    case IntTimes(lhs, rhs) => emitValDef(sym, q"$lhs * $rhs")
    case IntMinus(lhs, rhs) => emitValDef(sym, q"$lhs - $rhs")
    case DoublePlus(lhs, rhs) => emitValDef(sym, q"$lhs + $rhs")
    case DoubleTimes(lhs, rhs) => emitValDef(sym, q"$lhs * $rhs")
    case DoubleMinus(lhs, rhs) => emitValDef(sym, q"$lhs - $rhs")
    case DoubleDivide(lhs, rhs) => emitValDef(sym, q"$lhs / $rhs")
    case _ => super.emitNode(sym, rhs)
  }
}
