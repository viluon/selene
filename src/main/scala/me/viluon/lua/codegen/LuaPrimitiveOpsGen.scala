package me.viluon.lua.codegen

import scala.lms.common.PrimitiveOpsExp

trait LuaPrimitiveOpsGen extends BaseGen with QuoteGen {
  val IR: PrimitiveOpsExp

  import IR._

  // FIXME: not all primitive types are supported by Lua (e.g. integers aren't)
  // FIXME: untranslated JS
  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case ObjDoubleParseDouble(s) => emitValDef(sym, l"parseFloat($s)")
    case ObjDoublePositiveInfinity() => emitValDef(sym, l"Infinity")
    case ObjDoubleNegativeInfinity() => emitValDef(sym, l"-Infinity")
    case ObjDoubleMinValue() => emitValDef(sym, l"Number.MIN_VALUE")
    case ObjDoubleMaxValue() => emitValDef(sym, l"Number.MAX_VALUE")
    case DoubleFloatValue(lhs) => emitValDef(sym, l"$lhs")
    case ObjIntegerParseInt(s) => emitValDef(sym, l"parseInt($s, 10)")
    case ObjIntMaxValue() => emitValDef(sym, l"Number.MAX_VALUE")
    case ObjIntMinValue() => emitValDef(sym, l"Number.MIN_VALUE")
    case IntDivide(lhs, rhs) => emitValDef(sym, l"math.floor($lhs / $rhs)")
    case IntMod(lhs, rhs) => emitValDef(sym, l"$lhs % $rhs")
    case IntBinaryOr(lhs, rhs) => emitValDef(sym, l"$lhs | $rhs")
    case IntBinaryAnd(lhs, rhs) => emitValDef(sym, l"$lhs & $rhs")
    case IntBinaryXor(lhs, rhs) => emitValDef(sym, l"$lhs ^ $rhs")
    case IntDoubleValue(lhs) => emitValDef(sym, l"$lhs")
    case IntFloatValue(lhs) => emitValDef(sym, l"$lhs")
    case IntBitwiseNot(lhs) => emitValDef(sym, l"~$lhs")
    case IntToLong(lhs) => emitValDef(sym, l"$lhs")
    case IntToFloat(lhs) => emitValDef(sym, l"$lhs")
    case IntToDouble(lhs) => emitValDef(sym, l"$lhs")
    case FloatToDouble(lhs) => emitValDef(sym, l"$lhs")
    case LongBinaryOr(lhs, rhs) => emitValDef(sym, l"$lhs | $rhs")
    case LongBinaryAnd(lhs, rhs) => emitValDef(sym, l"$lhs & $rhs")
    case LongShiftLeft(lhs, rhs) => emitValDef(sym, l"$lhs << $rhs")
    case LongShiftRightUnsigned(lhs, rhs) => emitValDef(sym, l"$lhs >>> $rhs")
    case LongToInt(lhs) => emitValDef(sym, l"$lhs")
    case IntPlus(lhs, rhs) => emitValDef(sym, l"$lhs + $rhs")
    case IntTimes(lhs, rhs) => emitValDef(sym, l"$lhs * $rhs")
    case IntMinus(lhs, rhs) => emitValDef(sym, l"$lhs - $rhs")
    case DoublePlus(lhs, rhs) => emitValDef(sym, l"$lhs + $rhs")
    case DoubleTimes(lhs, rhs) => emitValDef(sym, l"$lhs * $rhs")
    case DoubleMinus(lhs, rhs) => emitValDef(sym, l"$lhs - $rhs")
    case DoubleDivide(lhs, rhs) => emitValDef(sym, l"$lhs / $rhs")
    case _ => super.emitNode(sym, rhs)
  }
}
