package me.viluon.lua.codegen

import scala.lms.common.PrimitiveOpsExp

trait LuaPrimitiveOpsGen extends BaseGen with QuoteGen {
  val IR: PrimitiveOpsExp

  import IR._

  // FIXME: not all primitive types are supported by Lua (e.g. integers aren't in 5.1)
  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case ObjDoubleParseDouble(s) => ???
    case ObjDoublePositiveInfinity() => ???
    case ObjDoubleNegativeInfinity() => ???
    case ObjDoubleMinValue() => ???
    case ObjDoubleMaxValue() => ???
    case DoubleFloatValue(lhs) => emitValDef(sym, l"$lhs")
    case ObjIntegerParseInt(s) => ???
    case ObjIntMaxValue() => ???
    case ObjIntMinValue() => ???
    case IntDivide(lhs, rhs) => ???
    case IntMod(lhs, rhs) => emitValDef(sym, l"$lhs % $rhs")
    case IntBinaryOr(lhs, rhs) => ???
    case IntBinaryAnd(lhs, rhs) => ???
    case IntBinaryXor(lhs, rhs) => ???
    case IntDoubleValue(lhs) => emitValDef(sym, l"$lhs")
    case IntFloatValue(lhs) => emitValDef(sym, l"$lhs")
    case IntBitwiseNot(lhs) => ???
    case IntToLong(lhs) => emitValDef(sym, l"$lhs")
    case IntToFloat(lhs) => emitValDef(sym, l"$lhs")
    case IntToDouble(lhs) => emitValDef(sym, l"$lhs")
    case FloatToDouble(lhs) => emitValDef(sym, l"$lhs")
    case LongBinaryOr(lhs, rhs) => ???
    case LongBinaryAnd(lhs, rhs) => ???
    case LongShiftLeft(lhs, rhs) => ???
    case LongShiftRightUnsigned(lhs, rhs) => ???
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
