package me.viluon.lua

import scala.lms.common.{BaseExp, BaseGenIfThenElse, EffectExp, EqualExp, IfThenElseExp, MiscOpsExp, NumericOpsExp, OrderingOpsExp, PrimitiveOpsExp, StringOpsExp}
import scala.lms.internal.{GenericCodegen, GenericNestedCodegen}

trait Codegen extends GenericCodegen {

  import java.io.PrintWriter
  import IR._

  override def emitValDef(sym: Sym[Any], rhs: String): Unit = stream.println(s"local ${quote(sym)} = $rhs")

  override def quote(x: Exp[Any]): String = x match {
    case Const(()) => "nil"
    case _ => super.quote(x)
  }

  override def emitBlock(y: Block[Any]): Unit = {
    //    stream.println("do -- emitBlock - is this correct?")
    super.emitBlock(y)
    //    stream.println("end")
  }

  override def emitSource[A: Typ](args: List[IR.Sym[_]],
                                  body: Block[A],
                                  className: String,
                                  out: PrintWriter): List[(IR.Sym[Any], Any)] = {
    val argsStr = args.map(quote).mkString(", ")

    withStream(out) {
      stream.println("function " + (if (className.isEmpty) "" else className) + s"($argsStr)")
      emitBlock(body)
      val result = getBlockResult(body)
      if (!(result.tp <:< ManifestTyp(manifest[Unit]))) {
        stream.println(s"return ${quote(result)}")
      }

      stream.println("end")
      stream.flush()
    }

    getFreeDataBlock(body)
  }

  //  def emitExecution[A: Manifest](a: => Exp[A], out: PrintWriter): List[(Sym[Any], Any)] = {
  //    val b = reifyBlock(a)
  //    out.print("(")
  //    emitSource(Nil, b, "", out)
  //    out.println(")()")
  //    getFreeDataBlock(b)
  //  }

  override def emitAssignment(lhs: Sym[Any], rhs: String): Unit = {
    stream.println(s"${quote(lhs)} = $rhs")
  }
}

trait BaseGen extends Codegen {
  val IR: BaseExp
}

// FIXME copied verbatim from js.scala
trait NestedCodegen extends GenericNestedCodegen with Codegen {

  import IR._

  override def quote(x: Exp[Any]) = x match { // TODO: quirk!
    case Sym(-1) => sys.error("Sym(-1) not supported")
    case _ => super.quote(x)
  }
}

trait LuaEffectGen extends NestedCodegen with BaseGen {
  val IR: EffectExp
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

trait LuaOrderingOpsGen extends BaseGen with QuoteGen {
  val IR: OrderingOpsExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case OrderingGT(lhs, rhs) => emitValDef(sym, q"$lhs > $rhs")
    case OrderingLT(lhs, rhs) => emitValDef(sym, q"$lhs < $rhs")
    case OrderingGTEQ(lhs, rhs) => emitValDef(sym, q"$lhs >= $rhs")
    case OrderingLTEQ(lhs, rhs) => emitValDef(sym, q"$lhs <= $rhs")
    case OrderingEquiv(lhs, rhs) => emitValDef(sym, q"$lhs == $rhs")
    case _ => super.emitNode(sym, rhs)
  }
}

trait LuaEqualGen extends BaseGen with QuoteGen {
  val IR: EqualExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case Equal(a, b) => emitValDef(sym, q"$a == $b")
    case NotEqual(a, b) => emitValDef(sym, q"$a ~= $b")
    case _ => super.emitNode(sym, rhs)
  }
}

trait LuaIfThenElseGen extends BaseGenIfThenElse with LuaEffectGen with QuoteGen {
  val IR: IfThenElseExp

  import IR._

  override def emitNode(sym: IR.Sym[Any], rhs: IR.Def[Any]): Unit = rhs match {
    case IfThenElse(cond, thn, els) =>
      stream.println(q"local $sym")
      stream.println(q"if $cond then")
      emitBlock(thn)
      //      emitAssignment(sym, quote(getBlockResult(thn)))
      stream.println(q"$sym = ${getBlockResult(thn)}")
      stream.println("else")
      emitBlock(els)
      //      emitAssignment(sym, quote(getBlockResult(els)))
      stream.println(q"$sym = ${getBlockResult(els)}")
      stream.println("end")
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
