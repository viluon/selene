package me.viluon.lua

import scala.lms.common.{BaseExp, BaseGenFunctions, BaseGenIfThenElse, BaseGenWhile, EffectExp, EqualExp, FunctionsExp, IfThenElseExp, MiscOpsExp, NumericOpsExp, OrderingOpsExp, PrimitiveOpsExp, StringOpsExp, TupledFunctionsExp, VariablesExp, WhileExp}
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
trait LuaNestedCodegen extends GenericNestedCodegen with Codegen with QuoteGen {
  val IR: EffectExp

  import IR._

  override def quote(x: Exp[Any]) = x match { // TODO: quirk!
    case Sym(-1) => sys.error("Sym(-1) not supported")
    case _ => super.quote(x)
  }

  /**
   * Taken from [[scala.lms.internal.ScalaNestedCodegen]]
   */
  // emit forward decls for recursive vals
  override def traverseStmsInBlock[A](stms: List[Stm]): Unit = {
    recursive foreach emitForwardDef
    super.traverseStmsInBlock(stms)
  }

  def emitForwardDef(sym: Sym[Any]): Unit = {
    stream.println(q"local $sym")
  }

  // special case for recursive vals
  override def emitValDef(sym: Sym[Any], rhs: String): Unit = {
    if (recursive contains sym)
      stream.println(q"$sym = $rhs") // we have a forward declaration above.
    else
      super.emitValDef(sym, rhs)
  }
}

trait LuaEffectGen extends LuaNestedCodegen with BaseGen {
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

trait LuaWhileGen extends BaseGenWhile with LuaEffectGen with QuoteGen {
  val IR: WhileExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case While(cond, body) =>
      emitValDef(sym, q"${Const(())}")
      val cond_fun = q"cond_$sym"
      stream.println(s"function $cond_fun()")
      emitBlock(cond)
      stream.println(q"return ${getBlockResult(cond)}")
      stream.println("end")
      stream.println(s"while $cond_fun() do")
      emitBlock(body)
      stream.println("end")
    case _ => super.emitNode(sym, rhs)
  }
}

trait LuaVariableGen extends LuaEffectGen with QuoteGen {
  val IR: VariablesExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case ReadVar(Variable(a)) => emitValDef(sym, q"$a")
    case NewVar(init) => emitValDef(sym.asInstanceOf[Sym[Variable[Any]]], q"$init")
    case Assign(Variable(a), b) => stream.println(q"$a = $b")
    case VarPlusEquals(Variable(a), b) => stream.println(q"$a = $a + $b")
    case VarMinusEquals(Variable(a), b) => stream.println(q"$a = $a - $b")
    case _ => super.emitNode(sym, rhs)
  }
}

trait LuaFunctionGen extends BaseGenFunctions with LuaEffectGen with QuoteGen {
  val IR: TupledFunctionsExp

  import IR._

  private def emitFunctionBody(body: Block[Any]): Unit = {
    emitBlock(body)
    val result = getBlockResult(body)
    if (!(result.tp <:< ManifestTyp(manifest[Unit]))) {
      stream.println(q"return $result")
    }
    stream.println("end")
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case Lambda(_, UnboxedTuple(args), body) =>
      emitValDef(sym, "function" + args.map(quote).mkString("(", ", ", ")"))
      emitFunctionBody(body)
    case Lambda(_, arg, body) =>
      emitValDef(sym, q"function($arg)")
      emitFunctionBody(body)
    case Apply(fun, UnboxedTuple(args)) =>
      emitValDef(sym, q"$fun" + args.map(quote).mkString("(", ", ", ")"))
    case Apply(fun, arg) =>
      emitValDef(sym, q"$fun($arg)")
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
