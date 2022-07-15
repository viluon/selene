package me.viluon.lua.codegen

import scala.lms.internal.{BlockTraversal, Expressions, GenerationFailedException}

trait DummyGen extends BlockTraversal {
  val IR: Expressions
  import IR._

  def quote(x: Exp[Any]) : String = x match {
    case Const(s: String) => "\""+s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n")+"\"" // TODO: more escapes?
    case Const(c: Char) => "'"+(""+c).replace("'", "\\'").replace("\n", "\\n")+"'"
    case Const(f: Float) => "%1.10f".format(f) + "f"
    case Const(l: Long) => l.toString + "L"
    case Const(null) => "null"
    case Const(z) => z.toString
    case Sym(n) => "x"+n
    case _ => throw new RuntimeException("could not quote " + x)
  }

  def emitBlock(y: Block[Any]): Unit = traverseBlock(y)

  def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit =
    throw new GenerationFailedException("don't know how to generate code for: " + rhs)

  override def traverseStm(stm: Stm): Unit = stm match {
    case TP(sym, rhs) => emitNode(sym,rhs)
    case _ => throw new GenerationFailedException("don't know how to generate code for statement: " + stm)
  }
}
