package me.viluon.lua

import java.io.PrintWriter
import scala.lms.common._

trait LuaScalaExp extends LuaScala
  with EffectExp
  with NumericOpsExp
  with OrderingOpsExp
  with EqualExp
  with IfThenElseExp
  with WhileExp
  with BooleanOpsExp
  with VariablesExp
  with PrimitiveOpsExp
  with StringOpsExp
  with MiscOpsExp

trait LuaExpGen extends LuaScalaExp { t =>

  override implicit def arrayTyp[T](implicit evidence$7: Typ[T]): Typ[Array[T]] = ???

  val codegen = new LuaScalaGen {
    val IR: t.type = t

//    override def emitSource[A](args: List[Sym[_]],
//                               body: Block[A],
//                               className: String,
//                               stream: PrintWriter)(implicit ev: Typ[A]): List[(Sym[Any], Any)] = {
//      ???
//    }
//
//    override def reifyBlock[T](x: => Rep[T])(implicit ev: Typ[T]): Block[T] = ???
//
//    override def getBlockResultFull[A](s: Block[A]): Rep[A] = ???
//
//    override def traverseBlock[A](block: Block[A]): Unit = ???
  }
}
