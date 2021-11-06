package me.viluon.lua

import java.io.{PrintWriter, StringWriter}
import scala.lms.common._

/**
 * Lua expressions in Scala, with DSL functionality from [[LuaScala]].
 */
trait LuaScalaExp extends LuaScala
  with EffectExp
  with NumericOpsExpOpt
  with OrderingOpsExpOpt
  with EqualExpOpt
  with IfThenElseExpOpt
  with WhileExp
  //  with BooleanOpsExp
  with FunctionsExp
  with TupledFunctionsExp
  with TupleOpsExp
  with VariablesExpOpt
  with PrimitiveOpsExpOpt
  with StringOpsExp
  with MiscOpsExp

/**
 * [[LuaExpGen]] extends Lua expressions in Scala ([[LuaScalaExp]]) with codegen ([[LuaScalaGen]]).
 */
trait LuaExpGen[-A, +B] extends LuaScalaExp {
  t =>

  override implicit def arrayTyp[T](implicit evidence$7: Typ[T]): Typ[Array[T]] = ???

  def main(input: Rep[A]): Rep[B]

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

  lazy val code: String = {
    val source = new StringWriter()
    codegen.emitSource(main, "main", new PrintWriter(source))(manifestTyp[Int], manifestTyp[Int])
    source.toString
  }
}
