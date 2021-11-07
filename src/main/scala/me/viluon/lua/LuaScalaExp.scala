package me.viluon.lua

import me.viluon.lua.codegen.LuaCodegen

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
  with StructExpOpt
  with StructExpOptCommon
  with VariablesExpOpt
  with PrimitiveOpsExpOpt
  with StringOpsExp
  with MiscOpsExp

/**
 * [[LuaDSL]] extends Lua expressions in Scala ([[LuaScalaExp]]) with codegen ([[LuaCodegen]]).
 */
abstract class LuaDSL[A: Manifest, B: Manifest] extends LuaScalaExp {
  t =>

  override implicit def arrayTyp[T](implicit evidence$7: Typ[T]): Typ[Array[T]] = ???

  def main(input: Rep[A]): Rep[B]

  val codegen = new LuaCodegen {
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

  def formatLua(code: String, indentString: String = "  "): String = {
    val it = for {line <- code.lines} yield {
      // FIXME incomplete keyword list, doesn't handle () [] {}
      "\\b(function|then|elseif|else|end)\\b".r.findFirstMatchIn(line) match {
        case Some(x) => x.toString match {
          case "function" | "then" => (0, 1)
          case "elseif" | "else" => (-1, 0)
          case "end" => (-1, -1)
        }
        case None => (0, 0)
      }
    }
    code.lines.zip(it).foldLeft((0, List[String]()))({
      case ((indent, acc), (line, (curr, next))) =>
        (indent + next, indentString * (indent + curr) + line :: acc)
    })._2.reverse.mkString("\n")
  }

  lazy val code: String = {
    val source = new StringWriter()
    codegen.emitSource(main, "main", new PrintWriter(source))(manifestTyp[A], manifestTyp[B])
    formatLua(source.toString)
  }
}
