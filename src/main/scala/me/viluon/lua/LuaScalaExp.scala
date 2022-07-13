package me.viluon.lua

import me.viluon.dsl.{DslExp, DslGen}
import me.viluon.lua.LuaUtils.formatLua
import me.viluon.lua.ast.{LuaArrayBufferOpsExp, LuaArrayOpsExpOpt, LuaMathExp, LuaModExp, LuaStringExp, LuaTupledFunctionsExp, LuaUnpackExp}
import me.viluon.lua.codegen.LuaCodegen

import java.io.{PrintWriter, StringWriter}
import scala.language.reflectiveCalls
import scala.lms.common._

/**
 * Lua expressions in Scala, with DSL functionality from [[LuaScala]].
 */
trait LuaScalaExp extends LuaScala
  with EffectExp
  with BaseFatExp
  with NumericOpsExpOpt
  with OrderingOpsExpOpt
  with EqualExpOpt
  with IfThenElseExpOpt
  with WhileExp
  with BooleanOpsExpOpt
  with LuaTupledFunctionsExp
  with TupledFunctionsRecursiveExp
  with TupleOpsExp
  with StructExpOpt
  with StructExpOptCommon
  with ObjectOpsExpOpt
  with VariablesExpOpt
  with PrimitiveOpsExpOpt
  with StringOpsExp
  with MiscOpsExp
  with SeqOpsExp
  with ArrayOpsExp
  with ArrayOpsExpOpt
  with LuaArrayBufferOpsExp
  with LuaArrayOpsExpOpt
  with LuaUnpackExp
  with LuaStringExp
  with LuaModExp
  with LuaMathExp

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

  lazy val code: String = {
    val source = new StringWriter()
    codegen.emitSource(main, "main", new PrintWriter(source))(manifestTyp[A], manifestTyp[B])
    formatLua(source.toString)
  }
}

abstract class Retargetable[A: Manifest, B: Manifest] extends LuaScalaExp with DslExp {
  self =>
  type SourceEmitter = {
    def emitSource[T, R](f: Rep[T] => Rep[R], n: String, o: PrintWriter)(implicit ev1: Typ[T], ev2: Typ[R]): List[(Sym[Any], Any)]
  }

  def main(input: Rep[A]): Rep[B]

  val luaGen = new LuaCodegen {
    override val IR: self.type = self
  }

  val scalaGen = new DslGen {
    override val IR: DslExp = self
  }

  private def emit(gen: SourceEmitter): String = {
    val source = new StringWriter()
    gen.emitSource(main, "main", new PrintWriter(source))(manifestTyp[A], manifestTyp[B])
    source.toString
  }

  lazy val lua: String = formatLua(emit(luaGen))
  lazy val scala: String = emit(scalaGen.asInstanceOf[SourceEmitter])
}
