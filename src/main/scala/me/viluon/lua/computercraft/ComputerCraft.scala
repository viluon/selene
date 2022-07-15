package me.viluon.lua.computercraft

import me.viluon.lua.LuaUtils.formatLua
import me.viluon.lua.codegen.LuaCodegen
import me.viluon.lua.computercraft.ast.{OsExp, TermExp}
import me.viluon.lua.computercraft.gen.{OsGen, TermGen}
import me.viluon.lua.computercraft.lang.{Os, Term}
import me.viluon.lua.{LuaScala, LuaScalaExp}

import java.io.{PrintWriter, StringWriter}
import scala.language.reflectiveCalls
import scala.lms.common.LivenessOpt
import scala.lms.internal.{Effects, Expressions, NestedBlockTraversal}
import scala.reflect.SourceContext

trait ComputerCraft extends LuaScala with Term with Os
trait ComputerCraftExp extends ComputerCraft with TermExp with OsExp
trait CCCodegen extends LuaCodegen with TermGen with OsGen {
  val IR: ComputerCraftExp
}

abstract class CCProgram extends LuaScalaExp with ComputerCraftExp {
  self =>
  type SourceEmitter = {
    def emitSource[T, R](f: Rep[T] => Rep[R], n: String, o: PrintWriter)(implicit ev1: Typ[T], ev2: Typ[R]): List[(Sym[Any], Any)]
  }

  val nil: Rep[Unit] = unit(())

  implicit val initialCtx = SourceContext("main", Nil)

  def main(): Rep[Unit]

  val ccGen = new CCCodegen {
    override val IR: self.type = self
  }

  private def compile(): String = {
    val body = ccGen.reifyBlock(main())

    val (entry, source, _) = ccGen.emitSource(Nil, body)
    val sched = ccGen.getSchedule(Nil)(())
    reifyEffects {
      scala.Predef.println("schedule:\n" + sched.mkString("\n"))
      scala.Predef.println("yep")
      sched.foreach(ccGen.traverseStm)
      nil
    }

    import ccGen.stringContextToQuote
    q"""
       |$source
       |$entry()
       |""".stripMargin
  }

  private def emit(gen: SourceEmitter): String = {
    val source = new StringWriter()
    gen.emitSource({ignore: Rep[Unit] => main()}, "main", new PrintWriter(source))
    s"""
       |${source.toString}
       |main()
       |""".stripMargin
  }

  lazy val lua: String = formatLua(compile())
}
