package me.viluon.lua.computercraft.lang

import me.viluon.lua.LuaScalaExp
import me.viluon.lua.LuaUtils.formatLua
import me.viluon.lua.codegen.LuaCodegen
import me.viluon.lua.computercraft.ast.TermExp
import me.viluon.lua.computercraft.gen.TermGen

import java.io.{PrintWriter, StringWriter}

trait ComputerCraft extends Term
trait ComputerCraftExp extends ComputerCraft with TermExp {
  implicit def arrayTyp[T:Typ]: Typ[Array[T]] = typ[T].arrayTyp
}
trait CCCodegen extends LuaCodegen with TermGen

abstract class CCProgram[A: Manifest, B: Manifest] extends LuaScalaExp with ComputerCraftExp {
  self =>
  type SourceEmitter = {
    def emitSource[T, R](f: Rep[T] => Rep[R], n: String, o: PrintWriter)(implicit ev1: Typ[T], ev2: Typ[R]): List[(Sym[Any], Any)]
  }

  def main(input: Rep[A]): Rep[B]

  val ccGen = new CCCodegen {
    override val IR: self.type = self
  }

  private def emit(gen: SourceEmitter): String = {
    val source = new StringWriter()
    gen.emitSource(main, "main", new PrintWriter(source))(manifestTyp[A], manifestTyp[B])
    source.toString
  }

  lazy val lua: String = formatLua(emit(ccGen))
}
