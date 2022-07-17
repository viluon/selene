package me.viluon.lua.computercraft

import me.viluon.lua.LuaUtils.formatLua
import me.viluon.lua.codegen.LuaCodegen
import me.viluon.lua.computercraft.ast.{OsExp, TermExp}
import me.viluon.lua.computercraft.gen.{OsGen, TermGen}
import me.viluon.lua.computercraft.lang.{Os, Term}
import me.viluon.lua.{LuaScala, LuaScalaExp}

import scala.reflect.SourceContext

trait ComputerCraft extends LuaScala with Term with Os
trait ComputerCraftExp extends ComputerCraft with TermExp with OsExp
trait CCCodegen extends LuaCodegen with TermGen with OsGen {
  val IR: ComputerCraftExp
}

trait CCLibrary extends LuaScalaExp with ComputerCraftExp { self =>
  val nil: Rep[Unit] = unit(())
  val ccGen = new CCCodegen {
    override val IR: self.type = self
  }
}

abstract class CCProgram extends CCLibrary {
  implicit val initialCtx: SourceContext = SourceContext("main", Nil)

  def main(): Rep[Unit]

  lazy val compiled: (String, String) = compile()
  def lua: String = formatLua(compiled._2)

  private def compile(): (String, String) = {
    val body = ccGen.reifyBlock(main())
    val (_, unalloc, source, _) = ccGen.emitSource(Nil, body)
    unalloc -> source
  }
}
