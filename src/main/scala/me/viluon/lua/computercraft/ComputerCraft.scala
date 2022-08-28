package me.viluon.lua.computercraft

import me.viluon.lua.LuaUtils.formatLua
import me.viluon.lua.codegen.LuaCodegen
import me.viluon.lua.computercraft.ast.{KeysExp, OsExp, TermExp}
import me.viluon.lua.computercraft.gen.{KeysGen, OsGen, TermGen}
import me.viluon.lua.computercraft.lang.{Keys, Os, Term}
import me.viluon.lua.{LuaScala, LuaScalaExp}

import scala.reflect.SourceContext

trait ComputerCraft extends LuaScala with Term with Os with Keys
trait ComputerCraftExp extends ComputerCraft with TermExp with OsExp with KeysExp
trait CCCodegen extends LuaCodegen with TermGen with OsGen with KeysGen {
  val IR: ComputerCraftExp
}

trait CCLibrary extends LuaScalaExp with ComputerCraftExp { self =>
  implicit val initialCtx: SourceContext = SourceContext("main", Nil)
  val nil: Rep[Unit] = unit(())
  val ccGen = new CCCodegen {
    override val IR: self.type = self
  }
}

abstract class CCProgram extends CCLibrary {
  def main(): Rep[Unit]

  lazy val compiled: (String, String) = compile()
  def lua: String = formatLua(compiled._2)

  private def compile(): (String, String) = {
    val body = ccGen.reifyBlock(main())
    val (_, unalloc, source, _) = ccGen.emitSource(Nil, body)
    unalloc -> source
  }
}
