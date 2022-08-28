package me.viluon.lua.computercraft.ast

import me.viluon.lua.LuaScalaExp
import me.viluon.lua.computercraft.lang.Keys

import scala.reflect.SourceContext

trait KeysExp extends Keys with LuaScalaExp {
  import Keys.Key

  override implicit def keyTyp: Typ[Key] = manifestTyp

  case class KeyToInt(k: Exp[Key]) extends Exp[Int]

  implicit class KeyOps(key: Exp[Key]) {
    def toInt(implicit src: SourceContext): Exp[Int] = key match {
      case Const(key) => key.id
      case _ => KeyToInt(key)
    }
  }
}
