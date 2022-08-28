package me.viluon.lua.computercraft.lang

import scala.lms.common.Base

trait Keys extends Base {
  object Keys extends Enumeration {
    type Key = Value
    val right : Key = Value(262)
    val left  : Key = Value(263)
    val down  : Key = Value(264)
    val up    : Key = Value(265)

    val backspace: Key = Value(259)
  }

  import Keys.Key
  implicit def keyTyp: Typ[Key]
}
