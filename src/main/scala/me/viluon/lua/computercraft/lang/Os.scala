package me.viluon.lua.computercraft.lang

import scala.lms.common.{ArrayOps, LiftArrays, StructOps}
import scala.reflect.SourceContext

trait Os extends StructOps with ArrayOps {
  type Event

  trait OsAPI {
    def pullEvent()(implicit pos: SourceContext): Rep[Event]
    def clock()(implicit pos: SourceContext): Rep[Double]
    def queueEvent(e: Rep[Any]*)(implicit pos: SourceContext): Rep[Unit]
  }

  def os(implicit pos: SourceContext): Rep[OsAPI]
}
