package me.viluon.lua.computercraft.lang

import scala.lms.common.{ArrayOps, LiftArrays, StructOps}
import scala.reflect.SourceContext

trait Os extends StructOps with ArrayOps {
  type Event

  trait OsAPI {
    def pullEvent()(implicit pos: SourceContext): Rep[Event]
    def clock()(implicit pos: SourceContext): Rep[Double]
    def queueEvent(e: Rep[String])(implicit pos: SourceContext): Rep[Unit]
    def queueEvent[A1: Typ](e: Rep[String], a1: Rep[A1])(implicit pos: SourceContext): Rep[Unit]
    def queueEvent[A1: Typ, A2: Typ](e: Rep[String], a1: Rep[A1], a2: Rep[A2])(implicit pos: SourceContext): Rep[Unit]
//    def queueEvent[A1: Typ, A2: Typ, A3: Typ](e: Rep[String], a1: Rep[A1], a2: Rep[A2], a3: Rep[A3])(implicit pos: SourceContext): Rep[Unit]
//    def queueEvent[A1: Typ, A2: Typ, A3: Typ, A4: Typ](e: Rep[String], a1: Rep[A1], a2: Rep[A2], a3: Rep[A3], a4: Rep[A4])(implicit pos: SourceContext): Rep[Unit]
  }

  def os(implicit pos: SourceContext): Rep[OsAPI]
}
