package me.viluon.lua.computercraft.lang

import scala.lms.common.{ArrayOps, LiftArrays, StructOps}

trait Os extends StructOps with ArrayOps {
  type Event

  trait OsApi {
    def pullEvent(): Rep[Event]
    def clock(): Rep[Double]
    def queueEvent(e: Rep[Event]): Rep[Unit]
  }
}
