package me.viluon.lua.computercraft.ast

import me.viluon.lua.LuaScalaExp
import me.viluon.lua.computercraft.lang.Os

import scala.lms.common.LiftArrays

trait OsExp extends Os with LuaScalaExp { self: LiftArrays =>
  type Event = Events.Event
  import Events.EventOps
  implicit val eventIsTyp = manifestTyp[Event]

  object Events {
    type Event = EventImpl
    private type EventImpl = Array[String]

    implicit class EventOps(ev: Exp[Event]) {
      def tag: Exp[String] =
        repArrayToArrayOps(ev)(manifestTyp)(unit(1))
    }
  }

  case class OsPullEvent() extends Def[Event]
  case class OsClock() extends Def[Double]
  case class OsQueueEvent(e: Exp[Event]) extends Def[Unit]

  object Os extends OsApi {
    override def pullEvent(): Exp[Event] = reflectEffect(OsPullEvent())
    override def clock(): Exp[Double] = reflectEffect(OsClock())
    override def queueEvent(e: Exp[Event]): Exp[Unit] = reflectEffect(OsQueueEvent(e))
  }
}
