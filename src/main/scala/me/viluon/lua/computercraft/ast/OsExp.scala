package me.viluon.lua.computercraft.ast

import me.viluon.lua.LuaScalaExp
import me.viluon.lua.ast.LuaFunctionUtils
import me.viluon.lua.computercraft.lang.Os

import scala.lms.common.LiftArrays
import scala.reflect.SourceContext

trait OsExp extends Os with LuaScalaExp with LuaFunctionUtils { self: LiftArrays =>
  type Event = Events.Event
  import Events.EventOps
  implicit val eventIsTyp = manifestTyp[Event]

  object Events {
    type Event = EventImpl
    private type EventImpl = Array[String]

    implicit class EventOps(ev: Exp[Event]) {
      def tag(implicit pos: SourceContext): Exp[String] =
        repArrayToArrayOps(ev)(manifestTyp)(unit(1))
    }
  }

  case class Dummy()

  implicit class OsOps(os: Exp[OsAPI]) extends OsAPI {
    override def pullEvent()(implicit pos: SourceContext): Exp[Event] = applyImpureFun(OsPullEvent(os), ())
    override def clock()(implicit pos: SourceContext): Exp[Double] = applyImpureFun(OsClock(os), ())
    private def queueFun[T: Typ](implicit pos: SourceContext): Exp[LuaUnboxedTuple[T]] => Exp[Unit] = {
      impureFun(OsQueueEvent(os))(manifestTyp, implicitly, implicitly)
    }

    override def queueEvent(e: Exp[String])(implicit pos: SourceContext): Exp[Unit] = {
      val f = queueFun[String]
      f(unpack[LuaUnboxedTuple[String]](List(e).toArray)(lutIsTyp, implicitly))
    }

    override def queueEvent[A1: Typ](e: Exp[String], a1: Exp[A1])(implicit pos: SourceContext): Exp[Unit] = {
      val f = queueFun[(String, A1)]
      f(unpack[LuaUnboxedTuple[(String, A1)]](List(e, a1).toArray)(lutIsTyp, implicitly))
    }

    override def queueEvent[A1: Typ, A2: Typ](e: Exp[String], a1: Exp[A1], a2: Exp[A2])(implicit pos: SourceContext): Exp[Unit] = {
      val f = queueFun[(String, A1, A2)]
      f(unpack[LuaUnboxedTuple[(String, A1, A2)]](List(e, a1, a2).toArray)(lutIsTyp, implicitly))
    }
  }

  case class GlobalOs() extends Def[OsAPI]
  implicit val osApiTyp: Typ[OsAPI] = manifestTyp

  case class OsPullEvent(os: Exp[OsAPI]) extends Def[Unit => Event]
  case class OsClock(os: Exp[OsAPI]) extends Def[Unit => Double]
  case class OsQueueEvent(os: Exp[OsAPI]) extends Def[LuaUnboxedTuple[_] => Unit]

  override def os(implicit pos: SourceContext): Exp[OsAPI] = toAtom(GlobalOs())
}
