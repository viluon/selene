package me.viluon.lua.computercraft.ast

import me.viluon.lua.LuaScalaExp
import me.viluon.lua.ast.LuaFunctionUtils
import me.viluon.lua.computercraft.lang.Os

import scala.lms.common.LiftArrays
import scala.reflect.SourceContext

trait OsExp extends Os with LuaScalaExp with LuaFunctionUtils { self: LiftArrays =>
  import collection.immutable.{List => ScalaList}

  implicit class OsOps(os: Exp[OsAPI]) extends OsAPI {
    // FIXME actually returns an unboxed tuple, requires explicit packing
    override def pullEvent()(implicit pos: SourceContext): Exp[LuaUnboxedTuple[(Any, Any, Any, Any, Any)]] =
      applyImpureFun(OsPullEvent(os), ())(unitTyp, manifestTyp, pos)
    override def clock()(implicit pos: SourceContext): Exp[Double] = applyImpureFun(OsClock(os), ())
    private def queueFun[T: Typ](implicit pos: SourceContext): Exp[LuaUnboxedTuple[T]] => Exp[Unit] = {
      impureFun(OsQueueEvent(os))(manifestTyp, implicitly, implicitly)
    }

    override def queueEvent(e: Exp[String])(implicit pos: SourceContext): Exp[Unit] = {
      val f = queueFun[Tuple1[String]](manifestTyp, implicitly)
      f(unpack[Tuple1[String]](array_obj_fromseq(ScalaList(e)))(manifestTyp, implicitly))
    }

    override def queueEvent[A1: Typ](e: Exp[String], a1: Exp[A1])(implicit pos: SourceContext): Exp[Unit] = {
      val f = queueFun[(String, A1)]
      f(unpack[(String, A1)](array_obj_fromseq(ScalaList(e, a1))(manifestTyp)))
    }

    override def queueEvent[A1: Typ, A2: Typ](e: Exp[String], a1: Exp[A1], a2: Exp[A2])(implicit pos: SourceContext): Exp[Unit] = {
      val f = queueFun[(String, A1, A2)]
      f(unpack[(String, A1, A2)](array_obj_fromseq(ScalaList(e, a1, a2))(manifestTyp)))
    }
  }

  case class GlobalOs() extends Def[OsAPI]
  implicit val osApiTyp: Typ[OsAPI] = manifestTyp

  case class OsPullEvent(os: Exp[OsAPI]) extends Def[Unit => LuaUnboxedTuple[(Any, Any, Any, Any, Any)]]
  case class OsClock(os: Exp[OsAPI]) extends Def[Unit => Double]
  case class OsQueueEvent(os: Exp[OsAPI]) extends Def[LuaUnboxedTuple[_] => Unit]

  override def os(implicit pos: SourceContext): Exp[OsAPI] = toAtom(GlobalOs())
}
