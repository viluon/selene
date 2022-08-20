package me.viluon.lua.ast

import scala.lms.common.{BooleanOps, EqualExpOpt}
import scala.reflect.SourceContext

trait LuaEqualOptExp extends EqualExpOpt { self: BooleanOps =>
  override def equals[A: Typ, B: Typ](a: Exp[A], b: Exp[B])(implicit pos: SourceContext): Exp[Boolean] = (a, b) match {
    case (Const(true), b)
      if implicitly[Typ[B]] <:< implicitly[Typ[Boolean]] => b.asInstanceOf[Exp[Boolean]]
    case (a, Const(true))
      if implicitly[Typ[A]] <:< implicitly[Typ[Boolean]] => a.asInstanceOf[Exp[Boolean]]
    case (Const(false), b)
      if implicitly[Typ[B]] <:< implicitly[Typ[Boolean]] => !b.asInstanceOf[Exp[Boolean]]
    case (a, Const(false))
      if implicitly[Typ[A]] <:< implicitly[Typ[Boolean]] => !a.asInstanceOf[Exp[Boolean]]
    case _ => super.equals(a, b)
  }

  override def notequals[A: Typ, B: Typ](a: Exp[A], b: Exp[B])(implicit pos: SourceContext): Exp[Boolean] = (a, b) match {
    case (Const(false), b)
      if implicitly[Typ[B]] <:< implicitly[Typ[Boolean]] => b.asInstanceOf[Exp[Boolean]]
    case (a, Const(false))
      if implicitly[Typ[A]] <:< implicitly[Typ[Boolean]] => a.asInstanceOf[Exp[Boolean]]
    case (Const(true), b)
      if implicitly[Typ[B]] <:< implicitly[Typ[Boolean]] => !b.asInstanceOf[Exp[Boolean]]
    case (a, Const(true))
      if implicitly[Typ[A]] <:< implicitly[Typ[Boolean]] => !a.asInstanceOf[Exp[Boolean]]
    case _ => super.notequals(a, b)
  }
}
