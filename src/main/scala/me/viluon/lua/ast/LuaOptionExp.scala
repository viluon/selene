package me.viluon.lua.ast

import me.viluon.lua.lang.LuaOption

import scala.lms.common.{BooleanOps, EffectExp, IfThenElse, LiftBoolean, Record, StructOps}

trait LuaOptionExp extends LuaOption with EffectExp {
  self: StructOps with BooleanOps with LiftBoolean with IfThenElse =>
  import scala.{Option => ScalaOption}

  private implicit def optionContainerTyp[A: Typ]: Typ[OptionalContainer] = manifestTyp

  /**
   * The unsafe container underlying [[Option]].
   */
  protected abstract class OptionalContainer extends Record {
    private[LuaOptionExp] def isDefined: Boolean
    private[LuaOptionExp] def value: Any
  }

  private abstract class Some() extends OptionalContainer
  private abstract class None() extends OptionalContainer

  private object OptionalContainer {
    def empty: Rep[OptionalContainer] = new None() {
      val isDefined: Boolean = false
      val value: Any = ()
    }
    def apply[T](x: Rep[T]): Rep[OptionalContainer] = new Some() {
      val isDefined: Boolean = true
      val value: Any = x
    }
  }

  implicit class OptionOps[A: Typ](fake: Rep[ScalaOption[A]]) {
    val x: Option[A] = fake.asInstanceOf[Option[A]]

    def map[B: Typ](f: Rep[A] => Rep[B]): Rep[ScalaOption[B]] = flatMap(f andThen Option.apply[B])
    def flatMap[B: Typ](f: Rep[A] => Rep[ScalaOption[B]]): Rep[ScalaOption[B]] = {
      if (x.isDefined) f(x.get)
      else Option.empty[B]
    }

    def orElse(default: Rep[ScalaOption[A]]): Rep[ScalaOption[A]] =
      if (x.isDefined) x else default
    def getOrElse(default: Rep[A]): Rep[A] =
      if (x.isDefined) x.get else default
  }

  /**
   * A Lua-time analogue of [[scala.Option]]. This type is <b>not</b> intended to be wrapped in [[Rep]],
   * it is already staged.
   *
   * Hides the internal unsafe implementation and provides a type-safe interface.
   */
  class Option[+A: Typ](private[LuaOptionExp] val container: Rep[OptionalContainer]) extends Exp[ScalaOption[A]] {
    // FIXME this wrapper style is a dead end.
    //  Either you keep the high-level wrapper entirely on the Scala side, which means it doesn't extend Exp
    //  and thus doesn't work with virtualized control flow, or you stage it by extending Exp and then
    //  can't rely on the fact that the associated Rep[T] will always be your neat Scala wrapper (it could be a
    //  variable reference instead, for example).
    //  We could of course just treat this as any language extension, but the purpose of this experiment was to build
    //  abstractions ergonomically with classes. Language extensions are more work.
    //  .
    //  So why do we need this class in the first place? Well, records don't support type arguments.
    //  This class exists to give OptionalContainer the necessary type info.

    def isDefined: self.Rep[Boolean] = container.isDefined
    def get: Rep[A] = container.selectDynamic[A]("value")
  }

  private implicit def scalaOptionTyp[A: Typ]: Typ[ScalaOption[A]] = {
    simpleClassTyp[ScalaOption[A]](classOf[ScalaOption[A]])
  }

  /**
   * The companion object of [[Option]].
   */
  object Option {
    def apply[A: Typ](x: Rep[A]): Option[A] = new Option[A](OptionalContainer(x))
    def empty[A: Typ]: Option[A] = new Option[A](OptionalContainer.empty)
  }
}
