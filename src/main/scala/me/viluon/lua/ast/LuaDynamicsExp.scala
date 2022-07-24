package me.viluon.lua.ast

import me.viluon.lua.lang.LuaDynamics

import scala.lms.common.EffectExp
import scala.reflect.SourceContext

trait LuaDynamicsExp extends LuaDynamics with EffectExp {
  type DynamicRep = DynamicExp
  private implicit val anyTyp: Typ[Any] = manifestTyp

  case class DynamicCall(receiver: Exp[Any], method: String, args: List[Exp[Any]]) extends Def[Any]
  case class DynamicSelect(receiver: Exp[Any], field: String) extends Def[Any]
  case class DynamicUpdate(receiver: Exp[Any], field: String, value: Exp[Any]) extends Def[Unit]
  case class DynamicNew(constructor: String, args: List[Exp[Any]]) extends Def[Any]
  case class DynamicInline(code: String) extends Def[Any]

  case class ApplyDynamicSelectorImpl(receiver: Exp[Any], field: String) extends ApplyDynamicSelector {
    override def apply(args: Exp[Any]*)(implicit sourceContext: SourceContext): DynamicExp =
      dynamic(reflectEffect(DynamicCall(receiver, field, args.toList)))
  }

  case class DynamicExp(receiver: Exp[Any]) extends Exp[Any] with DynamicRepImpl {
    override def applyDynamic(field: String)(implicit sourceContext: SourceContext): ApplyDynamicSelector =
      ApplyDynamicSelectorImpl(receiver, field)

    override def selectDynamic(field: String)(implicit sourceContext: SourceContext): DynamicExp =
    // No call to reflectEffect at the moment because selecting a
    // field is not _really_ a side-effecting operation. However, we
    // might still want to express that this operation causes a read
    // on the field of the receiver
    // TODO before attempting optimizations
      dynamic(DynamicSelect(receiver, field))

    override def updateDynamic(field: String)(value: Exp[Any])(implicit sourceContext: SourceContext): Exp[Unit] =
      reflectEffect(DynamicUpdate(receiver, field, value))
  }

  def dynamic(x: Exp[Any])(implicit sourceContext: SourceContext): DynamicExp = DynamicExp(x)

  def newDynamic(constructor: String)(args: Exp[Any]*)(implicit sourceContext: SourceContext): DynamicExp =
    dynamic(reflectEffect(DynamicNew(constructor, args.toList)))

  def inlineDynamic(code: String)(implicit sourceContext: SourceContext): DynamicExp =
    dynamic(reflectEffect(DynamicInline(code)))
}
