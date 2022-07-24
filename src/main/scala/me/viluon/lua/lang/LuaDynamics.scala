package me.viluon.lua.lang

import scala.lms.common.Base
import scala.reflect.SourceContext

trait LuaDynamics extends Base {
  import scala.language.dynamics
  type DynamicRep <: DynamicRepImpl with Rep[Any]

  trait DynamicRepImpl extends Dynamic {
    def applyDynamic(field: String)(implicit sourceContext: SourceContext): ApplyDynamicSelector
    def selectDynamic(field: String)(implicit sourceContext: SourceContext): DynamicRep
    def updateDynamic(field: String)(value: Rep[Any])(implicit sourceContext: SourceContext): Rep[Unit]
  }

  trait ApplyDynamicSelector {
    def apply(args: Rep[Any]*)(implicit sourceContext: SourceContext): DynamicRep
    // TODO: array-like update
    //def update(values: Rep[Any]*)(implicit sourceContext: SourceContext): Rep[Unit]
  }

  def dynamic(x: Rep[Any])(implicit sourceContext: SourceContext): DynamicRep
  def newDynamic(constructor: String)(args: Rep[Any]*)(implicit sourceContext: SourceContext): DynamicRep
  def inlineDynamic(code: String)(implicit sourceContext: SourceContext): DynamicRep
}
