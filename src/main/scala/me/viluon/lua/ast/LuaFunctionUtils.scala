package me.viluon.lua.ast

import scala.lms.common.FunctionsExp
import scala.reflect.SourceContext

trait LuaFunctionUtils {
  this: FunctionsExp =>

  def expOfFunIsFunOfExp[A: Typ, B: Typ](f: Exp[A => B])(implicit pos: SourceContext): Exp[A] => Exp[B] =
    x => doApply(f, x)

  def impureFun[A: Typ, B: Typ](d: Def[A => B])(implicit pos: SourceContext): Exp[A] => Exp[B] =
    expOfFunIsFunOfExp(toAtom(d))

  def applyImpureFun[A: Typ, B: Typ](d: Def[A => B], arg: Exp[A])(implicit pos: SourceContext): Exp[B] = {
    val f = impureFun(d)
    f(arg)
  }

  def pureFun[A: Typ, B: Typ](d: Def[A => B])(implicit pos: SourceContext): Exp[A] => Exp[B] =
    x => reflectEffect(Apply(toAtom(d), unbox(x)), Read(syms(x)))
}
