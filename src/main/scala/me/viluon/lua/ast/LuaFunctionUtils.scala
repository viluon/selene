package me.viluon.lua.ast

import scala.lms.common.FunctionsExp
import scala.reflect.SourceContext

trait LuaFunctionUtils {
  this: FunctionsExp with LuaUnpackExp =>

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

  def pureFun2[A: Typ, B: Typ, C: Typ](d: Def[A => B => C])(implicit pos: SourceContext): Exp[A] => Exp[B] => Exp[C] =
    x => y => {
      val f = pureFun(d)
      reflectEffect(Apply(f(x), unbox(y)), Read(syms(y)))
    }

  def pureFun3[A: Typ, B: Typ, C: Typ, D: Typ](d: Def[A => B => C => D])(implicit pos: SourceContext): Exp[A] => Exp[B] => Exp[C] => Exp[D] =
    x => y => z => {
      val f = pureFun2(d)
      reflectEffect(Apply(f(x)(y), unbox(z)), Read(syms(z)))
    }

  def expOfUnboxIsUnboxOfExp[A <: Product : Typ](x: Exp[LuaUnboxedTuple[A]]): LuaUnboxedTuple[Exp[A]] = {
    val LuaUnboxedTuple(t: Exp[A] @unchecked) = x
    LuaUnboxedTuple[Exp[A]](t)(simpleClassTyp(classOf[Exp[A]]))
  }
}
