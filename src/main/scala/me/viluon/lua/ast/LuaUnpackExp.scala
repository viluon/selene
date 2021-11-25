package me.viluon.lua.ast

import me.viluon.lua.lang.LuaUnpack

import scala.lms.common.{ArrayOpsExp, BaseFatExp, TupledFunctionsExp}
import scala.reflect.SourceContext

trait LuaUnpackExp extends LuaUnpack with TupledFunctionsExp with ArrayOpsExp with BaseFatExp {
  case class Unpack[T: Typ](x: Array[Exp[Any]]) extends Def[T]

  // TODO we should really produce a new UnboxedTuple[?]
  //  with fresh Syms for each element. During quoting,
  //  we just do .map(quote).mkString(", "). Getting the
  //  Syms is just a matter of copy-pasting from
  //  reflectEffectInternal.
  //  The question remains what to do about next-stage
  //  expressions during unpacking, type-wise (note that
  //  queueEvent(unpack(x)) is correct, regardless of
  //  the tuple's arity). Let's start with the simple
  //  case first though.

  override def unpack[T: Typ](x: Array[Exp[Any]])(implicit pos: SourceContext): Exp[T] = x match {
//    case Def(ArrayFromSeq(xs)) =>
//      def go[A: Typ](xs: Seq[Exp[A]]): (List[Exp[A]], Exp[Any]) = xs match {
//        case (k@Const(_)) +: xss => go(xss) match {
//          case (ks, suffix) => (k :: ks, suffix)
//        }
//        case Nil => (Nil, Const(()))
//        case suffix => (Nil, Unpack(array_obj_fromseq(suffix)))
//      }
//
//      // a constant prefix can be factored out of the unpack call
//      val (ks, suffix) = go(xs)
//      UnboxedTuple(ks :+ suffix)(implicitly[Typ[Any]])
    case _ => reflectEffect(Unpack(x))
  }
}
