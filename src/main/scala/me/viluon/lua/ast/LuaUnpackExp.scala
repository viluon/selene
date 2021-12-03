package me.viluon.lua.ast

import me.viluon.lua.lang.LuaUnpack

import scala.lms.common.{ArrayOpsExp, TupledFunctionsExp}
import scala.reflect.SourceContext

trait LuaUnpackExp extends LuaUnpack with TupledFunctionsExp with ArrayOpsExp {
  case class LuaUnboxedTuple[T: Typ](t: T)
  case class Unpack[T: Typ](x: Array[Exp[Any]]) extends Def[T]
  class UnboxedSym[T: Typ](id: Int) extends Sym[T](id) {
    val components: Seq[Sym[Any]] = {
      val tp = implicitly[Typ[T]]
      // FIXME maybe it'd be better to move this check to fresh,
      //       so that an unboxed sym isn't even constructed
      //       in such a case
      if (tp.typeArguments.head.typeArguments.isEmpty)
        List(fresh(tp.typeArguments.head))
      else tp.typeArguments.head.typeArguments.map {
        t: Typ[_] => fresh(t)
      }
    }
  }

  object UnboxedSym {
    def unapply[T](obj: Any): Option[(Sym[T], Seq[Sym[Any]])] = obj match {
      case _: UnboxedSym[T] =>
        val sym = obj.asInstanceOf[UnboxedSym[T]]
        Some((Sym(sym.id)(sym.tp), sym.components))
      case _ => None
    }
  }

  implicit def lutIsTyp[T: Typ]: Typ[LuaUnboxedTuple[T]] =
    simpleClassTyp(classOf[LuaUnboxedTuple[T]])

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

  override def fresh[T: Typ]: Sym[T] =
    if (implicitly[Typ[T]].runtimeClass == classOf[LuaUnboxedTuple[_]]) {
      new UnboxedSym(
        try nVars finally nVars += 1
      )(implicitly[Typ[T]])
    } else super.fresh[T]

  override def unpack[T: Typ](arr: Array[Exp[Any]])(implicit pos: SourceContext): Exp[T] = arr match {
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
    case _ => reflectEffect(Unpack(arr)(implicitly[Typ[T]]))
  }
}
