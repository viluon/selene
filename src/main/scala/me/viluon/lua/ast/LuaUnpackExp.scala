package me.viluon.lua.ast

import me.viluon.Main
import me.viluon.lua.lang.LuaUnpack

import scala.lms.common.{ArrayOpsExp, TupledFunctionsExp}
import scala.reflect.SourceContext

trait LuaUnpackExp extends LuaUnpack with TupledFunctionsExp with ArrayOpsExp {
  // TODO could we deprecate this in favour of LMS's UnboxedTuple?
  case class LuaUnboxedTuple[T: Typ](t: T)
  // ignored by code generation
  case class DummyUnboxedSymUse(sym: UnboxedSym[_]) extends Def[Unit]

  object LuaUnboxedTuple {
    def unapply[T <: Product](obj: Exp[LuaUnboxedTuple[T]])(implicit pos: SourceContext): Option[T] = obj match {
      case UnboxedSym(_, xs) => Some(((xs.map(sym => sym.withPos(pos :: sym.pos)) match {
        case Nil => new Product {
          override def productElement(n: Int): Any = ()
          override def productArity: Int = 0
          override def canEqual(that: Any): Boolean =
            that.isInstanceOf[Unit] || that.isInstanceOf[this.type]
        }
        case Seq(a) => Tuple1(a)
        case Seq(a, b) => (a, b)
        case Seq(a, b, c) => (a, b, c)
        case Seq(a, b, c, d) => (a, b, c, d)
        case Seq(a, b, c, d, e) => (a, b, c, d, e)
        case _ => ???
      }): Product).asInstanceOf[T])
      case _ => None
    }

//    def unapply[T <: Product : Typ](obj: LuaUnboxedTuple[T]): Option[T] = Some(obj.t)
  }

  case class Unpack[T: Typ](x: Exp[Array[Any]]) extends Def[T]
  class UnboxedSym[T: Typ](id: Int) extends Sym[T](id) with Product {
    val components: Seq[Sym[Any]] = {
      // T is a LuaUnboxedTuple, so its first (and only)
      // type argument is the actual tuple this sym represents
      val typ = implicitly[Typ[T]].typeArguments.head
      // FIXME maybe it'd be better to move this check to fresh,
      //       so that an unboxed sym isn't even constructed
      //       in such a case
      if (typ.typeArguments.isEmpty)
        List(fresh(typ))
      else typ.typeArguments.map {
        t: Typ[_] => fresh(t)
      }
    }

    // this ensures code that accesses only individual fields of the tuple
    // isn't moved out of the scope of the unboxed definition
    components.foreach {
      sym => createDefinition(sym, DummyUnboxedSymUse(this))
    }

    override def withPos(pos: List[SourceContext]): Sym[T] = {
      components.foreach(_.withPos(pos))
      super.withPos(pos)
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

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case Unpack(x) => boundSyms(x)
    case _ => super.boundSyms(e)
  }

  override def unpack[T <: Product : Typ](arr: Exp[Array[_]])(implicit pos: SourceContext): Exp[LuaUnboxedTuple[T]] = arr match {
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
    // this is where a LuaUnboxedTuple disappears -- it only serves
    // as a compile-time marker and is actually represented by an array
    case _: Sym[_] =>
      val sym = arr.asInstanceOf[Sym[Array[Any]]]
      reflectEffect(Unpack(sym)(lutIsTyp(implicitly[Typ[T]])), Read(List(sym)))
  }
}
