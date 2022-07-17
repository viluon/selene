package me.viluon.lua.ast

import me.viluon.Main
import me.viluon.lua.lang.LuaUnpack

import scala.lms.common.{ArrayOpsExp, TupledFunctionsExp}
import scala.reflect.SourceContext

trait LuaUnpackExp extends LuaUnpack with TupledFunctionsExp with ArrayOpsExp {
  // TODO could we deprecate this in favour of LMS's UnboxedTuple?
  case class LuaUnboxedTuple[T: Typ](t: T)

  case class DummyRead[A]() extends Def[A]

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
  class UnboxedSym[T: Typ](id: Int, var components: Seq[Sym[Any]]) extends Sym[T](id) with Product {
    override def withPos(pos: List[SourceContext]): Sym[T] = {
      components.foreach(_.withPos(pos))
      super.withPos(pos)
    }
  }

  object UnboxedSym {
    def unapply(sym: Sym[Any]): Option[(Int, Seq[Sym[Any]])] = sym match {
      case s: UnboxedSym[_] => Some((s.id, s.components))
      case _ => None
    }
  }

  implicit def lutIsTyp[T: Typ]: Typ[LuaUnboxedTuple[T]] =
    simpleClassTyp(classOf[LuaUnboxedTuple[T]])

  // TODO
  //  The question remains what to do about next-stage
  //  expressions during unpacking, type-wise (note that
  //  queueEvent(unpack(x)) is correct, regardless of
  //  the tuple's arity). Let's start with the simple
  //  case first though.

  override def fresh[T: Typ]: Sym[T] =
    if (implicitly[Typ[T]].runtimeClass == classOf[LuaUnboxedTuple[_]]) {
      val typ = implicitly[Typ[T]].typeArguments.head
      typ.typeArguments match {
        // FIXME these first two may crash
        case Nil => fresh(typ).asInstanceOf[Sym[T]]
        case single :: Nil => fresh(single).asInstanceOf[Sym[T]]
        case args =>
          val unboxedSym = new UnboxedSym(
            try nVars finally nVars += 1, Nil
          )(implicitly[Typ[T]])
          unboxedSym.components = args.map {
            t: Typ[_] =>
              // make the unboxed tuple components all read the unboxed sym
              createReflectDefinition(fresh(t), Reflect(
                DummyRead(), Read(List(unboxedSym)), List(unboxedSym)
              ))
          }
          unboxedSym
      }
    } else super.fresh[T]

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case Unpack(x) => boundSyms(x)
    case tup: LuaUnboxedTuple[_] => tup.t.asInstanceOf[Product].productIterator.toList.map {
      case s: Sym[_] => s
      case x => throw new IllegalArgumentException(s"unexpected $x in LuaUnboxedTuple")
    }
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
