package me.viluon.computercraft.dataStructures

import me.viluon.lua.computercraft.CCLibrary

import scala.language.implicitConversions
import scala.lms.common.Record
import scala.reflect.SourceContext

trait LinkedLists extends CCLibrary {
  implicit val complexTyp: Typ[Complex] = manifestTyp

  abstract class Complex() extends Record {
    // records only work in very specific circumstances:
    //  - the fields must be defs or vars, not vals
    //  - the constructor can take parameters,
    //    but they won't translate to anything in the generated code
    //  - default values for fields won't make it to generated code,
    //    they have to be overridden to appear in Lua
    //  - inheritance hierarchies with abstract classes do seem
    //    to work, but only the leaf RHS's of fields appear in
    //    the generated code
    //  - the class itself may be concrete or abstract,
    //    but it cannot be a case class and it cannot be a trait
    //  - methods are inaccessible via Rep[T], although one
    //    can work around this using implicit classes
    //  - isInstanceOf[T] disappears in generated code and doesn't
    //    work correctly during constant folding
    // almost everything outside these constraints either crashes
    // the typechecker, doesn't typecheck, crashes at codegen time
    // (class cast exception trying to cast to a RefinedManifest,
    // typical of traits and val usage), or (worst of all) gives
    // unintended results, such as a field missing in generated
    // code.
    def real: Double
    var imaginary: Double = 0
  }

  implicit class ADTOps[Adt <: ADT[_]](val adt: Adt) {
    implicit class RepSumOps[A, B](val x: Rep[Adt])(implicit ev: adt.TypeMember <:< Either[A, B]) {
      def map[C](f: StagedDynamic => Rep[C])(implicit src: SourceContext): Rep[Either[A, C]] =
        adt.switch(x.asInstanceOf[Rep[adt.type]]) { ops =>
          import ops._

          Left() ~~> (_ => x)
          // FIXME hack, generalise (needs a different interface than StagedDynamic)
          Right("imaginary", "real") ~~> f
          nil.asInstanceOf[Rep[Either[A, C]]]
        }
      def flatMap[C](f: StagedDynamic => Rep[Either[A, C]])(implicit src: SourceContext): Rep[Either[A, C]] =
        adt.switch(x.asInstanceOf[Rep[adt.type]]) { ops =>
          import ops._

          Left() ~~> (_ => x)
          Right("imaginary", "real") ~~> f
          nil.asInstanceOf[Rep[Either[A, C]]]
        }
    }
  }

  def testDSfull(): Rep[Unit] = {
    val x: Rep[Complex] = new Complex {
      override val real = 1.0
      override val imaginary = 2.0
    }
//    println(x)
//    println(x.imaginary)

    // complex number datatype via ADTs
    implicit val ADTComplex = ("real" -> ADTDouble()) * ("imaginary" -> ADTDouble())
    // sadly, we have to call the implicit make_tuple2 manually, I'm not sure why
    val y = ADTComplex(make_tuple2((1.0, 2.0)))
//    println(y)
    // match on the complex number and extract the imaginary part
//    println(((_: Rep[Double], im: Rep[Double]) => im)(y))
    // or do it with dynamics
//    println(ADTComplex.switch(y)(_.imaginary))
    // maybe with a helper?
    def switch[A, Adt <: ADT[A], R: Typ](x: Rep[Adt])(f: SwitchOps[A] => Rep[R])(implicit adt: Adt): Rep[R] =
      adt.switch(x.asInstanceOf[Rep[adt.type]])(f)
    // doesn't help at all...
//    println((switch[(Double, Double), ADT[(Double, Double)], Any](y)(_.imaginary)))

    // TODO maybe there's hope for extension methods via implicit classes for cases like these,
    //  but beware that there are many ADTs of the (Double, Double) type signature. The type inference
    //  would have to pick up on ADTComplex.type and little else. Maybe adding a type member to ADT
    //  would help? (To extract it without having to infer another type parameter at the switch
    //  callsite)

    // the optional complex number datatype
    val OptComplex = ADTs.Option(ADTComplex)
    // the construction here is even more involved :(
    val z1 = OptComplex(Right[Rep[Unit], Rep[(Double, Double)]](make_tuple2((1.0, 2.0))))
    val z2 = OptComplex(eitherToTaggedUnion(Right(make_tuple2((1.0, 2.0)))))

    def right[A](x: Rep[A]): Rep[Either[Nothing, A]] = eitherToTaggedUnion(Right(x))
    def left[A](x: Rep[A]): Rep[Either[A, Nothing]] = eitherToTaggedUnion(Left(x))
    implicit def tup2(t: (Double, Double)): Rep[(Double, Double)] = make_tuple2((t._1, t._2))

    // with a few helpers, the usage gets a lot more ergonomic
    val z3 = OptComplex(right((scala.math.Pi, 4.5)))

    def some[A](x: Rep[A]): Rep[Either[Unit, A]] = eitherToTaggedUnion(Right(x))
    def none[A]: Rep[Either[Unit, A]] = eitherToTaggedUnion(Left(()))

    val z4 = OptComplex(some((1.0, 2.0)))

    // let's add some ops
    val ops = new ADTOps(OptComplex)
    import ops.RepSumOps

    // test them out
//    val z5 = for {
//      a <- z3
//      b <- z4
//    } yield a.real[Double] + b.imaginary[Double]

    OptComplex.switch(OptComplex(some((42.0, 666.0)))) { ops =>
      import ops._
      Left() ~~> { _ =>
        println("nothing here")
      }
      Right("real", "imaginary") ~~> { x =>
        println("real part is " + x.real)
      }
      nil.asInstanceOf[Rep[Either[Unit, Double]]]
    }

    nil
//    println(z4)
//    println(OptComplex(none))
//    println(z5)
  }

  def testDS(): Rep[Unit] = {
    def none[A]: Rep[Either[Unit, A]] = eitherToTaggedUnion(Left(()))
    def some[A](x: Rep[A]): Rep[Either[Unit, A]] = eitherToTaggedUnion(Right(x))
    implicit def tup2(t: (Double, Double)): Rep[(Double, Double)] = make_tuple2((t._1, t._2))

    val ADTComplex = ("real" -> ADTDouble()) * ("imaginary" -> ADTDouble())
    val OptComplex = ADTs.Option(ADTComplex)
    implicit val optComplexTyp: Typ[OptComplex.type] = manifestTyp

    val inspectOptComplex = fun { (maybeN: Rep[OptComplex.type]) =>
      OptComplex.switch(maybeN) { ops =>
        import ops._
        Left() ~~> { _ =>
          println("nothing here")
        }
        Right("real", "imaginary") ~~> { x =>
          println("real part is " + x.real)
        }
        nil
      }
    }

    inspectOptComplex(OptComplex(some((scala.math.Pi, 4.5))))
    inspectOptComplex(OptComplex(none))
  }

//  sealed trait LinkedList[+T]
//  abstract class Cons[+T] extends LinkedList[T] with Struct {
//    val head: T
//    val tail: LinkedList[T]
//  }
//  class Nil() extends LinkedList[Nothing]
//
//  def Cons[T: Typ](_head: Rep[T], _tail: Rep[LinkedList[T]]): Rep[Cons[T]] =
//    new Cons[T] {
//      val head = _head
//      val tail = _tail
//    }

//  object LinkedList {
//    def fromArray[T: Typ](arr: Rep[Array[T]]): Rep[LinkedList[T]] = {
//      def loop(i: Rep[Int] = 0): Rep[LinkedList[T]] = {
//        if (i < arr.length) {
//          val x: Rep[LinkedList[T]] = Cons(arr(i), loop(i + 1))
//          x
//        } else {
//          new Nil()
//        }
//      }
//      loop()
//    }
//  }

//  implicit class LinkedListOps[T: Typ](xs: Rep[LinkedList[T]]) {
//    def reverse: Rep[LinkedList[T]] = {
//      val buf = ArrayBuffer[T]()
//      reverse(buf)
//    }
//
//    def reverse(stack: Rep[ArrayBuffer[T]]): Rep[LinkedList[T]] = {
//      if (xs.isInstanceOf[Nil]) LinkedList.fromArray(stack.toArray)
//      else {
//        val xxs = xs.asInstanceOf[Rep[Cons[T]]]
//        stack.append(xxs.head)
//        xxs.tail.reverse(stack)
//      }
//    }
//  }
}
