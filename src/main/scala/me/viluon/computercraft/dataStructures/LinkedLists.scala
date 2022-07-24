package me.viluon.computercraft.dataStructures

import me.viluon.lua.computercraft.CCLibrary

import scala.lms.common.Record

trait LinkedLists extends CCLibrary {
  implicit val complexTyp: Typ[Complex] = manifestTyp

  abstract class Complex() extends Record {
    // records only work in very specific circumstances:
    //  - the fields must be defs or vars, not vals
    //  - the constructor can take parameters,
    //    but they won't translate to anything in the generated code
    //  - default values for vars won't make it to generated code,
    //    they have to be overridden to appear in Lua
    //  - the class itself may be concrete or abstract,
    //    but it cannot be a case class and it cannot be a trait
    //  - methods are inaccessible via Rep[T], although one
    //    can work around this using implicit classes
    // almost everything else either doesn't typecheck, crashes
    // at codegen time (class cast exception trying to cast to a
    // RefinedManifest, typical of traits and val usage), or
    // (worst of all) gives unintended results, such as a field
    // missing in generated code.
    def real: Double
    var imaginary: Double = 0
  }

  def testDS(): Rep[Unit] = {
    val x: Rep[Complex] = new Complex {
      override val real = 1.0
      override val imaginary = 2.0
    }
    println(x)
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
