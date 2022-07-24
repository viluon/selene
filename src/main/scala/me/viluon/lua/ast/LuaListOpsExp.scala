package me.viluon.lua.ast

import scala.lms.common._
import scala.reflect.SourceContext

trait LuaListOpsExp extends BaseExp with ListOpsExp with While with BooleanOps with TupleOps {
  implicit class ListOps[A: Typ](xs: Rep[List[A]]) {
    def nonEmpty(implicit ctx: SourceContext): Rep[Boolean] = !xs.isEmpty

    def reverse(implicit ctx: SourceContext): Rep[List[A]] = xs.foldRight(List[A]()) { t =>
      val (x, xs) = t2(t)
      x :: xs
    }

    def foreach(f: Rep[A] => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit] = {
      var ys = xs
      while (ys.nonEmpty) {
        val x = ys.head
        f(x)
        ys = ys.tail
      }
    }

    def foldRight[B: Typ](z: Rep[B])(f: Rep[(A, B)] => Rep[B])(implicit ctx: SourceContext): Rep[B] = {
      var ys = xs
      var acc = z
      while (ys.nonEmpty) {
        val x = ys.head
        acc = f((x, acc))
        ys = ys.tail
      }
      acc
    }
  }
}
