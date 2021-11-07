package me.viluon

import me.viluon.dsl._
import me.viluon.lua.LuaDSL

object Main {
  object F extends DslDriver[Int, Int] {
    def snippet(x: Rep[Int]): Rep[Int] = {
      def power(b: Rep[Int], x: Int): Rep[Int] =
        if (x == 0) 1 else b * power(b, x - 1)

      power(x, 5)
    }
  }

  object Hello extends LuaDSL[Int, Int] {
    def main(b: Rep[Int]): Rep[Int] = {
      def square(x: Rep[Int]): Rep[Int] = x * x

      def power(b: Rep[Int], n: Int): Rep[Int] =
        if (n == 0) 1
        else if (n % 2 == 0) square(power(b, n / 2))
        else b * power(b, n - 1)

      def foo(a: Rep[Int], b: Rep[Int]): Rep[Int] = a + b
      def oof(a: Rep[Int], b: Rep[Int]): Rep[Int] = a - b

      val bar = if (b > 3) foo _ else oof _
      println(bar(b, 3))

      power(b, 5)
    }
  }

  def main(args: Array[String]): Unit = {
    println(F.code)
    println(Hello.code)
  }
}
