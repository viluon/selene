package me.viluon

import scala.lms.common._
import me.viluon.dsl._
import me.viluon.lua.{LuaExpGen, LuaScala, LuaScalaExp, ScalaGen, LuaScalaGen => LuaGen}

import java.io.{PrintWriter, StringWriter}

object Main {
  object F extends DslDriver[Int, Int] {
    def snippet(x: Rep[Int]): Rep[Int] = {
      def power(b: Rep[Int], x: Int): Rep[Int] =
        if (x == 0) 1 else b * power(b, x - 1)

      power(x, 5)
    }
  }

  object Hello extends LuaExpGen {
    def main(b: Rep[Int]): Rep[Int] = {
      def square(x: Rep[Int]): Rep[Int] = x * x

      def power(b: Rep[Int], n: Int): Rep[Int] =
        if (n == 0) 1
        else if (n % 2 == 0) square(power(b, n / 2))
        else b * power(b, n - 1)

      power(b, 5)
    }
  }

  def main(args: Array[String]): Unit = {
    println(F.code)
    println(Hello.code)
  }
}
