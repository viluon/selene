package me.viluon

import scala.lms.common._
import me.viluon.dsl._
import me.viluon.lua.{LuaScala, LuaScalaExp, LuaScalaGen}

object Main {
  object F extends DslDriver[Int, Int] {
    def snippet(x: Rep[Int]): Rep[Int] = {
      def square(x: Rep[Int]): Rep[Int] = x * x
      def power(b: Rep[Int], x: Int): Rep[Int] = x match {
        case 0 => 1
        case n if n % 2 == 0 => square(power(b, n / 2))
        case x => b * power(b, x - 1)
      }

      power(x, 5)
    }
  }

  trait Hello extends LuaScala {
    def main(name: Rep[String]): Rep[Unit] = {
      println(s"Hello, $name!")
    }
  }

  val hello = new Hello with LuaScalaExp

  def main(args: Array[String]): Unit = {
//    println(F.code)
//    println(F.eval(2))

    val luaGen = new LuaScalaGen { val IR: hello.type = hello }
    luaGen.emitSource(hello.main, "main", new java.io.PrintWriter(System.out))
  }
}
