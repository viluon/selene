package me.viluon

import scala.lms.common._
import me.viluon.dsl._
import me.viluon.lua.{LuaExpGen, LuaScala, LuaScalaExp, ScalaGen, LuaScalaGen => LuaGen}

import java.io.{PrintWriter, StringWriter}

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
      println("Hello, " + name + "!")
    }
  }

  trait Ugh extends LuaExpGen with Hello {
    lazy val code: String = {
      val source = new StringWriter()
      codegen.emitSource(main, "main", new PrintWriter(source))(manifestTyp[String], manifestTyp[Unit])
      source.toString
    }
  }

//  val hello = new Hello with LuaGen

  def main(args: Array[String]): Unit = {
    //    println(F.code)
    //    println(F.eval(2))

    //    val luaGen = new LuaGen { val IR: hello.type = hello }
    //    luaGen.emitSource(hello.main, "main", new PrintWriter(System.out))

    //    val scalaGen = new ScalaGen { val IR: hello.type = hello }
    //    scalaGen.emitSource(hello.main, "main", new PrintWriter(System.out))(manifestTyp[String], manifestTyp[Unit])


    val ugh = new Ugh {}
    println(ugh.code)
  }
}
