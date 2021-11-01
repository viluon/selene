package me.viluon

import scala.lms.common._
import scala.lms.tutorial.DslDriver

object Main {
  object F extends DslDriver[Int, Int] {
    def snippet(x: Rep[Int]): Rep[Int] = {
      def square(x: Rep[Int]): Rep[Int] = x * x
      def power(b: Rep[Int], x: Int): Rep[Int] = x match {
        case 0 => 1
        case n if n % 2 == 0 => square(power(b, n - 1))
        case x => b * power(b, x - 1)
      }

      power(x, 5)
    }
  }

  def main(args: Array[String]): Unit = {
    println(F.code)
    println(F.eval(2))
  }
}
