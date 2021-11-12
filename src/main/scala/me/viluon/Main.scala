package me.viluon

import me.viluon.lua._

object Main {
  object Hello extends Retargetable[Int, Int] {
    def square(x: Rep[Int]): Rep[Int] = x * x

    def power(b: Rep[Int], n: Int): Rep[Int] =
      if (n == 0) 1
      else if (n % 2 == 0) square(power(b, n / 2))
      else b * power(b, n - 1)

    def main(b: Rep[Int]): Rep[Int] = {
      power(b, 5)
    }
  }

  def main(args: Array[String]): Unit = {
    println(Hello.lua)
    println(Hello.scala)
  }
}
