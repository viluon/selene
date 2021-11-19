package me.viluon

import me.viluon.lua.computercraft.lang.CCProgram

object Main {
  object Hello extends CCProgram[Unit, Unit] {
    def main(ignore: Rep[Unit]): Rep[Unit] = {
      val term = globalTerm
      term.setTextColour(unit(Colour.Grey))
      term.write("hello world!")
    }
  }

  def main(args: Array[String]): Unit = {
    println(Hello.lua)
//    println(Hello.scala)
  }
}
