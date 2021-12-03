package me.viluon

import me.viluon.lua.computercraft.CCProgram

import java.io.FileWriter

object Main {
  object Hello extends CCProgram {
    //noinspection JavaAccessorEmptyParenCall
    def main(): Rep[Unit] = {
      val size = term.getSize()
      val w = size(1)
      val h = size(2)
      val x = w / 2
      val y = h / 2
      term.setTextColour(Colours.LightBlue)
      term.setBackgroundColour(Colours.Grey)
      val eoq: Rep[String] = "end_of_queue"
      os.queueEvent(eoq)

      os.queueEvent("hello", "world", "!")

      while (true) {
        import Events.EventOps
        val ev = os.pullEvent()
        if (ev.tag == eoq) {
          os.queueEvent(eoq)
        }

        term.clear()
        term.setCursorPos(x, y)
        term.write("hello world! " + os.clock())
        term.setCursorPos(x, y + 1)
        term.write(ev.tag)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(Hello.lua)
    val fw = new FileWriter("/tmp/hello.lua")
    fw.write(Hello.lua)
    fw.close()
//    println(Hello.scala)
  }
}
