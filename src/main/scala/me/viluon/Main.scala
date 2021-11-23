package me.viluon

import me.viluon.lua.computercraft.CCProgram

import java.io.FileWriter
import scala.reflect.SourceContext

object Main {
  object Hello extends CCProgram {
    //noinspection JavaAccessorEmptyParenCall
    def main(): Rep[Unit] = {
      val term = globalTerm
      val size = term.getSize()
      val (w, h) = (size(1), size(2))
      val (x, y) = (w / 2, h / 2)
      term.setTextColour(Colours.LightBlue)
      term.setBackgroundColour(Colours.Grey)
      val eoq = Array("end_of_queue").asInstanceOf[Rep[Event]]
      Os.queueEvent(eoq)

      while (true) {
        import Events.EventOps
        val ev = Os.pullEvent()
        if (ev.tag == "end_of_queue") {
          Os.queueEvent(eoq)
        }

        term.clear()
        term.setCursorPos(x, y)
        term.write("hello world! " + Os.clock())
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
