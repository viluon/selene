package me.viluon

import me.viluon.Main.Hello
import me.viluon.lua.computercraft.CCProgram

import java.io.FileWriter

object Main {
  object Hello extends CCProgram {
    //noinspection JavaAccessorEmptyParenCall
    def main(): Rep[Unit] = {
      val (x, y) = setUpTerm()

      val t = unpack[(String, String)](Array("hey", "there"))
      os.queueEvent("hello", 420, t)

      handleEvents { (_, arg, _, _, _) =>
        term.clear()
        term.setCursorPos(x, y)
        term.write("hello world! " + os.clock())
        term.setCursorPos(x, y + 1)
        term.write(arg.toString())
      }
    }

    private def handleEvents(f: (Rep[Any], Rep[Any], Rep[Any], Rep[Any], Rep[Any]) => Rep[Unit]): Rep[Unit] =
      handleEventsInternal { case LuaUnboxedTuple((a: Rep[_], b: Rep[_], c: Rep[_], d: Rep[_], e: Rep[_])) =>
        f(a, b, c, d, e)
      }

    private def handleEventsInternal(f: Rep[LuaUnboxedTuple[(Any, Any, Any, Any, Any)]] => Rep[Unit]): Rep[Unit] = {
      val eoq = "end_of_queue"
      os.queueEvent(eoq)

      while (true) {
        val ev = os.pullEvent()
        val LuaUnboxedTuple((tag: Rep[String], _, _, _, _)) = ev
        implicit val w: Typ[Any] = manifestTyp
        if (tag == eoq) {
          os.queueEvent(eoq)
        }

        f(ev)
      }
    }

    private def setUpTerm(): (Rep[Int], Rep[Int]) = {
      val size = term.getSize()
      val w = size(1)
      val h = size(2)
      val x = w / 2
      val y = h / 2
      term.setTextColour(Colours.LightBlue)
      term.setBackgroundColour(Colours.Grey)
      (x, y)
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
