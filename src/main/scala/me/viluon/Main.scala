package me.viluon

import me.viluon.computercraft.dataStructures.LinkedLists
import me.viluon.computercraft.programs.Editor
import me.viluon.computercraft.{Compression, EventHandling, FunctionalRendering}
import me.viluon.lua.computercraft.CCProgram

import java.io.FileWriter

object Main {
  object Hello extends CCProgram
    with FunctionalRendering
    with EventHandling
    with Compression {

    def main(): Rep[Unit] = {
      val (w, h, x, y) = setUpTerm()

      handleEvents { (tag, arg, _, _, _) =>
        val grey = encode(Colours.Grey, Colours.White, "x")
        val white = encode(Colours.White, Colours.Black, ".")
        val image = circle(
          solid(grey),
          solid(white),
          (20, 20),
          3 * (math.sin(2 * os.clock()) + 1) + 3
        )
        render(fastShrink(image))
      }
    }
  }

  def main(args: Array[String]): Unit = {
//    println(Hello.lua)
//    val fw = new FileWriter("/tmp/hello.lua")
//    fw.write(Hello.lua)
//    fw.close()
//    val fw2 = new FileWriter("/tmp/hello-unallocated.lua")
//    fw2.write(Hello.compiled._1)
//    fw2.close()

//    println(Hello.scala)
//    Hello.testLzwCompression()

//    println(new CCProgram with LinkedLists {
//      override def main(): Exp[Unit] = testDS()
//    }.lua)

    println(Editor.lua)
    val fw = new FileWriter("/tmp/editor.lua")
    fw.write(Editor.compiled._1)
    fw.close()
  }
}
