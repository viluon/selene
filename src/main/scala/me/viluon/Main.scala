package me.viluon

import me.viluon.Main.Hello
import me.viluon.lua.computercraft.CCProgram

import java.io.FileWriter
import scala.reflect.SourceContext

object Main {
  object Hello extends CCProgram {
    import Colours.Colour
    type Pixel = Double

    def encode(bg: Rep[Colour] = Colours.White, fg: Rep[Colour] = Colours.Black, ch: Rep[String] = " "): Rep[Pixel] = {
      val b = bg.toInt.toDouble()
      val f = fg.toInt.toDouble()
      val c = string.byte(ch).toDouble()
      b * 256 + f + c / 256
    }

    def decode(px: Rep[Pixel]): (Rep[Colour], Rep[Colour], Rep[String]) = {
      val frac_ch = %(px, 1)
      val shifted = px / 256
      val frac_fg_ch = %(shifted, 1)
      val bg = shifted - frac_fg_ch
      val fg = %(px, 256) - frac_ch
      val ch = frac_ch * 256
      (bg.toColour, fg.toColour, string.char(ch.asInstanceOf[Rep[Int]]))
    }

//    case class Buffer(w: Rep[Int], h: Rep[Int]) {
//      var store: Var[Array[Pixel]] = array_obj_new[Pixel](w * h)
//
//      def clear(px: Pixel): Buffer = {
//        var i: Var[Int] = 1
//        while (i <= w * h) {
//          store(readVar(i)) = px
//          i += 1
//        }
//        this
//      }
//
//      def for_each(f: (Rep[Int], Rep[Int], Rep[Pixel]) => Rep[Unit]): Unit = {
//        var x: Var[Int] = 1
//        var y: Var[Int] = 1
//        while (y <= h) {
//          while (x <= w) {
//            f(readVar(x), readVar(y), store(readVar(y) * w + readVar(x)))
//            x += 1
//          }
//          y += 1
//        }
//      }
//
//      def render(): Rep[Unit] = for_each { (x, y, px) =>
//        val (bg, fg, ch) = decode(px)
//        term.setCursorPos(x, y)
//        term.setBackgroundColour(bg)
//        term.setTextColour(fg)
//        term.write(ch)
//      }
//
//      def write(x: Rep[Int], y: Rep[Int], bg: Rep[Colour], fg: Rep[Colour], text: Rep[String]): Unit = {
//        var i: Var[Int] = 0
//        while (readVar(i) <= string_length(text) && x + readVar(i) <= w) {
//          val index = y * w + x + readVar(i)
//          store(index) = encode(bg, fg, string_charAt(text, readVar(i)).asInstanceOf[Rep[String]])
//          i += 1
//        }
//      }
//    }

    type Image = (Rep[Int], Rep[Int]) => Rep[Pixel]

    def solid(px: Rep[Pixel]): Image = (_x, _y) => px

    def square[T: Typ : Numeric](x: Rep[T]): Rep[T] = x * x

    def circle(inner: Image, outer: Image, centre: (Rep[Int], Rep[Int]), radius: Rep[Double]): Image = (x, y) =>
      if ((square(x - centre._1) + square(y - centre._2)).toDouble() < square(radius)) inner(x, y)
      else outer(x, y)

    def shrink(img: Image): Image = { (x, y) =>
      val uniqueColours = ArrayBuffer[Colour]()
      val buckets = ArrayBuffer[Int]()
      val pixel = ArrayBuffer[Colour]()
      for {
        xOffset <- 0 until 2
        yOffset <- 0 until 3
      } {
        val (c, _, _) = decode(img(x + xOffset, y + yOffset))
        if (buckets(c.toInt) == nil) {
          buckets.toArray(c.toInt) = 0
          // FIXME uniqueColours isn't marked as mutable
          uniqueColours += c
        }
        buckets.toArray(c.toInt) = buckets.toArray(implicitly)(c.toInt) + 1
        pixel.toArray(1 + xOffset + 2 * yOffset) = c
      }

      val bg: Rep[Colour] =
        if (uniqueColours.toArray.length == 1)
          uniqueColours.toArray(implicitly)(1)
        else Colours.Red

      encode(bg, Colours.White, " ")
    }

    def render(img: Image): Rep[Unit] = {
      val size = term.getSize()
      val (w, h) = (size(1), size(2))
      var y: Var[Int] = 1
      while (readVar(y) <= h) {
        var x: Var[Int] = 1
        val yValue = readVar(y)
        term.setCursorPos(1, yValue)
        while (readVar(x) <= w) {
          val (bg, fg, ch) = decode(img(readVar(x), yValue))
          term.setBackgroundColour(bg)
          term.setTextColour(fg)
          term.write(ch)
          x += 1
        }
        y += 1
      }
    }

    //noinspection JavaAccessorEmptyParenCall
    def main(): Rep[Unit] = {
      val (w, h, x, y) = setUpTerm()

//      val t = unpack[(String, String)](Array("hey", "there"))
//      os.queueEvent("hello", 420, t)

      handleEvents { (tag, arg, _, _, _) =>
        val grey = encode(Colours.Grey, Colours.White, "x")
        val white = encode(Colours.White, Colours.Black, ".")
        render(
          shrink(
            circle(
              solid(grey),
              solid(white),
              (x, y),
              3 * (math.sin(2 * os.clock()) + 1) + 3
            )
          )
        )
      }
    }

    private def handleEvents(f: (Rep[String], Rep[Any], Rep[Any], Rep[Any], Rep[Any]) => Rep[Unit]): Rep[Unit] =
      handleEventsInternal { case LuaUnboxedTuple((a: Rep[String], b: Rep[_], c: Rep[_], d: Rep[_], e: Rep[_])) =>
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

    private def setUpTerm(): (Rep[Int], Rep[Int], Rep[Int], Rep[Int]) = {
      val size = term.getSize()
      val w = size(1)
      val h = size(2)
      val x = w / 2
      val y = h / 2
      term.setTextColour(Colours.LightBlue)
      term.setBackgroundColour(Colours.Grey)
      (w, h, x, y)
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
