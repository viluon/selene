package me.viluon.computercraft

import me.viluon.lua.computercraft.CCLibrary

trait FunctionalRendering extends CCLibrary {
  import Colours.Colour
  type Pixel = Double

  def encode(bg: Rep[Colour] = Colours.White, fg: Rep[Colour] = Colours.Black, ch: Rep[String] = " "): Rep[Pixel] = {
    val b = bg.toInt.toDouble
    val f = fg.toInt.toDouble
    val c = string.byte(ch).toDouble
    b * 256 + f + c / 256
  }

  def decode(px: Rep[Pixel]): (Rep[Colour], Rep[Colour], Rep[String]) = {
    val frac_ch = px % 1
    val shifted = px / 256
    val frac_fg_ch = shifted % 1
    val bg = shifted - frac_fg_ch
    val fg = (px % 256) - frac_ch
    val ch = frac_ch * 256
    (bg.toColour, fg.toColour, string.char(ch.asInstanceOf[Rep[Int]]))
  }

  type Image = (Rep[Int], Rep[Int]) => Rep[Pixel]

  def solid(px: Rep[Pixel]): Image = (_x, _y) => px

  def checker(white: Image, black: Image, w: Int, h: Int): Image = (x, y) => {
    val i = x / w
    val j = y / h
    if ((i + j) % 2 == 0) white(x, y) else black(x, y)
  }

  def square[T: Typ : Numeric](x: Rep[T]): Rep[T] = x * x

  def circle(inner: Image, outer: Image, centre: (Rep[Int], Rep[Int]), radius: Rep[Double]): Image = (x, y) =>
    if ((square(x - centre._1) + square(y - centre._2)).toDouble < square(radius)) inner(x, y)
    else outer(x, y)

  /**
   * Shrink an image using background colours and teletext characters.
   * Doesn't give the prettiest results, but runs fast.
   */
  def fastShrink(img: Image): Image = { (x, y) =>
    val points = for {
      // ranges would be implicitly converted to Lua representations,
      // so we annotate them as Scala ranges explicitly. Iterations over
      // Scala ranges will be staged, erasing the loops.
      yOffset <- 0 until 3: Range
      xOffset <- 0 until 2: Range
    } yield decode(img(x * 2 + xOffset, y * 3 + yOffset))._1

    var sub: Var[Int] = 15
    var char: Var[Int] = 0

    for (i <- 0 until 5: Range) {
      if (points(i) != points(5)) {
        sub = points(i).toInt
        char = readVar(char) + scala.math.pow(2.0, i.toDouble).toInt
      }
    }

    val b = points(5).toInt.toDouble
    val f = readVar(sub).toDouble
    val c = (readVar(char) + 128).toDouble
    b * 256 + f + c / 256
  }

  def render(img: Image): Rep[Unit] = {
    val LuaUnboxedTuple((w, h)) = term.getSize()
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

  def setUpTerm(): (Rep[Int], Rep[Int], Rep[Int], Rep[Int]) = {
    val LuaUnboxedTuple((w, h)) = term.getSize()
    val x = w / 2
    val y = h / 2
    term.setTextColour(Colours.LightBlue)
    term.setBackgroundColour(Colours.Grey)
    (w, h, x, y)
  }
}
