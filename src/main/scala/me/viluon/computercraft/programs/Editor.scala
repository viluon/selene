package me.viluon.computercraft.programs

import me.viluon.computercraft.{EventHandling, FunctionalRendering}
import me.viluon.lua.computercraft.CCProgram

import scala.lms.common.Record

object Editor extends CCProgram
  with EventHandling
  with FunctionalRendering {

  implicit val stateTyp: Typ[EditorState] = manifestTyp
  abstract class EditorState extends Record {
    def buffer: String
    def cursor: Int
  }

  type M[A] = Rep[EditorState] => (Rep[EditorState], Rep[A])
  implicit class MOps[A](m: M[A]) {
    def map[B](f: Rep[A] => Rep[B]): M[B] = s1 => {
      val (s2, a) = m(s1)
      (s2, f(a))
    }
    def flatMap[B](f: Rep[A] => M[B]): M[B] = s1 => {
      val (s2, a) = m(s1)
      f(a)(s2)
    }
    def withFilter(p: Rep[A] => Boolean): M[A] = s1 => {
      val (s2, a) = m(s1)
      if (!p(a)) throw new IllegalArgumentException("withFilter predicate failed")
      (s2, a)
    }
  }

  def get: M[EditorState] = s => (s, s)
  def put(s: Rep[EditorState]): M[Unit] = _ => (s, nil)
  def getCursor: M[Int] = s => (s, s.cursor)
  def setCursor(c: Rep[Int]): M[Unit] = s => (new EditorState {
    override val buffer = s.buffer
    override val cursor = c
  }, nil)
  def getBuffer: M[String] = s => (s, s.buffer)
  def setBuffer(b: Rep[String]): M[Unit] = s => (new EditorState {
    override val buffer = b
    override val cursor = s.cursor
  }, nil)
  // necessary, because the for comprehension transformation doesn't work
  // for let bindings (x = foo rather than x <- foo) with staged functions
  def pure[A](a: Rep[A]): M[A] = s => (s, a)

  /**
   * Zero-based substring.
   */
  def sub(s: Rep[String], i: Rep[Int], j: Rep[Int]): Rep[String] = string.sub(s, i + 1, j + 1)

  def ins(str: Rep[String]): M[Unit] = for {
    buf <- getBuffer
    cur <- getCursor
    prefix <- pure(sub(buf, 0, cur - 1))
    suffix <- pure(sub(buf, cur, string.length(buf) - 1))
    _ <- setBuffer(prefix + str + suffix)
    _ <- setCursor(cur + string.length(str))
  } yield nil

  def insert(str: Rep[String]): M[Unit] = state => ({
    val buf = state.buffer: Rep[String]
    val cur = state.cursor: Rep[Int]
    val prefix = string.sub(buf, 1, cur - 1)
    val suffix = string.sub(buf, cur, string.length(buf))
    new EditorState {
      override val buffer = prefix + str + suffix
      override val cursor = cur + string.length(str)
    }
  }, nil)

  def delete(n: Rep[Int]): M[Unit] = state => ({
    val buf = state.buffer: Rep[String]
    val cur = state.cursor: Rep[Int]
    val m = math.max(0, math.min(string.length(buf), n)).asInstanceOf[Rep[Int]]
    val prefix = string.sub(buf, 1, math.max(1, cur - m - 1).asInstanceOf[Rep[Int]])
    val suffix = string.sub(buf, cur, string.length(buf))
    new EditorState {
      override val buffer = prefix + suffix
      override val cursor = cur - m
    }
  }, nil)

  sealed trait Motion
  object Motion {
    case object Left extends Motion
    case object Right extends Motion
  }

  def screen(w: Rep[Int], h: Rep[Int])(state: Rep[EditorState]): Image = { (x, y) =>
    if (y == 1) {
      val ch = string.sub(state.buffer, x, x)
      val bg = if (x == state.cursor) Colours.Cyan else Colours.White
      encode(bg = bg, ch = if (ch == nil || ch == "") " " else ch)
    } else text(1, h, "cursor: " + state.cursor)(x, y)
  }

  def motion(motion: Motion): M[Unit] = state => {
    val cur = state.cursor: Rep[Int]
    val newCursor: Rep[Int] = motion match {
      case Motion.Left =>
        if (cur <= 1) 1 else cur - 1
      case Motion.Right =>
        if (cur >= string.length(state.buffer)) cur else cur + 1
    }
    (new EditorState {
      override val buffer = state.buffer
      override val cursor = newCursor
    }, nil)
  }

  override def main(): Rep[Unit] = {
    var state: Var[EditorState] = new EditorState {
      override val buffer = "Hello, World!"
      override val cursor = 1
    }

    val (w, h, _, _) = setUpTerm()

    handleEvents { (tag, a, b, c, d) =>
      def unchanged = make_tuple2(readVar(state) -> nil)
      def next[A: Typ](m: M[A]) = make_tuple2(m(state))

      val r: Rep[(EditorState, Any)] =
        if (tag == "char") {
          next(insert(a.asInstanceOf[Rep[String]]))
        } else if (tag == "key") {
          val k = a.asInstanceOf[Rep[Keys.Key]]
          if (k == Keys.left) {
            next(motion(Motion.Left))
          } else if (k == Keys.right) {
            next(motion(Motion.Right))
          } else if (k == Keys.backspace) {
            next(delete(1))
          } else unchanged
        } else unchanged
      val (newState, _) = r: (Rep[EditorState], _)
      state = newState
      render(screen(w, h)(newState))
    }
  }
}
