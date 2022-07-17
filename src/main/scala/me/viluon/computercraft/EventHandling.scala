package me.viluon.computercraft

import me.viluon.lua.computercraft.CCLibrary

trait EventHandling extends CCLibrary {
  def handleEvents(f: (Rep[String], Rep[Any], Rep[Any], Rep[Any], Rep[Any]) => Rep[Unit]): Rep[Unit] =
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
}
