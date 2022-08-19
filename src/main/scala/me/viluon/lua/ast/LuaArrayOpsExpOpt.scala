package me.viluon.lua.ast

import scala.lms.common.ArrayOpsExpOpt
import scala.reflect.SourceContext

trait LuaArrayOpsExpOpt extends ArrayOpsExpOpt {
  // inspired by the superclass
  override def array_apply[T: Typ](x: Exp[Array[T]], k: Exp[Int])(implicit pos: SourceContext): Exp[T] = k match {
    case Const(n) =>
      val vs = x.asInstanceOf[Sym[Array[T]]]

      val rhs = for {
        TP(_, rhs) <- findDefinition(vs)
        ArrayFromSeq(xs: Seq[Exp[T]] @unchecked) <- Some(rhs)
        if n <= xs.length && n >= 1
        elem = xs(n - 1)
        Const(_) <- Some(elem)
      } yield elem

      rhs.getOrElse(super.array_apply(x, k))
    case _ => super.array_apply(x, k)
  }
}
