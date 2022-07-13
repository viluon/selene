package me.viluon.lua.ast

import scala.collection.mutable.ArrayBuffer
import scala.lms.common.ArrayBufferOpsExp
import scala.reflect.SourceContext

trait LuaArrayBufferOpsExp extends ArrayBufferOpsExp {
  override def arraybuffer_toarray[A: Typ](x: Exp[ArrayBuffer[A]])(implicit pos: SourceContext): Exp[Array[A]] =
    reflectMutable(ArrayBufferToArray(x))
}
