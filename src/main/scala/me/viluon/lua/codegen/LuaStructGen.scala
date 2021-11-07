package me.viluon.lua.codegen

import scala.lms.common.StructExp
import scala.reflect.NameTransformer

trait LuaStructGen extends BaseGen with QuoteGen {
  val IR: StructExp
  import IR._

  private def accessString(name: String, dot: String = "."): String = {
    val n = NameTransformer.decode(name)
    if (n == name) s"$dot$n" else "[\"" + n + "\"]"
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case Struct(_tag, fields) => emitValDef(sym, "{\n" + fields.map({
      case (name, value) => q"\t${accessString(name, "")} = $value;\n"
    }) + "}")
    case FieldApply(struct, index) => emitValDef(sym, q"$struct${accessString(index)}")
    case _ => super.emitNode(sym, rhs)
  }
}
