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
    case Struct(_tag, fields) =>
      val str = "{\n" + fields.map({
        case (name, value) => q"\t${accessString(name, "")} = $value;\n"
      }) + "}"
      val uses = fields.map(_._2).flatMap(syms).toList
      emitValDef(sym, LLExpr(str, uses))
    case FieldApply(struct, index) =>
      val uses = syms(struct) ++ syms(index)
      emitValDef(sym, LLExpr(q"$struct${accessString(index)}", uses))
    case _ => super.emitNode(sym, rhs)
  }
}
