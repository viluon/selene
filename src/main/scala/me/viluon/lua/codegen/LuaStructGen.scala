package me.viluon.lua.codegen

import scala.lms.common.StructExp
import scala.reflect.NameTransformer

trait LuaStructGen extends BaseGen with QuoteGen {
  val IR: StructExp
  import IR._

  private def accessString(name: String, dot: String = "."): String = {
      val nm = name
      val decoded = NameTransformer.decode(nm)
      if (decoded == nm) s"$dot$decoded" else "[\"" + decoded + "\"]"
    }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case Struct(_tag, fields) =>
      emitValDef(sym, l"{\n" + fields.map({
        case (name, value) => l"\t${accessString(name, "")} = $value;\n"
      }).reduceOption(_ + _).getOrElse(l"") + l"}")
    case FieldApply(struct, index) =>
      emitValDef(sym, l"$struct${accessString(index)}")
    case _ => super.emitNode(sym, rhs)
  }
}
