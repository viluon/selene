package me.viluon.lua.codegen

import scala.reflect.SourceContext

trait LuaCoreCodegen extends DummyGen with QuoteGen {
  import IR._
  type LLExpr

  @deprecated("Pass a LLExpr instead", "from now till the end of times")
  def emitValDef(ignore: Sym[Any], ignore2: String): Unit =
    throw new IllegalArgumentException("beware implicit conversions in codegen")

  def originalContext(x: SourceContext): SourceContext =
    x.parent.filter(_.parent.nonEmpty).map(originalContext).getOrElse(x)

  def shortName(str: String): String =
    "^([a-zA-Z]*)".r.findFirstMatchIn(str).map(_.group(0)).map {
      case "String" => "str"
      case "Function" => "fn"
      case "boolean" => "bool"
      case s => s
    }.getOrElse(str)

  override def quote(x: Exp[Any]): String = x match {
    case Const(()) => "nil"
    case s@Sym(n) if s.pos.nonEmpty =>
      val orgContext = originalContext(s.pos.head)
      val nm =
        if (s.tp.runtimeClass == classOf[(_) => _])
          // name functions after the methods that produced them
          orgContext.methodName
        else orgContext
            // try getting the binding from the Scala code
            .bindings.headOption.flatMap({ case (str, _) => Option(str) })
            .map(_.replace('$', '_'))
            // let intermediaries fallback to a name derived from the type
            .getOrElse({
              val name =
                  (if (s.tp.runtimeClass == classOf[Exp[_]])
                    s.tp.typeArguments.head.runtimeClass
                  else s.tp.runtimeClass).getSimpleName

              if (!name.endsWith("[]")) shortName(name)
              else shortName(name.substring(0, name.length - 2)) + "s"
            })
      nm.replace(' ', '_') + q"_$n"
    case _ => super.quote(x)
  }

  def emitValDef(sym: Sym[Any], rhs: LLExpr): Unit
  def emitFunctionBody(body: Block[Any]): Unit
  def emitAssignment(lhs: LLExpr, rhs: LLExpr): Unit
  def emitAssignment(lhs: Exp[Any], rhs: Exp[Any]): Unit
}
