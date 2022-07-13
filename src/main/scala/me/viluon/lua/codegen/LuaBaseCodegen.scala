package me.viluon.lua.codegen

import scala.lms.internal.GenericCodegen
import scala.reflect.SourceContext

trait LuaBaseCodegen extends GenericCodegen with QuoteGen {

  import IR._

  import java.io.PrintWriter

  override def emitValDef(sym: Sym[Any], rhs: String): Unit = stream.println({
    // FIXME this should be done in sth like a DCE pass
    if (sym.tp.<:<(ManifestTyp(manifest[Unit]))) rhs
    else q"local $sym = $rhs"
  })

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

  override def emitBlock(y: Block[Any]): Unit = {
    //    stream.println("do -- emitBlock - is this correct?")
    super.emitBlock(y)
    //    stream.println("end")
  }

  def emitFunctionBody(body: Block[Any]): Unit = {
    emitBlock(body)
    val result = getBlockResult(body)
    if (!(result.tp <:< ManifestTyp(manifest[Unit]))) {
      stream.println(q"return $result")
    }
    stream.println("end")
  }

  override def emitSource[A: Typ](args: List[IR.Sym[_]],
                                  body: Block[A],
                                  className: String,
                                  out: PrintWriter): List[(IR.Sym[Any], Any)] = {
    val argsStr = args.map(quote).mkString(", ")

    withStream(out) {
      stream.println("function " + (if (className.isEmpty) "" else className) + s"($argsStr)")
      emitFunctionBody(body)
      stream.flush()
    }

    getFreeDataBlock(body)
  }

  //  def emitExecution[A: Manifest](a: => Exp[A], out: PrintWriter): List[(Sym[Any], Any)] = {
  //    val b = reifyBlock(a)
  //    out.print("(")
  //    emitSource(Nil, b, "", out)
  //    out.println(")()")
  //    getFreeDataBlock(b)
  //  }

  override def emitAssignment(lhs: Sym[Any], rhs: String): Unit = {
    stream.println(q"$lhs = $rhs")
  }
}
