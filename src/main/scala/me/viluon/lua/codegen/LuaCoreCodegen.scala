package me.viluon.lua.codegen

import me.viluon.lua.codegen.lowLevel.{LLStmt, LLStmtOps}

import scala.collection.mutable.ArrayBuffer
import scala.reflect.SourceContext

trait LuaCoreCodegen extends DummyGen with QuoteGen with LLStmtOps {
  import IR._

  var luaCode: ArrayBuffer[LLStmt] = new ArrayBuffer()

  def emitValDef(sym: Sym[Any], rhs: LLExpr): Unit = luaCode += {
    // FIXME this should be done in sth like a DCE pass
    if (sym.tp.<:<(ManifestTyp(manifest[Unit]))) LLStandalone(rhs)
    else LLLocal(sym, Some(rhs))
  }

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

  override def emitBlock(y: Block[Any]): Unit = {
    //    stream.println("do -- emitBlock - is this correct?")
    super.emitBlock(y)
    //    stream.println("end")
  }

  def emitFunctionBody(body: Block[Any]): Unit = {
    emitBlock(body)
    val result = getBlockResult(body)
    if (!(result.tp <:< ManifestTyp(manifest[Unit]))) {
      luaCode += LLReturn(l"$result")
    }
    luaCode += LLEnd()
  }

  def emitSource[A: Typ](args: List[IR.Sym[_]], body: Block[A]): (Sym[Any], String, List[(IR.Sym[Any], Any)]) = {
    val entryPoint = fresh // TODO should be named main
    luaCode += LLLocal(entryPoint, Some(LLFunctionHeader(args.map(quote))))
    emitFunctionBody(body)

    val liveness = analyseLiveness
    println(liveness.toList.sortBy(_._1.id).map(p => quote(p._1) -> p._2).mkString("\n"))
    println(s"liveness info gathered for ${liveness.size} out of $nVars variables")

    (entryPoint, luaCode.map(_.asLua).mkString("\n"), getFreeDataBlock(body))
  }

  private def analyseLiveness[A: Typ] = {
    def symsIn(xs: Iterable[Any]): Set[Sym[Any]] = xs.collect {
      case expr: LLExpr => expr.uses.toSet
      case it: Iterable[_] => it.map(Set(_)).flatMap(symsIn).toSet
      case prod: Product => prod.productIterator.map(Set(_)).flatMap(symsIn).toSet
    }.toSet.flatten
    val syms = symsIn(luaCode)
    syms.map(sym => sym -> {
      val start = luaCode.zipWithIndex.find {
        case (stmt, _) => symsIn(List(stmt)).contains(sym)
      }.get._2
      val end = luaCode.zipWithIndex.reverse.find {
        case (stmt, _) => symsIn(List(stmt)).contains(sym)
      }.get._2
      start -> end
    }).toMap
  }

  def emitAssignment(lhs: Exp[Any], rhs: Exp[Any]): Unit = emitAssignment(l"$lhs", l"$rhs")
  def emitAssignment(lhs: LLExpr, rhs: LLExpr): Unit = luaCode += LLAssign(lhs, rhs)
}
