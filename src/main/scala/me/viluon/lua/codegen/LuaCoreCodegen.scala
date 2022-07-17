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

  def allocRegisters(liveness: Map[IR.Sym[Any], (Int, Int)], input: Iterable[LLStmt]): (Map[IR.Sym[Any], IR.Sym[Any]], List[LLStmt]) = {
    import collection.mutable
    val regs = (0 to 200).map(_ => fresh(simpleClassTyp[Any](classOf))).toBuffer[Sym[Any]]
    val freeRegs = mutable.ArrayStack(regs: _*)
    val allocation = mutable.Map[IR.Sym[Any], IR.Sym[Any]]()
    val code = mutable.ArrayBuffer[LLStmt]()

    val live = liveness.mapValues(p => p._1 until p._2).toSeq.map(_.swap)
    def liveAt(i: Int): Set[IR.Sym[Any]] = live.filter(_._1 contains i).map(_._2).toSet

    for ((stmt, i) <- input.zipWithIndex) {
      val liveSyms = liveAt(i)
      val toBeFreed = if (i == 0) Set() else liveAt(i - 1).map(allocation) -- liveSyms.collect {
        // look at the syms which have regs allocated and collect those regs
        case sym if allocation contains sym => allocation(sym)
      }
      val toBeAllocated = liveSyms -- allocation.keySet
      toBeFreed.foreach(freeRegs.push)
      toBeAllocated.foreach(sym => allocation(sym) = freeRegs.pop)
      code += substituted(stmt, allocation.toMap.withDefault(identity))
    }

    allocation.toMap -> code.toList
  }

  def emitSource[A: Typ](args: List[IR.Sym[_]], body: Block[A]): (Sym[Any], String, List[(IR.Sym[Any], Any)]) = {
    val entryPoint = fresh // TODO should be named main
    luaCode += LLLocal(entryPoint, Some(LLFunctionHeader(args.map(quote))))
    emitFunctionBody(body)
    luaCode += LLStandalone(l"$entryPoint()")

    val liveness = analyseLiveness
    println(liveness.toList.sortBy(_._1.id).map(p => quote(p._1) -> p._2).mkString("\n"))
    println(s"liveness info gathered for ${liveness.size} out of $nVars variables")

    val (allocation, code) = allocRegisters(liveness, luaCode)
    val usedRegs = allocation.values.toSet
    val withDeclarations = usedRegs.toList.sortBy(_.id).map(LLLocal(_, None)) ++ code.map {
      case LLLocal(reg, Some(rhs)) if usedRegs contains reg => LLAssign(l"$reg", rhs)
      case x => x
    }.filter {
      case LLLocal(reg, None) if usedRegs contains reg => false
      case LLAssign(LLExpr(lhs, _), LLExpr(rhs, _)) if lhs == rhs => false
      case _ => true
    }

    (entryPoint, withDeclarations.map(_.asLua).mkString("\n"), getFreeDataBlock(body))
  }

  private def analyseLiveness[A: Typ] = {
    def symsIn(xs: Iterable[Any]): Set[Sym[Any]] = xs.collect {
      case s: Sym[_] => Set(s)
      case expr: LLExpr => expr.uses.toSet
      case it: Iterable[_] => it.map(Set(_)).flatMap(symsIn).toSet
      case prod: Product => prod.productIterator.map(Set(_)).flatMap(symsIn).toSet
    }.toSet.flatten

    // TODO a sym with a use edge crossing a loop or function boundary
    //  needs to have its range extended to the end of the loop or function
    //  (actually, for functions it's even worse as upvalues can escape via closures!)

    symsIn(luaCode).map(sym => sym -> {
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
