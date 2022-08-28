package me.viluon.lua.codegen

import me.viluon.lua.LuaUtils
import me.viluon.lua.codegen.lowLevel.{LLStmt, LLStmtImplGen}

import scala.collection.mutable.ArrayBuffer

trait LuaRegisterAllocation extends BaseGen with LLStmtImplGen with LuaUnpackGen {
  import IR._

  var luaCode: ArrayBuffer[LLStmt] = new ArrayBuffer()

  def inScope(body: => Unit): ArrayBuffer[LLStmt] = {
    val oldCode = luaCode
    luaCode = new ArrayBuffer[LLStmt]
    body
    val newCode = luaCode
    luaCode = oldCode
    newCode
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

  def emitSource[A: Typ](args: List[IR.Sym[_]], body: Block[A]): (Sym[Any], String, String, List[(IR.Sym[Any], Any)]) = {
    val entryPoint = fresh[Unit] // TODO should be named main
    val mainBody = inScope(emitFunctionBody(body))
    luaCode += LLLocal(entryPoint, Some(LLFunctionHeader(args.map(quote), mainBody.size)))
    luaCode ++= mainBody
    luaCode += LLStandalone(l"$entryPoint()")

    val liveness = analyseLiveness
    println(liveness.toList.sortBy(_._1.id).map(p => quote(p._1) -> p._2).mkString("\n"))
    println(s"liveness info gathered for ${liveness.size} out of $nVars variables")

    println("\nbefore register allocation:")
    val unallocated = LuaUtils.formatLua(luaCode.map(_.asLua).mkString("\n"))
    println(unallocated)

    val (allocation, code) = allocRegisters(liveness, luaCode)
    val dummy = fresh(unitTyp)
    val usedRegs = allocation.values.toSet + dummy
    val withDeclarations = LLManyLocals(usedRegs.toList.sortBy(_.id)) +: code.map {
      case LLLocal(unboxed: UnboxedSym[_], Some(rhs))
        if (usedRegs.intersect(unboxed.components.toSet)).nonEmpty =>
        val components = unboxed.components.map(s => if (usedRegs(s)) s else dummy)
        LLAssign(LLExpr.fromSeq(components, "", ", ", ""), rhs)
      case LLLocal(reg, Some(rhs)) if usedRegs contains reg => LLAssign(l"$reg", rhs)
      case x => x
    }.filter {
      case LLLocal(reg, None) if usedRegs contains reg => false
      case LLAssign(LLExpr(lhs, _), LLExpr(rhs, _)) if lhs == rhs => false
      case _ => true
    }

    val finalCode = withDeclarations.map(_.asLua).mkString("\n")
    (entryPoint, unallocated, finalCode, getFreeDataBlock(body))
  }

  private def analyseLiveness = {
    def symsIn(xs: Iterable[Any]): Set[Sym[Any]] = xs.collect {
      case unboxed: UnboxedSym[_] => unboxed.components.toSet
      case s: Sym[_] => Set(s)
      case expr: LLExpr => expr.uses.toSet
      case it: Iterable[_] => it.map(Set(_)).flatMap(symsIn).toSet
      case prod: Product => prod.productIterator.map(Set(_)).flatMap(symsIn).toSet
    }.toSet.flatten

    def sliceReferences(start: Int, end: Int, sym: IR.Sym[Any]) = {
      luaCode.slice(start, end).flatMap(x => symsIn(Set(x))).toSet.contains(sym)
    }

    symsIn(luaCode).map(sym => sym -> {
      val start = luaCode.zipWithIndex.find {
        case (stmt, _) => symsIn(List(stmt)).contains(sym)
      }.get._2
      val end = luaCode.zipWithIndex.reverse.find {
        case (stmt, _) => symsIn(List(stmt)).contains(sym)
      }.get._2

      // check to see if there's a reference in an open loop or function
      val loopEnds = luaCode.zipWithIndex.filter {
        case (_, i) => i >= start && i <= end
      }.collect {
        case (LLWhile(_, bodyLen), i)
          if i + bodyLen > end && sliceReferences(i, i + bodyLen, sym)
        => bodyLen + i
        case (LLLocal(_, Some(LLFunctionHeader(_, bodyLen, _))), i)
          if sliceReferences(i, i + bodyLen, sym)
        => luaCode.size // FIXME this is a hack, but it works for now
        //  upvalues are considered live forever
      }

      val loopEnd: Option[Int] = if (loopEnds.isEmpty) None else Some(loopEnds.max)
      start -> loopEnd.map(_ max end).getOrElse(end)
    }).toMap
  }

  override def emitValDef(sym: Sym[Any], rhs: LLExpr): Unit = luaCode += {
    // FIXME this should be done in sth like a DCE pass
    if (sym.tp.<:<(ManifestTyp(manifest[Unit]))) LLStandalone(rhs)
    else LLLocal(sym, Some(rhs))
  }
  override def emitAssignment(lhs: Exp[Any], rhs: Exp[Any]): Unit = emitAssignment(l"$lhs", l"$rhs")
  override def emitAssignment(lhs: LLExpr, rhs: LLExpr): Unit = luaCode += LLAssign(lhs, rhs)
}
