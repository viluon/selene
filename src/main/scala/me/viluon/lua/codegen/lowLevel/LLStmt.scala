package me.viluon.lua.codegen.lowLevel

import me.viluon.lua.codegen.QuoteGen

sealed trait LLStmt {
  def asLua: String
}

trait LLStmtOps { self: QuoteGen =>
  sealed trait LLExpr {
    def expr(uses: List[self.IR.Sym[Any]]): String
    val uses: List[self.IR.Sym[Any]]

    def substituted(allocation: Map[self.IR.Sym[Any], self.IR.Sym[Any]]): LLExpr = this match {
      case LLExprStandalone(exprGen, uses) => LLExprStandalone(exprGen, uses.map(allocation))
      case LLFunctionHeader(args, bodyLen, uses) => LLFunctionHeader(args, bodyLen, uses.map(allocation))
    }
  }

  object LLExpr {
    def unapply(e: LLExpr): Option[(String, List[self.IR.Sym[Any]])] = Some((e.expr(e.uses), e.uses))
    def apply(e: List[self.IR.Sym[Any]] => String, uses: List[self.IR.Sym[Any]]): LLExpr = LLExprStandalone(e, uses)

    def fromSeq(xs: Seq[self.IR.Exp[Any]], sep: String = ", "): LLExpr =
      fromSeq(xs, "{", sep, "}")

    def fromSeq(xs: Seq[self.IR.Exp[Any]], start: String, sep: String, end: String): LLExpr =
      fromSeqUnsafe(xs, start, sep, end)

    def genericExprGen(arity: Int, components: List[Either[Int, String]]): List[self.IR.Sym[Any]] => List[String] =
      (uses: List[self.IR.Sym[Any]]) => {
        assert(arity == uses.length, s"arity mismatch: $arity != ${uses.length}")
        components.map(_.left.map(i => quote(uses(i))).merge)
      }

    def fromSeqUnsafe(xs: Seq[Any], start: String, sep: String, end: String): LLExpr = {
      val (arity, folded) = xs.foldLeft(0 -> List[Either[Int, String]]()) {
        // for each referenced sym, add its index to the list
        case ((n, acc), expr: self.IR.Exp[_]) if IR.syms(expr).nonEmpty =>
          // TODO more than one ref?
          if (IR.syms(expr).length > 1) {
            throw new IllegalArgumentException(s"cannot handle multiple refs in $expr")
          }
          n + 1 -> (acc :+ Left(n))
        // quote all else
        case ((n, acc), expr: self.IR.Exp[_]) => n -> (acc :+ Right(quote(expr)))
        case ((n, acc), str: String) => n -> (acc :+ Right(str))
        case _ => throw new IllegalArgumentException("unsupported type in LLExpr.fromSeq")
        // :+ here is quadratic, but we don't expect xs to get very long
        // and reversing separately is just so *ugly*...
      }
      LLExpr(genericExprGen(arity, folded).andThen(_.mkString(start, sep, end)), xs.flatMap(IR.syms).toList)
    }
  }

  case class LLExprStandalone(exprGen: List[self.IR.Sym[Any]] => String, uses: List[self.IR.Sym[Any]]) extends LLExpr {
    def expr(uses: List[self.IR.Sym[Any]]): String = exprGen(uses)
  }

  case class LLFunctionHeader(args: List[String], bodyLen: Int, uses: List[self.IR.Sym[Any]] = Nil) extends LLExpr {
    override def expr(uses: List[self.IR.Sym[Any]]): String = {
      assert(uses.isEmpty, "Function header cannot reference variables")
      args.mkString("function(", ", ", ")")
    }
  }

  sealed trait LLStmtImpl extends LLStmt {
    def asLua: String = this match {
      case LLAssign(LLExpr(lhs, _), LLExpr(rhs, _)) => q"$lhs = $rhs"
      case LLReturn(LLExpr(expr, _)) => q"return $expr"
      case LLWhile(LLExpr(cond, _), _) => q"while $cond do"
      case LLIf(LLExpr(cond, _)) => q"if $cond then"
      case LLEnd() => "end"
      case LLElse() => "else"
      case LLStandalone(LLExpr(expr, _)) => q"$expr"
      case LLLocal(sym, None) => q"local $sym"
      case LLLocal(sym, Some(LLExpr(expr, _))) => q"local $sym = $expr"
    }
  }

  def substituted(stmt: LLStmt, allocation: Map[self.IR.Sym[Any], self.IR.Sym[Any]]): LLStmt = stmt match {
    case LLAssign(lhs, rhs) => LLAssign(lhs.substituted(allocation), rhs.substituted(allocation))
    case LLReturn(expr) => LLReturn(expr.substituted(allocation))
    case LLWhile(cond, bodyLen) => LLWhile(cond.substituted(allocation), bodyLen)
    case LLIf(cond) => LLIf(cond.substituted(allocation))
    case LLEnd() => LLEnd()
    case LLElse() => LLElse()
    case LLStandalone(expr) => LLStandalone(expr.substituted(allocation))
    case LLLocal(sym, expr) => LLLocal(allocation(sym), expr.map(_.substituted(allocation)))
  }

  case class LLAssign(lhs: LLExpr, rhs: LLExpr) extends LLStmtImpl
  case class LLReturn(expr: LLExpr) extends LLStmtImpl
  case class LLWhile(cond: LLExpr, bodyLen: Int) extends LLStmtImpl
  case class LLIf(cond: LLExpr) extends LLStmtImpl
  case class LLEnd() extends LLStmtImpl
  case class LLElse() extends LLStmtImpl
  case class LLStandalone(expr: LLExpr) extends LLStmtImpl
  case class LLLocal(sym: self.IR.Sym[Any], expr: Option[LLExpr] = None) extends LLStmtImpl
}
