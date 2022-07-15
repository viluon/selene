package me.viluon.lua.codegen.lowLevel

import me.viluon.lua.codegen.QuoteGen

import scala.lms.internal.Expressions

sealed trait LLStmt {
  def asLua: String
}

trait LLStmtOps { self: QuoteGen =>
  sealed trait LLExpr {
    val expr: String
    val uses: List[self.IR.Sym[Any]]
  }

  object LLExpr {
    def unapply(e: LLExpr): Option[(String, List[self.IR.Sym[Any]])] = Some((e.expr, e.uses))
    def apply(e: String, uses: List[self.IR.Sym[Any]]): LLExpr = LLExprStandalone(e, uses)
  }

  case class LLExprStandalone(expr: String, uses: List[self.IR.Sym[Any]]) extends LLExpr
  case class LLFunctionHeader(args: List[String], uses: List[self.IR.Sym[Any]] = Nil) extends LLExpr {
    override val expr: String = args.mkString("function(", ", ", ")")
  }

  sealed trait LLStmtImpl extends LLStmt {
    def asLua: String = this match {
      case LLAssign(LLExpr(lhs, _), LLExpr(rhs, _)) => q"$lhs = $rhs"
      case LLReturn(LLExpr(expr, _)) => q"return $expr"
      case LLWhile(LLExpr(cond, _)) => q"while $cond do"
      case LLIf(LLExpr(cond, _)) => q"if $cond then"
      case LLEnd() => "end"
      case LLElse() => "else"
      case LLStandalone(LLExpr(expr, _)) => q"$expr"
      case LLLocal(sym, None) => q"local $sym"
      case LLLocal(sym, Some(LLExpr(expr, _))) => q"local $sym = $expr"
    }
  }

  case class LLAssign(lhs: LLExpr, rhs: LLExpr) extends LLStmtImpl
  case class LLReturn(expr: LLExpr) extends LLStmtImpl
  case class LLWhile(cond: LLExpr) extends LLStmtImpl
  case class LLIf(cond: LLExpr) extends LLStmtImpl
  case class LLEnd() extends LLStmtImpl
  case class LLElse() extends LLStmtImpl
  case class LLStandalone(expr: LLExpr) extends LLStmtImpl
  case class LLLocal(sym: self.IR.Sym[Any], expr: Option[LLExpr] = None) extends LLStmtImpl
}
