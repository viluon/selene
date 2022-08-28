package me.viluon.lua.codegen.lowLevel

import me.viluon.lua.codegen.DummyGen

import scala.collection.mutable.ArrayBuffer

sealed trait LLStmt {
  def asLua: String
}

trait LLStmtMixin {
  self: DummyGen =>
  val LLExpr: LLExprAPI
  var luaCode: collection.mutable.ArrayBuffer[LLStmt]

  sealed trait LLExpr {
    def expr(uses: List[self.IR.Sym[Any]]): String
    val uses: List[self.IR.Sym[Any]]
    def arity: Int = uses.length

    def substituted(allocation: Map[self.IR.Sym[Any], self.IR.Sym[Any]]): LLExpr = this match {
      case LLExprStandalone(exprGen, uses) => LLExprStandalone(exprGen, uses.map(allocation))
      case LLFunctionHeader(args, bodyLen, uses) => LLFunctionHeader(args, bodyLen, uses.map(allocation))
    }

    def +(other: LLExpr): LLExpr = LLExpr((uses: List[self.IR.Sym[Any]]) => {
      val (lhs, rhs) = (this.expr(uses.take(arity)), other.expr(uses.drop(arity)))
      lhs + rhs
    }, this.uses ++ other.uses)
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

  trait LLExprAPI {
    def unapply(e: LLExpr): Option[(String, List[self.IR.Sym[Any]])]
    def apply(e: List[self.IR.Sym[Any]] => String, uses: List[self.IR.Sym[Any]]): LLExpr
    def fromSeq(xs: Seq[self.IR.Exp[Any]], sep: String = ", "): LLExpr
    def fromSeq(xs: Seq[self.IR.Exp[Any]], start: String, sep: String, end: String): LLExpr
    def genericExprGen(arity: Int, components: List[Either[Int, String]]): List[self.IR.Sym[Any]] => List[String]
    def fromSeqUnsafe(xs: Seq[Any], start: String, sep: String, end: String): LLExpr
  }

  def lowLevelStmtAsLua(stmt: LLStmtImpl): String
  def inScope(body: => Unit): ArrayBuffer[LLStmt]

  sealed trait LLStmtImpl extends LLStmt {
    def asLua: String = lowLevelStmtAsLua(this)
  }

  case class LLAssign(lhs: LLExpr, rhs: LLExpr) extends LLStmtImpl
  case class LLReturn(expr: LLExpr) extends LLStmtImpl
  case class LLWhile(cond: LLExpr, bodyLen: Int) extends LLStmtImpl
  case class LLIf(cond: LLExpr) extends LLStmtImpl
  case class LLEnd() extends LLStmtImpl
  case class LLElse() extends LLStmtImpl
  case class LLStandalone(expr: LLExpr) extends LLStmtImpl
  case class LLLocal(sym: self.IR.Sym[Any], expr: Option[LLExpr] = None) extends LLStmtImpl
  case class LLManyLocals(syms: List[self.IR.Sym[Any]]) extends LLStmtImpl
}
