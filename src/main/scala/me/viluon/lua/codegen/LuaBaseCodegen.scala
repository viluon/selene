package me.viluon.lua.codegen

import scala.lms.internal.GenericCodegen

trait LuaBaseCodegen extends GenericCodegen {

  import IR._

  import java.io.PrintWriter

  override def emitValDef(sym: Sym[Any], rhs: String): Unit = stream.println(s"local ${quote(sym)} = $rhs")

  override def quote(x: Exp[Any]): String = x match {
    case Const(()) => "nil"
    case _ => super.quote(x)
  }

  override def emitBlock(y: Block[Any]): Unit = {
    //    stream.println("do -- emitBlock - is this correct?")
    super.emitBlock(y)
    //    stream.println("end")
  }

  override def emitSource[A: Typ](args: List[IR.Sym[_]],
                                  body: Block[A],
                                  className: String,
                                  out: PrintWriter): List[(IR.Sym[Any], Any)] = {
    val argsStr = args.map(quote).mkString(", ")

    withStream(out) {
      stream.println("function " + (if (className.isEmpty) "" else className) + s"($argsStr)")
      emitBlock(body)
      val result = getBlockResult(body)
      if (!(result.tp <:< ManifestTyp(manifest[Unit]))) {
        stream.println(s"return ${quote(result)}")
      }

      stream.println("end")
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
    stream.println(s"${quote(lhs)} = $rhs")
  }
}
