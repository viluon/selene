package me.viluon.lua.codegen

import scala.lms.internal.GenericFatCodegen

//trait LuaFatCodegen extends LuaCoreCodegen with GenericFatCodegen {
//  import IR._
//
//  def emitMultiValDef(syms: List[Sym[Any]], rhs: String): Unit =
//    syms.map {
//      case sym if sym.tp <:< ManifestTyp(manifest[Unit]) => "_"
//      case sym => q"$sym"
//    }.mkString("local ", ", ", s" = $rhs")
//}
