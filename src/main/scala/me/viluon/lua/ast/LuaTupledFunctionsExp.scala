package me.viluon.lua.ast

import scala.lms.common.TupledFunctionsExp

trait LuaTupledFunctionsExp extends TupledFunctionsExp {
  // hack to expose manifestTyp in LuaTupleGen
  def pubManifestTyp[A: Manifest]: Typ[A] = manifestTyp[A]
}
