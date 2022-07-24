package me.viluon.lua.ast

import me.viluon.lua.lang.LuaMap

import scala.lms.common.EffectExp

trait LuaMapExp extends LuaMap with EffectExp {
  override implicit def mapIsTyp[K: Typ, V: Typ]: Typ[Map[K, V]] = {
    simpleClassTyp(classOf[Map[K, V]])(implicitly, implicitly)
  }

  override def map_new[K: Typ, V: Typ](init: Map[K, V]): Rep[Map[K, V]] = ???
  override def map_map[K: Typ, V: Typ, A: Typ](m: Rep[Map[K, V]], f: Rep[V] => Rep[A]): Rep[Map[K, A]] = ???
  override def map_contains[K: Typ, V: Typ](m: Rep[Map[K, V]], k: Rep[K]): Rep[Boolean] = ???
  override def map_size[K: Typ, V: Typ](m: Rep[Map[K, V]]): Rep[Int] = ???
  override def map_add[K: Typ, V: Typ](m: Rep[Map[K, V]], k: Rep[K], v: Rep[V]): Rep[Map[K, V]] = ???
}
