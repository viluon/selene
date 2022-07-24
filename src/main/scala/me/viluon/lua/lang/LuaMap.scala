package me.viluon.lua.lang

import scala.language.implicitConversions
import scala.lms.common.{Base, TupleOps}

trait LuaMap extends Base with TupleOps {
  implicit def mapToRepMap[K: Typ, V: Typ](m: Map[K, V]): Rep[Map[K, V]] = map_new(m)
  implicit def mapIsTyp[K: Typ, V: Typ]: Typ[Map[K, V]]

  implicit class MapOps[K: Typ, V: Typ](m: Rep[Map[K, V]]) {
    def map[A: Typ](f: Rep[V] => Rep[A]): Rep[Map[K, A]] =
      map_map(m, f)
    def contains(k: Rep[K]): Rep[Boolean] =
      map_contains(m, k)
    def size: Rep[Int] =
      map_size(m)
    def +(k: Rep[K], v: Rep[V]): Rep[Map[K, V]] =
      map_add(m, k, v)
    def +(kv: Rep[(K, V)]): Rep[Map[K, V]] =
      map_add(m, kv._1, kv._2)
  }

  def map_new[K: Typ, V: Typ](init: Map[K, V]): Rep[Map[K, V]]
  def map_map[K: Typ, V: Typ, A: Typ](m: Rep[Map[K, V]], f: Rep[V] => Rep[A]): Rep[Map[K, A]]
  def map_contains[K: Typ, V: Typ](m: Rep[Map[K, V]], k: Rep[K]): Rep[Boolean]
  def map_size[K: Typ, V: Typ](m: Rep[Map[K, V]]): Rep[Int]
  def map_add[K: Typ, V: Typ](m: Rep[Map[K, V]], k: Rep[K], v: Rep[V]): Rep[Map[K, V]]
}
