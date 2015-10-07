package morsechaos

import scala.collection.mutable

case class Memo1[T1, R](f: T1 => R) extends (T1 => R) {
  private val cache = mutable.Map.empty[T1, R]
  def apply(v1: T1) = cache.getOrElseUpdate(v1, f(v1))
}
