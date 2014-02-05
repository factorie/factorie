package cc.factorie.util

/**
 * @author John Sullivan
 */

class SubclassableVector[+A](coll:Iterable[A]) extends IndexedSeq[A] {
  private val innerColl = coll.toVector

  val length = innerColl.size

  def apply(idx: Int): A = innerColl(idx)
}