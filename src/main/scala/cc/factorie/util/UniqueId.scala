package cc.factorie.util

/** An object that has a unique identifier that is consistent across serialization. */
trait UniqueId[A] {
  def uniqueId: A
}
