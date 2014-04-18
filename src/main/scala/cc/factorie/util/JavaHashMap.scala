package cc.factorie.util

import scala.collection.mutable
import scala.collection.JavaConverters._
import java.util

// Java hash mutable hashmaps are significantly faster than the Scala ones, for no good reason
// Replace this with AnyRefMap when we switch to Scala 2.11 -luke
object JavaHashMap {
  def apply[K, V](): mutable.Map[K, V] = new util.HashMap[K, V]().asScala
}
