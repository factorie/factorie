package cc.factorie.util.namejuggler

/**
 * Shamelessly yoinked from edu.umass.cs.iesl.scalacommons
 */
object OptionUtils {
  def merge[T](a: Option[T], b: Option[T], merger: (T, T) => T): Option[T] = {
    (a, b) match {
      case (Some(x), Some(y)) => Some(merger(x, y))
      case (Some(x), None) => a
      case (None, Some(y)) => b
      case (None, None) => None
    }
  }

  def mergeWarn[T](a: Option[T], b: Option[T]): Option[T] = {
    def warn(x: T, y: T): T = {
      if (x != y) {
        println("Merging unequal values, preferring: " + x + "  to " + y)
      }
      x
    }
    merge(a, b, warn)
  }


  def mergeOrFail[T](a: Option[T], b: Option[T]): Option[T] = {
    def fail(x: T, y: T): T = {
      if (x != y) {
        throw new OptionMergeException(x, y)
      }
      x
    }

    merge(a, b, fail)
  }
}

class OptionMergeException[T](x: T, y: T) extends Exception("unequal values: " + x + "  ,  " + y)