package cc.factorie.util
import scala.reflect.ClassTag
import scala.annotation.unchecked.uncheckedVariance

/** Immutable IndexedSeq that, unlike scala.collection.immutable.Vector, can be subclassed.
    @author John Sullivan, Andrew McCallum
 */
class ImmutableArrayIndexedSeq[+A<:AnyRef](elements:Iterable[A]) extends IndexedSeq[A] {
  private val a = (new Array[AnyRef](elements.size)).asInstanceOf[Array[A @uncheckedVariance]]
  elements.copyToArray(a)
  final val length = a.length
  final def apply(i: Int): A = a(i)
}
