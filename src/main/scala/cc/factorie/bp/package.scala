package cc.factorie.bp

import scala.util.Random
import annotation.tailrec

/**
 * @author sriedel
 */
package object base {
  val testEps = 0.00000001
  val eps = 0.00000001

  def epsEq(x1: Double, x2: Double) = {
    val diff = x1 - x2
    diff < eps && diff > -eps
  }

  implicit def toApproxComparable(x: Double) = new AnyRef {
    def ~~~(that: Double) = epsEq(x, that)
    def !~~(that: Double) = !epsEq(x, that)
  }

  def incompatible(thisType: GenericMessage, thatType: GenericMessage) =
    throw new MessageOperationNotSupported(thisType, thatType)

  def cantCompute(function: String) = throw new CantCompute(function)

  def subsetsNoLargerThan[T](set: Set[T], maxSize: Int): Iterable[Set[T]] = {
    subsets(set).filter(_.size <= maxSize)
  }

  @tailrec
  def subsets[T](set: Set[T], allSubsetsWithoutGivenSet: Iterable[Set[T]] = Seq(Set.empty[T])): Iterable[Set[T]] = {
    if (set.isEmpty) {
      allSubsetsWithoutGivenSet
    } else {
      val first = set.head
      val remainder = set.drop(1)
      val result = allSubsetsWithoutGivenSet ++ allSubsetsWithoutGivenSet.map(_ + first)
      subsets(remainder, result)
    }
  }

  @tailrec
  def allTuples(domains: Seq[Iterable[Any]], tuplesWithout: Stream[Seq[Any]] = Stream(Seq.empty[Any])): Stream[Seq[Any]] = {
    if (domains.isEmpty) tuplesWithout
    else {
      val last = domains.last
      val remainder = domains.dropRight(1)
      val newTuples = for (x <- tuplesWithout; value <- last.toStream) yield {
        Seq(value) ++ x
      }
      allTuples(remainder, newTuples)
    }
  }

  @tailrec
  def allTuples2(domains: Seq[Seq[Any] => Iterable[Any]], acc: Stream[Seq[Any]] = Stream(Seq.empty[Any])): Stream[Seq[Any]] = {
    if (domains.isEmpty) acc
    else {
      val head = domains.head
      val remainder = domains.drop(1)
      val newTuples = for (x <- acc; value <- head(x).toStream) yield {
        x ++ Seq(value)
      }
      allTuples2(remainder, newTuples)
    }
  }

  var randomSeed = -1
  lazy val random = if (randomSeed < 0) new Random() else new Random(randomSeed)

}

