package cc.factorie

import cc.factorie.la._
import junit.framework._
import Assert._

/**
 * @author Tim Vieira
 * @since Sep 16, 2010
 */

class TestSparseBinaryVector extends TestCase {

  def test = {
    val v = new SparseBinaryVector(1000)

    // initial size should be zero
    assertEquals(v.activeDomain.size, 0)

    val a = 0
    val b = 1
    val c = v.length-1

    // add in unsorted order
    v += b
    v += a
    v += b
    v += c
    v += a
    v += c

    assertEquals(v.activeDomain, List(a,b,c))
    assertEquals(v.activeDomainSize, 3)

    // activeDomain's size is *not* the length!
    assertEquals(v.length, 1000)
  }

  def test_suspicious_indices = {
    val v = new SparseBinaryVector(1000)
    // check the boundaries
    v += 0
    v += v.length-1
    // TODO: these shoud probably raise an Exception!
    v += v.length
    v += v.length+100
    v += -1
  }

  def test_zero = {
    val v = new SparseBinaryVector(1000)

    def doesNotAlterLength = assertEquals(v.length, 1000)
    def appearsToBeEmpty {
      doesNotAlterLength
      assertEquals(v.activeDomain, List[Int]())
      assertEquals(v.activeDomainSize, 0)
    }

    v.zero
    appearsToBeEmpty

    v += 100
    doesNotAlterLength
    assertEquals(v.activeDomain, List(100))
    assertEquals(v.activeDomainSize, 1)

    v.zero
    appearsToBeEmpty

    v += 100
    doesNotAlterLength
    assertEquals(v.activeDomain, List(100))
    assertEquals(v.activeDomainSize, 1)

  }

  //private def ensureCapacity(n:Int)
  //private def _positionLte(x:Int, start:Int, end:Int): Int = {
  //private def _contains(x:Int, start:Int, end:Int): Boolean = {

  /*
  def test_zero: Unit = { }
  def test_iterator: Unit = { }
  def test_activeElements: Unit = { }
  def test_plusEquals: Unit = { }
  */

  /* TODO: test the different kinds of dot products
  def test_dot_with_DenseVector: Unit = { }
  def test_dot_with_SparseHashVector: Unit = { }
  def test_dot_with_SingletonBinaryVector: Unit = { }
  def test_dot_with_SingletonVector: Unit = { }
  def test_dot_with_VectorTimesScalar: Unit = { }
  def test_dot_with_SparseBinaryVector: Unit = { }
  */
}

object TestSparseBinaryVector extends TestSuite {
  addTestSuite(classOf[TestSparseBinaryVector])
  def main(args: Array[String]) {
    junit.textui.TestRunner.run(this)
  }
}

