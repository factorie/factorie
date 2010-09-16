package cc.factorie

import cc.factorie.la._
import junit.framework._
import Assert._

/**
 * @author Tim Vieira
 * @since Sep 16, 2010
 */

class TestSparseBinaryVector extends TestCase {

  def test_activeDomain_and_activeDomainSize = {
    println("testing active domain...")

    val v = new SparseBinaryVector(10)
    assertEquals(v.activeDomain.size, 0)

    // check that adding something twice doesn't get added twice to activeDomain
    v += 1
    v += 1  // add 1 twice
    assertEquals(v.activeDomain, List(1))
    assertEquals(v.activeDomain.size, 1)
  }

  def test_length = {
    println("testing length..")
    val v = new SparseBinaryVector(1000)

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

  def test_index_gt_length = {
    println("test_index_gt_length")
    val v = new SparseBinaryVector(1000)
    // check the boundary cases
    v += 0
    v += v.length-1
    // TODO: these shoud probably raise an Exception!
    v += v.length
    v += v.length+100
    v += -1
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

  /*
  def test_dot_with_DenseVector: Unit = { }
  def test_dot_with_SparseHashVector: Unit = { }
  def test_dot_with_SingletonBinaryVector: Unit = { }
  def test_dot_with_SingletonVector: Unit = { }
  def test_dot_with_VectorTimesScalar: Unit = { }
  def test_dot_with_SparseBinaryVector: Unit = { }
  */
}


object TestSparseBinaryVector {

  def main(args: Array[String]) {
    val suite = new TestSuite
    suite.addTestSuite(classOf[TestSparseBinaryVector])

    junit.textui.TestRunner.run(suite)
  }
}
