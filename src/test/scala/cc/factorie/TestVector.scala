package cc.factorie

import cc.factorie.la._
import junit.framework._
import Assert._

/**
 * @author Tim Vieira
 * @since Sep 16, 2010
 */

class TestSparseBinaryVector extends TestCase {

  def testBasics = {
    val v = new SparseBinaryVector(10000)

    // initial size should be zero
    assertEquals(v.activeDomain.size, 0)

    val x = List(100,300,1000,500,2,100)
    x.foreach(v+=_)

    val expect = x.toSet.toList.sortWith(_<_)
    assertEquals(v.activeDomain.toList, expect)
    assertEquals(v.activeDomainSize, expect.size)

    // the contains test
    x.forall(v(_)==1.0)
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
