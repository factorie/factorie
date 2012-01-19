/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie.la

import cc.factorie._
import junit.framework._
import Assert._

/**
 * @author timv, sameer
 * @since Sep 16, 2010
 */

class TestSparseBinaryVector extends TestCase {

  def test_suspicious_indices:Unit = {
    val v = new SparseBinaryVector(1000)
    // check the boundaries
    v include 0
    v include v.length-1
    try {
      v include v.length
      v include v.length + 100
      v include -1
      fail("No Exception Thrown")
    } catch {
      case e: java.lang.IllegalArgumentException =>
      case _ => fail("Incorrect Exception")
    }
    assertTrue(true)
  }

  def test_advanced:Unit = {
    val v = new SparseBinaryVector(10000)

    // initial size should be zero
    assertEquals(v.activeDomain.size, 0)

    val x = List(100,300,1000,500,2,100)
    x.foreach(v include _)

    val expect = x.toSet.toList.sortWith(_<_)
    assertEquals(v.activeDomain.toList, expect)
    assertEquals(v.activeDomainSize, expect.size)

    // the contains test
    assertTrue(x.forall(v(_)==1.0))
  }

  def test_zero:Unit = {
    val v = new SparseBinaryVector(1000)

    def doesNotAlterLength = assertEquals(v.length, 1000)
    def appearsToBeEmpty {
      doesNotAlterLength
      assertEquals(v.activeDomain, List[Int]())
      assertEquals(v.activeDomainSize, 0)
    }

    v.zero
    appearsToBeEmpty

    v include 100
    doesNotAlterLength
    assertEquals(v.activeDomain, List(100))
    assertEquals(v.activeDomainSize, 1)

    v.zero
    appearsToBeEmpty

    v include 100
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


/**
 * @author Brian Martin
 */

object VectorTestingUtils {
  def fillVector(v: Vector, length:Int): Unit = {
    var i = 0
    while (i < length) {
      v(i) = i
      i += 1
    }
  }

  def copyVector(v1: Vector,  v2: Vector): Unit = {
    assert(v1.size == v2.size)
    var i = 0
    while (i < v1.size) {
      v2.update(i, v1(i))
      i += 1
    }
  }
}

class TestSparseOuter1DenseVector1 extends TestCase {
  import VectorTestingUtils._

  val dim1 = 3 // sparse dimension size
  val dim2 = 5 // dense dimension size

  val denseWeights = new DenseVector(dim1*dim2)
  val sparseOuterWeights = new SparseOuter1DenseVector1(dim1, dim2)

  fillVector(denseWeights, dim1*dim2)
  fillVector(sparseOuterWeights, dim1*dim2)

//  println(denseWeights)
//  println(sparseOuterWeights)

  def testInner: Unit = {
    val i = dim1 - 1 // the last inner
    val inner = sparseOuterWeights.inner(i)
    val offset = dim2 * i
    var j = offset
    while (j < offset + dim2) {
      assertTrue(inner(j - offset) == denseWeights(j))
      j += 1
    }
  }

  def testPlusEq: Unit = {
    sparseOuterWeights += denseWeights
    for ((se, de) <- sparseOuterWeights.activeElements.zip(denseWeights.activeElements))
      assertTrue(se._1 == de._1 && se._2 == de._2 * 2)
  }

  def testActiveDomainSize: Unit = {
    println("activeDomain: ")
    println(sparseOuterWeights.activeDomain.mkString(", "))
    assertTrue(sparseOuterWeights.activeDomainSize == dim1 * dim2)
  }

  def testSizeOfActiveDomain: Unit = {
    assertTrue(sparseOuterWeights.activeDomain.size == dim1 * dim2)
  }

  def testActiveElements: Unit = {
    val actualValues = (0 until (dim1*dim2)).map(i => (i, i.toDouble)).iterator
    println("activeElements: ")
    println(sparseOuterWeights.activeElements.mkString(", "))
    sparseOuterWeights.activeElements.zip(actualValues).foreach(v => assertTrue(v._1 == v._2))
  }

  def testInnerDenseDotDense: Unit = {
    val dotted = denseWeights.dot(denseWeights)
    assertTrue(denseWeights.dot(sparseOuterWeights) == dotted)
    assertTrue(sparseOuterWeights.dot(denseWeights) == dotted)
    assertTrue(sparseOuterWeights.dot(sparseOuterWeights) == dotted)
  }
//  def testInnerSparseBinaryDotDense

}

class TestSparseOuter2DenseVector1 extends TestCase {
  import VectorTestingUtils._

  val dim1 = 2 // first sparse dimension size
  val dim2 = 3 // second sparse dimension size
  val dim3 = 4 // dense dimension size

  val denseWeights = new DenseVector(dim1*dim2*dim3)
  val sparseOuterWeights = new SparseOuter2DenseVector1(dim1, dim2, dim3)

  fillVector(denseWeights, dim1*dim2*dim3)
  fillVector(sparseOuterWeights, dim1*dim2*dim3)

  println(denseWeights)
  println(sparseOuterWeights)

  def testInner: Unit = {
    val i = dim1 - 1
    val j = dim2 - 1
    val inner = sparseOuterWeights.inner(i, j)
    val offset = (i * dim2 + j) * dim3
    var k = offset
    while (k < offset + dim3) {
      //println(k + " " + offset + " " + (inner(k - offset) == denseWeights(k)))
      assertTrue(inner(k - offset) == denseWeights(k))
      k += 1
    }
  }

  def testActiveDomainSize: Unit = {
    println("activeDomain: ")
    println(sparseOuterWeights.activeDomain.mkString(", "))
    assertTrue(sparseOuterWeights.activeDomainSize == dim1 * dim2 * dim3)
  }

  def testSizeOfActiveDomain: Unit = {
    assertTrue(sparseOuterWeights.activeDomain.size == dim1 * dim2 * dim3)
  }

  def testActiveElements: Unit = {
    val actualValues = (0 until (dim1*dim2*dim3)).map(i => (i, i.toDouble)).iterator
    println("activeElements: ")
    println(sparseOuterWeights.activeElements.mkString(", "))
    sparseOuterWeights.activeElements.zip(actualValues).foreach(v => assertTrue(v._1 == v._2))
  }

  def testInnerDenseDotDense: Unit = {
    val dotted = denseWeights.dot(denseWeights)
    assertTrue(denseWeights.dot(sparseOuterWeights) == dotted)
    assertTrue(sparseOuterWeights.dot(denseWeights) == dotted)
    assertTrue(sparseOuterWeights.dot(sparseOuterWeights) == dotted)
  }
  //  def testInnerSparseBinaryDotDense
//  def main(args: Array[String]): Unit = {
//    testInnerDenseDotDense
//  }

}

class TestSparseOuter extends TestCase {
  def testSuite = assertTrue(true)
}

object TestSparseOuter extends TestSuite {
  addTestSuite(classOf[TestSparseOuter1DenseVector1])
  addTestSuite(classOf[TestSparseOuter2DenseVector1])
  def main(args: Array[String]) {
    junit.textui.TestRunner.run(this)
  }
}
