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

import cc.factorie.la._
import junit.framework._
import Assert._

/**
 * @author Tim Vieira
 * @since Sep 16, 2010
 */

class TestSparseBinaryVector extends TestCase {

  def test_suspicious_indices:Unit = {
    val v = new SparseBinaryVector(1000)
    // check the boundaries
    v += 0
    v += v.length-1
    // TODO: these shoud probably raise an Exception!
    v += v.length
    v += v.length+100
    v += -1
    assertTrue(true)
  }

  def test_advanced:Unit = {
    val v = new SparseBinaryVector(10000)
     
    // initial size should be zero
    assertEquals(v.activeDomain.size, 0)
    
    val x = List(100,300,1000,500,2,100)
    x.foreach(v+=_)

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
