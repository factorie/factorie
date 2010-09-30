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

import org.junit.Assert._
import org.junit.Before
import org.junit.Test
import org.scalatest.junit.JUnitSuite
import cc.factorie.TestUtils

class TestVectorOuterProduct extends JUnitSuite with TestUtils {
  import cc.factorie.la.OuterProduct.{computeMatrix => outerProductArrayJava}
  import OuterProductMath.outerProductArray

  object HelperFunctions {
    def arrayEqual(a1:Array[Int], a2:Array[Int]): Boolean = (a1 zip a2).forall {case (a,b) => a==b}

    // test equality of two sparse vector domains
    def domainEqual(v1:Vector, v2:Vector): Boolean = arrayEqual(v1.activeDomain.toArray, v1.activeDomain.toArray)

    def assertClassType[T](msg:String, cls:Class[T], v:Vector) {
      // println("v = " + v); println("cls = " + cls)
      assertTrue(msg + ": should be a " + cls.toString, v.getClass==cls)
    }

    def assertVectorEquality[T](msg:String, cls:Class[T], v1:Vector, v2:Vector) {
      assertClassType(msg, cls, v1)
      assertClassType(msg, cls, v2)
      assertEquals("lengths are not equal", v1.size, v2.size)
      assertTrue("domains are not equal", domainEqual(v1,v2))
    }
  }
  import HelperFunctions._

  // Generate some vectors for test data:
  object V {
    // gen all sparse binary vectors of length 4 w/domain size = 2
    val sparsev2s = chooseN(0 to 3, 2) map { pairs => 
      new SparseBinaryVector(4, pairs.toArray) }

    // gen all sparse binary vectors of length 4 w/domain size = 1
    val sparsev1s = (0 to 3) map { i => 
      new SparseBinaryVector(4, Array(i)) }

    // gen all singleton binary vectors of length 4 
    val singletonvs = (0 to 3) map { i => 
      new SingletonBinaryVector(4, i) }
  }

  // SparseBinaryVector.flatOuter
  @Test def sparseBinaryVectors_flatOuter():Unit = {
    // test out 3 different version of flatOuter on 
    //    pairs of vectors, making sure they all return same value
    for (vvs <- chooseN(V.sparsev2s, 2)) vvs match {
      case Seq(v1, v2) => {
        val r1 = v1 flatOuter v2   
        val r2 = OriginalFlatOuter.flatOuter(v1, v2)
        val r3 = outerProductArrayJava(v1.activeDomain.toArray, v2.activeDomain.toArray, v2.size)
        assertVectorEquality("sparse * sparse", classOf[SparseBinaryVector], r1, r2)
        assertTrue("domains should be equal", arrayEqual(r1.activeDomain.toArray, r3))
      }
    }
  }
  
  // 3-vector version of flatOuter
  @Test def sparseBinaryVectors_flatOuter3():Unit = {
    for (vvs <- chooseN(V.sparsev2s, 3)) vvs match {
      case Seq(v1, v2, v3) => {
        val r1 = v1 flatOuter v2 flatOuter v3
        val r2 = OriginalFlatOuter.flatOuter(OriginalFlatOuter.flatOuter(v1, v2), v3)
        assertVectorEquality("sparse * sparse", classOf[SparseBinaryVector], r1, r2)
      }
    }
  }

  // SingletonBinaryVector.flatOuter
  @Test def singletonBinaryVectors_flatOuter():Unit = {
    // test out 3 different version of flatOuter on pairs of vectors
    for (vvs <- chooseN(V.singletonvs, 2)) vvs match {
      case Seq(v1, v2) => {
        val r1 = v1 flatOuter v2
        val r2 = OriginalFlatOuter.flatOuter(v1, v2)
        val r3 = outerProductArrayJava(v1.activeDomain.toArray, v2.activeDomain.toArray, v2.size)
        assertVectorEquality("singleton * singleton", classOf[SingletonBinaryVector], r1, r2)
        assertTrue("domains should be equal", arrayEqual(r1.activeDomain.toArray, r3))
      }
    }
  }

  // 3-vector version of flatOuter
  @Test def singletonBinaryVectors_flatOuter3():Unit = {
    for (vvs <- chooseN(V.singletonvs, 3)) vvs match {
      case Seq(v1, v2, v3) => {
        val r1 = v1 flatOuter v2 flatOuter v3
        val r2 = OriginalFlatOuter.flatOuter(OriginalFlatOuter.flatOuter(v1, v2), v3)
        assertVectorEquality("sparse * sparse", classOf[SingletonBinaryVector], r1, r2)
      }
    }
  }

  @Test
  def mixedBinaryVectors_flatOuter():Unit = {
    (V.singletonvs zip V.sparsev1s) map {
      case (v1, v2) => {
        val r1 = v1 flatOuter v2
        val r2 = OriginalFlatOuter.flatOuter(v1, v2)
        assertVectorEquality("singleton * sparse", classOf[SparseBinaryVector], r1, r2)
        assertTrue("domains should be equal", domainEqual(r1,r2))
      }
    }
  }

  @Test def dotProduct { }
  
  @Test def update { }

  @Test def += { }

  @Test def * { }
}

