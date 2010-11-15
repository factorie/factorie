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

/** A vector, for holding a sequence of Doubles and performing various linear algebra operations.
    See also @see cc.factorie.generative.Counts
    @author Andrew McCallum */
trait Vector extends scala.collection.mutable.IndexedSeq[Double] {
  def length: Int
  def activeDomainSize: Int
  def activeDomain: Iterable[Int]
  def forActiveDomain(f: (Int)=>Unit): Unit = activeDomain.foreach(f(_))
  def activeElements: Iterator[(Int,Double)]
  def dot(v:Vector): Double
  def oneNorm: Double = activeElements.foldLeft(0.0)(_ + _._2)
  def update(index:Int, value:Double): Unit = throw new Error("Method update not defined on class "+getClass.getName)
  def increment(index:Int, incr:Double): Unit = throw new Error("Method update not defined on class "+getClass.getName)
  def +=(v:Vector): Unit = throw new Error("Method +=(Vector) not defined on class "+getClass.getName)
  def +=(s:Double): Unit = throw new Error("Method +=(Double) not defined on class "+getClass.getName)
  def *(scalar:Double) = new VectorTimesScalar(this, scalar)
  // def toString = this.take(15).mkString(printName+"(", ",", if (length > 15) "...)" else ")")
  // override def toString = this.take(math.min(5, length)).mkString(getClass.getName+"(", ",", if (length > 5) "...)" else ")")
  def flatOuter(that:Vector): Vector = throw new Error("Method flatOuter(Vector) not defined on class "+getClass.getName)
  def flatOuter(v1:Vector, v2:Vector):Vector = throw new Error("Method flatOuter(Vector, Vector) not defined on class "+getClass.getName)
}



object OuterProductMath {
  /** All pairwise computations for SingletonBinaryVector and SparseBinaryVectors */
  /* note: singleton * singleton is done inline in flatOuter() */

  /* sparse * sparse*/
  def outerProductArray(a1: Array[Int], s1:Int, a2: Array[Int], s2:Int, a2width:Int) = {
    val arr = new Array[Int](s1 * s2)
    var i = 0; var n = 0
    while (i<s1) {
      var j = 0
      val m = a1(i) * a2width
      while (j<s2) {
        arr(n) = m + a2(j)
        j +=1
        n += 1
      }
      i += 1
    }
    arr
  }

  /* sparse * singleton */
  def outerProductArray(a1: Array[Int], s1:Int, i2: Int, a2width:Int) = {
    val arr = new Array[Int](s1)
    var i = 0
    while (i<s1) {
      arr(i) = a1(i) * a2width + i2
      i +=1
    }
    arr
  }

  /** All three-way computations for SingletonBinaryVector and SparseBinaryVector combinations */
  /* note: singleton * singleton * singleton is done inline in flatOuter() */


  /* sparse * sparse * singleton */
  def outerProductArray(a1: Array[Int], s1: Int,              // sparse
                        a2: Array[Int], s2: Int, a2width:Int, // sparse
                        i3: Int, i3width:Int): Array[Int]     // singleton
  = {
    val arr = new Array[Int](s1 * s2)
    var i1 = 0; var n = 0
    while (i1<s1) {
      var i2 = 0
      val m1 = a1(i1) * a2width
      while (i2<s2) {
        val m2 = (m1 + a2(i2)) * i3width
        arr(n) = m2 + i3
        n += 1
        i2 += 1
      }
      i1 += 1
    }
    arr
  }  

  /* sparse * singleton * singleton */
  def outerProductArray(a1: Array[Int], s1: Int,          // sparse
                        i2: Int, i2width:Int,             // singleton
                        i3: Int, i3width:Int): Array[Int] // singleton
  = {
    val arr = new Array[Int](s1)
    var i1 = 0
    while (i1<s1) {
      val m1 = a1(i1) * i2width
      val m2 = (m1 + i2) * i3width + i3
      arr(i1) = m2
      i1 += 1
    }
    arr
  }  

  /* sparse * sparse * sparse */
  def outerProductArray(a1: Array[Int], s1:Int,                          // sparse
                        a2: Array[Int], s2:Int, a2width:Int,             // sparse
                        a3: Array[Int], s3:Int, a3width:Int): Array[Int] // sparse
  = {
    val arr = new Array[Int](s1 * s2 * s3)
    var i1 = 0; var n = 0
    while (i1<s1) {
      var i2 = 0
      val m1 = a1(i1) * a2width
      while (i2<s2) {
        var i3 = 0
        val m2 = (m1 + a2(i2)) * a3width
        while (i3<s3) {
          arr(n) = m2 + a3(i3)
          i3 += 1
          n += 1
        }
        i2 += 1
      }
      i1 += 1
    }
    arr
  }
}
