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

/** A Vector that may contain mostly zeros, with a few arbitrary non-zeros, represented compactly in memory,
    implemented as a HashMap from Int indices to Double values.
    @author Andrew McCallum */
class SparseHashVector(theLength:Int) extends Vector {
  def length: Int = theLength
  var default = 0.0
  private val h = new scala.collection.mutable.HashMap[Int,Double] { override def default(index:Int) = SparseHashVector.this.default }
  def apply(index:Int) = h(index)
  def activeElements = h.iterator
  def activeDomainSize = h.size
  def activeDomain: Iterable[Int] = h.keys
  override def forActiveDomain(f: (Int)=>Unit): Unit = h.keys.foreach(f(_))
  override def update(index:Int, value:Double) = h(index) = value // TODO Should we assert(index < length) ?
  override def increment(index:Int, incr:Double): Unit = h(index) = h(index) + incr // TODO Should we assert(index < length) ?
  def dot(v:Vector): Double = v match {
    case dv:DenseVector => {
      var result = 0.0
      h.iterator.foreach({case(index,value) => result += dv(index) * value})
      result
    }
    case v:SparseBinaryVector => v dot this
    case v:VectorTimesScalar => v dot this
    case v:SingletonBinaryVector => v dot this
    case v:SingletonVector => v dot this
    case sv:SparseHashVector => {
      var result = 0.0
      if (v.size > this.size) h.iterator.foreach({case(index,value) => result += sv(index) * value})
      else sv.h.iterator.foreach({case(index,value) => result += h(index) * value})
      result
    }
    case _ => throw new Error("SparseHashVector.dot does not handle "+v.getClass.getName)
  }
  override def +=(v:Vector): Unit = v.activeElements.foreach({case(index,value) => h.update(index, h(index) + value)})
  override def +=(s:Double): Unit = {
    default += s
    h.keys.foreach(index => h.update(index, h(index) + s))
  }
}

class GrowableSparseVector(val sizeProxy: Iterable[_]) extends SparseHashVector(-1) {
  override def length: Int = sizeProxy.size
}
