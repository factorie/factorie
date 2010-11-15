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

/** A lazy product of a Vector and a scalar.
    @author Andrew McCallum */
class VectorTimesScalar(val vector:Vector, val scalar:Double) extends Vector {
  def length = vector.length
  def activeDomainSize: Int = vector.activeDomainSize
  def activeDomain: Iterable[Int] = vector.activeDomain
  def dot(v:Vector): Double = vector.dot(v) * scalar
  def activeElements: Iterator[(Int,Double)] = new Iterator[(Int,Double)] {
    val iter = vector.activeElements
    def hasNext = iter.hasNext
    def next = { val n = iter.next; (n._1, n._2 * scalar) }
  }
  def apply(index:Int) = vector.apply(index) * scalar
}

