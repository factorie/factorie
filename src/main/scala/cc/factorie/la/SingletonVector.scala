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

/** A Vector that has all zeros, except one position containing some arbitrary Double 'value'.
    @author Andrew McCallum */
class SingletonVector(val theLength:Int, val singleIndex:Int, val value:Double) extends Vector {
  var default = 0.0
  def length = theLength
  def activeDomainSize = 1
  def activeDomain: Iterable[Int] = Seq(singleIndex)
  override def forActiveDomain(f: (Int)=>Unit): Unit = f(singleIndex)
  def apply(index:Int): Double = if (index == singleIndex) value else default
  def dot(v:Vector) = v(singleIndex) * value
  def activeElements = Iterator.single((singleIndex, value))
}
