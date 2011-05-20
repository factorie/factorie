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

package cc.factorie
import scala.collection.mutable.ArrayBuffer

/** Variable containing a compact array of values, 
    each accessible by 'index' from 0 to arraySize-1. 
    @author Andrew McCallum */
trait ArrayVar[+A] extends Variable with VarAndValueType[ArrayVar[A],Seq[A]] {
  type ElementValueType = A
  def appendValue[B>:ElementValueType](v:B): Unit
  def value: Seq[ElementValueType]
}

/*
abstract class DiscreteArrayDomain extends Domain[Seq[DiscreteValue]] {
  def elementDomain: DiscreteDomain
}

abstract class DiscreteArrayVariable extends ArrayVar[DiscreteValue] {
  def domain: DiscreteArrayDomain
  private val _array = new ArrayBuffer[ElementValueType]
  def appendValue(v:ElementValueType): Unit = _array += v
  def value: Seq[ElementValueType] = _array
}

abstract class CategoricalArrayVariable[A] extends DiscreteArrayVariable {
}
*/

/** Experimental thoughts about an alternative to ArrayVar. */
/*
trait PlatedVar[A] extends Variable with VarAndValueType[PlatedVar[A],Seq[A]] {
  type ElementValueType = A
  def plateIndex: Int
  def appendValue(v:ElementValueType): Unit
  def value: Seq[ElementValueType]
}
*/
