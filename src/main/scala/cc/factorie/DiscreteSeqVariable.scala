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
import scala.math
import java.util.Arrays

abstract class DiscreteSeqDomain extends Domain[Seq[DiscreteValue]] {
  def elementDomain: DiscreteDomain
}

trait DiscreteSeqVar extends IndexedSeqVar[DiscreteValue] {
  def domain: DiscreteSeqDomain
  def intValue(seqIndex:Int): Int
  def intValues: Array[Int]
  def uniqueIntValues: Array[Int]
  def discreteValues: IndexedSeq[DiscreteValue]
  def length: Int
  def apply(index:Int): DiscreteValue
}

//abstract class DiscreteSeqVariable extends MutableVar with cc.factorie.util.ProtectedIntArrayBuffer with SeqEqualsEq[DiscreteValue] with VarAndElementType[DiscreteSeqVariable,DiscreteValue] 
abstract class DiscreteSeqVariable extends MutableVar with cc.factorie.util.ProtectedIntArrayBuffer with DiscreteSeqVar {
  def this(initialValue:Seq[Int]) = { this(); /*_setCapacity(if (initialValue.length > 0) initialValue.length else 1);*/ if (initialValue.length > 0) _appendAll(initialValue.toArray) }
  def this(initialValue:Array[Int]) = { this(); if (initialValue.length > 0) _appendAll(initialValue) }
  def this(len:Int) = { this(); _setCapacity(len); _appendAll(Array.fill(len)(0)) }
  def length = _length
  def apply(index: Int): ElementType = domain.elementDomain.apply(_apply(index))
  def domain: DiscreteSeqDomain
  def discreteValues: IndexedSeq[DiscreteValue] = new IndexedSeq[DiscreteValue] {
    def length = _length
    def apply(index:Int) = domain.elementDomain.apply(_apply(index))
  }
  def value: Value = new IndexedSeq[ElementType] {
    private val arr = new Array[ElementType](_length)
    _mapToArray(arr, (i:Int) => domain.elementDomain.apply(i)) // Do this so that it stays constant even if _array changes later
    def length = arr.length
    def apply(i:Int) = arr(i)
   //_toSeq.map(i => domain.elementDomain.getValue(i)) // TODO make this more efficient 
  }
  def set(newValue:Value)(implicit d:DiffList): Unit = _set(Array.tabulate(newValue.length)(i => newValue(i).intValue))
  def trimCapacity: Unit = _trimCapacity
  def clear(): Unit = _clear()
  def fill(newValue:Int): Unit = Arrays.fill(_array, newValue)
  def appendInt(i:Int): Unit = _append(i)
  def +=(e:VariableType#ElementType): Unit = appendInt(e.intValue)
  def ++=(es:Iterable[VariableType#ElementType]): Unit = _appendAll(es.map(_.intValue))
  def appendInts(xs:Iterable[Int]) = _appendAll(xs)
  def appendInts(xs:Array[Int]) = _appendAll(xs)
  def intValue(seqIndex:Int): Int = _apply(seqIndex)
  def intValues: Array[Int] = _array
  def uniqueIntValues: Array[Int] = _array.distinct.sorted
  def set(seqIndex:Int, newValue:Int)(implicit d:DiffList): Unit = {
    require(d eq null)
    _update(seqIndex, newValue)
  }
}

trait SeqBreaks {
  /** Contains indices of the sequence positions which immediately follow breaks (e.g. removed stopwords) */
  val breaks = new scala.collection.mutable.BitSet
}

