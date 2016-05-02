/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie.variable

import java.util.Arrays

/** A Domain for sequences of DiscreteValues.
    The 'elementDomain' is abstract.
    Typical usage for DiscreteValues with domain size of 10: object MyDomain extends DiscreteSeqDomain { val elementDomain = new DiscreteDomain(10) }
    These are used, for example, for the 'z' indicator variables in Latent Dirichlet Allocation.
    @author Andrew McCallum */
abstract class DiscreteSeqDomain extends Domain {
  type Value <: Seq[DiscreteValue]
  def elementDomain: DiscreteDomain
}

/** An abstract variable whose values are sequences of DiscreteValues.
    The method 'domain' is abstract.
    @author Andrew McCallum */
trait DiscreteSeqVar extends IndexedSeqVar[DiscreteValue] {
  type Value <: IndexedSeq[DiscreteValue]
  def domain: DiscreteSeqDomain
  def intValue(seqIndex:Int): Int
  def intValues: Array[Int]
  def uniqueIntValues: Array[Int]
  def discreteValues: IndexedSeq[DiscreteValue]
  def length: Int
  def apply(index:Int): DiscreteValue
}

/** An abstract variable whose values are sequences of DiscreteValues, and this sequence can be changed.
    The method 'domain' is abstract.
    @author Andrew McCallum */
trait MutableDiscreteSeqVar[A<:DiscreteValue] extends MutableVar with cc.factorie.util.ProtectedIntArrayBuffer with DiscreteSeqVar {
  type Value <: IndexedSeq[A]
  override def length = _length
  override def apply(index: Int): A = domain.elementDomain.apply(_apply(index)).asInstanceOf[A]
  def domain: DiscreteSeqDomain
  def discreteValues: IndexedSeq[DiscreteValue] = new IndexedSeq[A] {
    def length = _length
    def apply(index:Int) = domain.elementDomain.apply(_apply(index)).asInstanceOf[A]
  }
  def value: Value = new IndexedSeq[A] {
    private val arr = new Array[Any](_length)
    _mapToArray(arr, (i:Int) => domain.elementDomain.apply(i)) // Do this so that it stays constant even if _array changes later
    def length = arr.length
    def apply(i:Int) = arr(i).asInstanceOf[A]
   //_toSeq.map(i => domain.elementDomain.getValue(i)) // TODO make this more efficient 
  }.asInstanceOf[Value]
  def set(newValue:Value)(implicit d:DiffList): Unit = _set(Array.tabulate(newValue.length)(i => newValue(i).intValue))
  def trimCapacity(): Unit = _trimCapacity
  def clear(): Unit = _clear()
  def fill(newValue:Int): Unit = Arrays.fill(_array, newValue)
  def appendInt(i:Int): Unit = _append(i)
  def +=(e:A): Unit = appendInt(e.intValue)
  def ++=(es:Iterable[A]): Unit = _appendAll(es.map(_.intValue))
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

/** An variable whose values are sequences of DiscreteValues, and this sequence can be changed.
    The method 'domain' is abstract.
    These are used, for example, to store the 'z' indicator variables in Latent Dirichlet Allocation.
    @author Andrew McCallum */
abstract class DiscreteSeqVariable extends MutableDiscreteSeqVar[DiscreteValue] {
  type Value = IndexedSeq[DiscreteValue]
  def this(initialValue:Seq[Int]) = { this(); /*_setCapacity(if (initialValue.length > 0) initialValue.length else 1);*/ if (initialValue.length > 0) _appendAll(initialValue.toArray) }
  def this(initialValue:Array[Int]) = { this(); if (initialValue.length > 0) _appendAll(initialValue) }
  def this(len:Int) = { this(); _setCapacity(len); _appendAll(Array.fill(len)(0)) }
}

/** Mix this trait into a DiscreteSeqVariable in order to be able to mark positions in the sequence as "breaks".
    For example, this is used to mark which words in a Latent Dirichlet Allocation document had stopwords removed immediately before them;
    this in turn is used in various phrase processing logic. 
    @author Andrew McCallum */
trait SeqBreaks {
  /** Contains indices of the sequence positions which immediately follow breaks (e.g. removed stopwords) */
  val breaks = new scala.collection.mutable.BitSet
}

