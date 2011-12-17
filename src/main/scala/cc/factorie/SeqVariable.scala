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

// Variables for dealing with sequences

/** Revert equals/hashCode behavior of Seq[A] to the default Object.
    WARNING: This doesn't actually satisfy == commutativity with a Seq[A]. :-( */
trait SeqEqualsEq[+A] extends scala.collection.Seq[A] {
  override def equals(that:Any): Boolean = that match {
    case that:AnyRef => this eq that
    case _ => false
  }
  override def hashCode: Int = java.lang.System.identityHashCode(this)
}

trait IndexedSeqEqualsEq[+A] extends SeqEqualsEq[A] with IndexedSeq[A] {
  override def equals(that:Any): Boolean = that match {
    case that:AnyRef => this eq that
    case _ => false
  }
  override def hashCode: Int = java.lang.System.identityHashCode(this)
}

trait ElementType[+ET] {
  type ElementType = ET
}

@deprecated("Will be removed")
trait VarAndElementType[+This<:Variable,+ET] extends VarAndValueType[This,IndexedSeq[ET]] with ElementType[ET]

trait SeqVar[+E] extends Variable with /*VarAndElementType[SeqVar[X],X] with*/ SeqEqualsEq[E] with VarAndValueType[SeqVar[E],Seq[E]] with ElementType[E] //with VarAndValueGenericDomain[SeqVar[E],Seq[E]]
trait IndexedSeqVar[+E] extends Variable with IndexedSeqEqualsEq[E] with VarAndValueType[IndexedSeqVar[E],IndexedSeq[E]] with ElementType[E] //with VarAndValueGenericDomain[IndexedSeqVar[E],IndexedSeq[E]]
//trait IndexedSeqVar[+X] extends Variable with VarAndElementType[IndexedSeqVar[X],X] with IndexedSeqEqualsEq[X] with VarAndValueGenericDomain[IndexedSeqVar[X],IndexedSeq[X]] 

/** A variable containing a mutable sequence of other variables.  
    This variable stores the sequence itself, and tracks changes to the contents and order of the sequence. 
    @author Andrew McCallum */
trait MutableSeqVar[X] extends SeqVar[X] with MutableVar /*with VarAndElementType[SeqVar[X],X] with SeqEqualsEq[X]*/ {
  //type ElementType <: AnyRef
  type Element = VariableType#ElementType
  protected val _seq = new ArrayBuffer[Element] // TODO Consider using an Array[] instead so that SeqVar[Int] is efficient.
  final def value: IndexedSeq[Element] = _seq
  def set(newValue:Value)(implicit d:DiffList): Unit = { _seq.clear; _seq ++= newValue }
  def update(seqIndex:Int, x:Element)(implicit d:DiffList): Unit = UpdateDiff(seqIndex, x)
  def append(x:Element)(implicit d:DiffList) = AppendDiff(x)
  def prepend(x:Element)(implicit d:DiffList) = PrependDiff(x)
  def trimStart(n:Int)(implicit d:DiffList) = TrimStartDiff(n)
  def trimEnd(n: Int)(implicit d:DiffList) = TrimEndDiff(n)
  def remove(n:Int)(implicit d:DiffList) = Remove1Diff(n)
  def swap(i:Int,j:Int)(implicit d:DiffList) = Swap1Diff(i,j)
  def swapLength(pivot:Int,length:Int)(implicit d:DiffList) = for (i <- pivot-length until pivot) Swap1Diff(i,i+length)
  abstract class SeqVariableDiff(implicit d:DiffList) extends AutoDiff {override def variable = MutableSeqVar.this}
  case class UpdateDiff(i:Int, x:Element)(implicit d:DiffList) extends SeqVariableDiff {val xo = _seq(i); def undo = _seq(i) = xo; def redo = _seq(i) = x}
  case class AppendDiff(x:Element)(implicit d:DiffList) extends SeqVariableDiff {def undo = _seq.trimEnd(1); def redo = _seq.append(x)}
  case class PrependDiff(x:Element)(implicit d:DiffList) extends SeqVariableDiff {def undo = _seq.trimStart(1); def redo = _seq.prepend(x)}
  case class TrimStartDiff(n:Int)(implicit d:DiffList) extends SeqVariableDiff {val s = _seq.take(n); def undo = _seq prependAll (s); def redo = _seq.trimStart(n)}
  case class TrimEndDiff(n:Int)(implicit d:DiffList) extends SeqVariableDiff {val s = _seq.drop(_seq.length - n); def undo = _seq appendAll (s); def redo = _seq.trimEnd(n)}
  case class Remove1Diff(n:Int)(implicit d:DiffList) extends SeqVariableDiff {val e = _seq(n); def undo = _seq.insert(n,e); def redo = _seq.remove(n)}
  case class Swap1Diff(i:Int,j:Int)(implicit d:DiffList) extends SeqVariableDiff { def undo = {val e = _seq(i); _seq(i) = _seq(j); _seq(j) = e}; def redo = undo }
  // for Seq trait
  def length = _seq.length
  def iterator = _seq.iterator
  def apply(index: Int) = _seq(index)
  // for changes without Diff tracking
  def +=(x:Element) = _seq += x
  def ++=(xs:Iterable[Element]) = _seq ++= xs
  //def update(index:Int, x:Element): Unit = _seq(index) = x
}

abstract class SeqVariable[X](initialValue: Seq[X]) extends MutableSeqVar[X] /*with VarAndElementType[SeqVariable[X],X] with VarAndValueGenericDomain[SeqVariable[X],Seq[X]]*/ {
  def this() = this(Nil)
  _seq ++= initialValue
  //protected val _seq: ArrayBuffer[X] = { val a = new ArrayBuffer[X](); a ++= sequence; a }
}

abstract class DiscreteSeqDomain extends Domain[Seq[DiscreteValue]] {
  def elementDomain: DiscreteDomain
}

trait DiscreteSeqVar extends IndexedSeqVar[DiscreteValue] {
  def domain: DiscreteSeqDomain
  def intValue(seqIndex:Int): Int
  def intValues: Array[Int]
  def uniqueIntValues: Array[Int]
}

//abstract class DiscreteSeqVariable extends MutableVar with cc.factorie.util.ProtectedIntArrayBuffer with SeqEqualsEq[DiscreteValue] with VarAndElementType[DiscreteSeqVariable,DiscreteValue] 
abstract class DiscreteSeqVariable extends MutableVar with cc.factorie.util.ProtectedIntArrayBuffer with DiscreteSeqVar {
  def this(initialValue:Seq[Int]) = { this(); /*_setCapacity(if (initialValue.length > 0) initialValue.length else 1);*/ if (initialValue.length > 0) _appendAll(initialValue.toArray) }
  def this(len:Int) = { this(); _setCapacity(len); _appendAll(Array.fill(len)(0)) }
  def length = _length
  def apply(index: Int): ElementType = domain.elementDomain.getValue(_apply(index))
  override def iterator = new Iterator[ElementType] {
    var i = 0
    def hasNext = i < _length
    def next = { i += 1; domain.elementDomain.getValue(_apply(i-1)) }
  }
  def domain: DiscreteSeqDomain
  def value: Value = new IndexedSeq[ElementType] {
    private val arr = new Array[ElementType](_length)
    _mapToArray(arr, (i:Int) => domain.elementDomain.getValue(i)) // Do this so that it stays constant even if _array changes later
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

class CategoricalSeqDomain[C] extends DiscreteSeqDomain with Domain[Seq[CategoricalValue[C]]] {
  lazy val elementDomain: CategoricalDomain[C] = new CategoricalDomain[C]
}
abstract class CategoricalSeqVariable[C] extends DiscreteSeqVariable with VarAndElementType[CategoricalSeqVariable[C],CategoricalValue[C]] {
  //def this(initialValue:Seq[C]) = { this(); val d = domain.elementDomain; initialValue.foreach(c => this += d.index(c)) }
  def this(initialValue:Seq[C]) = {
    this()
    _setCapacity(if (initialValue.length > 0) initialValue.length else 1)
    val d = domain.elementDomain
    initialValue.foreach(c => this += d.getValue(c))
  }
  def domain: CategoricalSeqDomain[C]
  def appendCategory(x:C): Unit = this += domain.elementDomain.getValue(x)
  def appendCategories(xs:Iterable[C]): Unit = _appendAll(xs.map(c => domain.elementDomain.index(c)).toArray)
  def categoryValue(seqIndex:Int): C = domain.elementDomain.getCategory(_apply(seqIndex))
  def categoryValues: Seq[C] = Seq.tabulate(length)(i => categoryValue(i))
}

