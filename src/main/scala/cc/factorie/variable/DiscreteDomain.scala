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

import cc.factorie.la._
import cc.factorie.util.Cubbie
import cc.factorie.variable

// For variables that hold a single discrete value, which is represented as a one-hot Tensor1.

/** A value in a DiscreteDomain.  Conceptually it is a single integer value ranging from 0 to the domain size minus one.
    But it is also a Tensor1, of length domain.size, with 0s in all positions except its integer value; this
    facilitates its use in various tensor statistics operations. 
    @author Andrew McCallum */
trait DiscreteValue extends SingletonBinaryTensorLike1 {
  // def dim1: Int remains abstract
  @inline final def intValue: Int = singleIndex // TODO Consider swapping singleIndex <-> intValue
  @inline final def booleanValue = if (intValue == 0) false else true
  override def toString: String = singleIndex.toString
}

/** The domain of a DiscreteVar.  It has a finite size and provides a DiscreteValue for each integer from 0 to size-1.
    Because DiscreteDomain is an IndexedSeq it can be passed as a "sizeProxy" to various "growable tensors", etc.
    @author Andrew McCallum */
class DiscreteDomain(sizeProxy:Iterable[Any]) extends IndexedSeq[DiscreteValue] with VectorDomain with Domain {
  thisDomain =>
  type Value <: variable.DiscreteValue
  def this(size:Int) = { this(null.asInstanceOf[Iterable[Any]]); _size = size }
  def dimensionDomain: DiscreteDomain = this
  /** If true, do not allow this domain to change. */
  protected var _frozen = false
  override def freeze(): Unit = _frozen = true
  def unfreeze(): Unit = _frozen = false
  /** Can new category values be added to this Domain? */
  def frozen = _frozen
  var maxRequestedInt: Int = 0

  /** Maps from integer index to the DiscreteValue objects */
  private val __elements = new scala.collection.mutable.ArrayBuffer[Value]
  def _elements = __elements // Define this way so that _elements can be overridden

  // If _size >= 0, _size is used to determine DiscreteDomain.size, otherwise _sizeProxy.size is used. 
  private var _size = -1
  private val _sizeProxy = sizeProxy
  def length = if (_size >= 0) _size else _sizeProxy.size
  def apply(index:Int): Value = {
    if (index > maxRequestedInt) maxRequestedInt = index
    if (index >= size) throw new IllegalArgumentException("DiscreteDomain.getValue: index "+index+" larger than size "+size)
    if (index >= __elements.size) {
      __elements synchronized { for (i <- __elements.size to index) __elements += new DiscreteValue(i).asInstanceOf[Value] }
    } //.asInstanceOf[Value] // Here new a DiscreteValue gets created
    __elements(index)
  }
  def unapply(value:Value): Option[Int] = value match { // TODO Is this callable?
    case dv:DiscreteValue => Some(dv.intValue)  //if (value.domain == this) Some(value.intValue) else None
    case _ => None
  }
  override def iterator: Iterator[Value] = _elements.iterator
  def getAll(c: Iterator[Int]) = c map apply
  def getAll(c: List[Int]) = c map apply
  def getAll(c: Array[Int]) = c map apply
  def getAll(c: Set[Int]) = c map apply

  protected class DiscreteValue(val singleIndex:Int) extends variable.DiscreteValue {
    def domain = thisDomain
    def dim1 = thisDomain.size // Do this rather than use a constant, so that it will grow dynamically if necessary.
    override def equals(other:Any): Boolean = 
      other match { case other:DiscreteValue => this.singleIndex == other.singleIndex; case _ => false }
    // Note that we aren't also insisting that the Domain objects match, although it might be nice to do so. -akm
  }
}

/** A Cubbie for serializing a DiscreteDomain.
    Note that this Cubbie can only serialize DiscreteDomains with a fixed size, not with a growable sizeProxy.
    @author Andrew McCallum */
class DiscreteDomainCubbie extends Cubbie {
  val size = IntSlot("size")
  def store(d: DiscreteDomain): Unit = size := d.size
  def fetch(): DiscreteDomain = new DiscreteDomain(size.value)
}
