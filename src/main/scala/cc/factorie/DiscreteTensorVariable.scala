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
import cc.factorie.la._
import scala.util.Random
import scala.collection.mutable.{ArrayBuffer,HashMap}

/** A Domain for variables whose value is a Tensor whose length matches the size of a DiscreteDomain. 
    This domain has a non-negative integer size.  The method 'dimensionDomain' is abstract. */
trait DiscreteTensorDomain[+T<:Tensor] extends TensorDomain with ValueType[T] {
  def dimensionDomain: CategoricalDomain2[_]
  /** The maximum size to which this domain will be allowed to grow.  
      The 'dimensionDomain.size' method may return values smaller than this, however.
      This method is used to pre-allocate a Template's parameter arrays and is useful for growing domains. */
  def dimensionSize: Int = dimensionDomain.size
  def dimensionName(i:Int): String = i.toString
  def freeze(): Unit = dimensionDomain.freeze
  override def save(dirname: String, gzip: Boolean = false) {
    // TODO: Note that if multiple domains have same dimension domains, it will be written multiple times
    dimensionDomain.save(dirname, gzip)
  }
  override def load(dirname: String, gzip: Boolean = false) {
    // TODO: Note that the dimensionDomain might get read multiple times
    if(!dimensionDomain.frozen) dimensionDomain.load(dirname, gzip)
  }
}

// TODO Just a placeholder for now
trait CategoricalTensorDomain[+T<:Tensor,C] extends DiscreteTensorDomain[T] 

trait DiscreteTensorVar[+A<:Tensor] extends TensorVar[A] with VarAndValueType[DiscreteTensorVar[A],A] {
  def domain: DiscreteTensorDomain[A]
}

/** A vector with dimensions corresponding to a DiscreteDomain, and with Double weights for each dimension, represented by a sparse vector. */
abstract class DiscreteTensorVariable[A<:Tensor] extends TensorVariable[A] with DiscreteTensorVar[A] {
  //thisVariable =>
  //_set(new SparseVector(domain.dimensionSize) with DiscreteVectorValue { def domain = thisVariable.domain })
}

trait CategoricalTensorVar[+T<:Tensor,C] extends DiscreteTensorVar[T] {
  def domain: CategoricalTensorDomain[T,C]
}
abstract class CategoricalTensorVariable[T<:Tensor,C] extends DiscreteTensorVariable[T] with CategoricalTensorVar[T,C]

abstract class SparseCategoricalTensorVariable1[C] extends CategoricalTensorVariable[GrowableSparseTensor1,C] {
  _set(new GrowableSparseTensor1(domain.dimensionDomain))
}


// A sketch for the future:

abstract class CategoricalValue2[@specialized(Int, Boolean) C](val singleIndex:Int, val category:C) extends SingletonBinaryTensorLike1 {
  def domain: CategoricalDomain2[C]
  @inline final def intValue: Int = singleIndex // TODO Consider swapping singleIndex <-> intValue
  @inline final def dim1 = domain.size
}


class CategoricalDomain2[@specialized(Int, Boolean) C] extends IndexedSeq[CategoricalValue2[C]] with CategoricalTensorDomain[CategoricalValue2[C],C] with ValueType[CategoricalValue2[C]] {
  thisDomain =>
  def this(values:Iterable[C]) = { this(); values.foreach(value(_)) }
  private var _frozen: Boolean = false
  def frozen: Boolean = _frozen
  private val _elements = new ArrayBuffer[ValueType]
  private val _indices = new HashMap[C,ValueType] with collection.mutable.SynchronizedMap[C, ValueType] //new HashMap[C,ValueType]
  /** If positive, throw error if size tries to grow larger than it.  Use for growable multi-dim Factor weights;
      override this method with the largest you think your growable domain will get. */
  var maxSize = -1
  def dimensionDomain = throw new Error("Not yet implemented")
  @inline final def length = _elements.length
  def value(category:C): ValueType = {
    if (_frozen) _indices.getOrElse(category, null.asInstanceOf[ValueType])
    else {
      if (_indices.contains(category)) { // double-tap locking necessary to ensure only one thread adds to _indices
        _indices.synchronized({
          if (_indices.get(category).isEmpty) {
            val m = _elements.size
            if (maxSize > 0 && m >= maxSize) throw new Error("Index size exceeded maxSize")
            val e: ValueType = new CategoricalValue2(m, category) { def domain = thisDomain }.asInstanceOf[ValueType]
            _elements += e
            _indices(category) = e
          }
        })
      }
      _indices.getOrElse(category, null)
    }
  }
  def apply(i:Int): ValueType = _elements(i)
  def category(i:Int): C = _elements(i).category
  def categories: Seq[C] = _elements.map(_.category)
  def index(category: C): Int = {
    val v = value(category)
    if (v eq null) -1 else v.intValue
  }
  /** Like index, but throw an exception if the category is not already there. */
  def getIndex(category:C): Int = _indices.getOrElse(category, throw new Error("Category not present; use index() to cause the creation of a new value.")).intValue

  def +=(x:C) : Unit = this.value(x)
  def ++=(xs:Traversable[C]) : Unit = xs.foreach(this.index(_))
  /** Wipe the domain and its indices clean */
  def clear(): Unit = { _frozen = false; _elements.clear(); _indices.clear() }
  // Separate argument types preserves return collection type
  def indexAll(c: Iterator[C]) = c map index;
  def indexAll(c: List[C]) = c map index;
  def indexAll(c: Array[C]) = c map index;
  def indexAll(c: Set[C]) = c map index;
  def getAllValues(c: Iterator[Int]) = c map apply
  def getAllValues(c: List[Int]) = c map apply
  def getAllValues(c: Array[Int]) = c map apply
  def getAllValues(c: Set[Int]) = c map apply

  override def dimensionName(i:Int): String = category(i).toString
  override def toString = "CategoricalDomain2[]("+size+")"
}

