/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie
import scala.collection.mutable.ArrayBuffer
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}
import scala.util.Sorting

trait VectorValue extends DiscreteValues {
  this: Variable =>
  // TODO Remember that DiscreteValues Domains currently need special initialization
  def vector : Vector
  def indices: Collection[Int]
}

trait CategoricalVectorValue[T] extends Variable with VectorValue with CategoricalValues[T] {
  // TODO Anything to put here?
}

/** A variable whose value is a SparseBinaryVector; immutable.  
    If the second constructor is false, then attempting to += a category 
    for which the CategoryDomain returns CategoryDomain.NULL_INDEX == -1 will result in a throw exception.
    If not specified, it defaults to false. */
// I considered renaming this VectorObservation, but then I realized that methods such as += change its value. -akm
// TODO Rename to BinaryVectorVariable?
// TODO Consider renaming BinaryFeatureVector (where "Feature") refers to being Categorical?
// or perhaps BinaryCategoricalVector?  But that is a weird name.
@DomainInSubclasses
abstract class BinaryVectorVariable[T<:AnyRef](initVals:Iterable[T]) extends CategoricalVectorValue[T] {
  //def this(iv:T*) = this(iv:Seq[T])
  def this() = this(null)
  //def this(initVals:Iterable[T]) = this(initVals, false)
  type VariableType <: BinaryVectorVariable[T];
  def skipNonCategories = false
  //private val _indices = new it.unimi.dsi.fastutil.ints.IntLinkedOpenHashSet
  private val _indices: ArrayBuffer[Int] = new ArrayBuffer[Int]()
  private var _vector: Vector = null // TODO Can we make this more memory efficient?  Avoid having both Vector and ArrayBuffer?;
  if (initVals ne null) this ++= initVals
  def indices: Seq[Int] = _indices // TODO project to ensure no changes, even with casting?  But this would involve allocating the Projection
  def values: Seq[T] = { val d = this.domain; val result = new ArrayBuffer[T](_indices.size); _indices.foreach(result += d.get(_)); result }
  //{ val indxs = _indices.toIntArray.asInstanceOf[Array[Int]]; val d = this.domain; val result = new ArrayBuffer[T](indxs.length) ++= indxs.map(d.get(_)); result }
  def zero: Unit = { _indices.clear; _vector = null }
  override def vector = {
    if (_vector == null || _vector.size != domain.allocSize) {
      val indices = _indices.toArray
      Sorting.quickSort(indices)
      _vector = new SparseBinaryVector(domain.allocSize, indices)
    }
    _vector
  }
  def incrementInto(x:{def increment(i:Int,x:Double)(implicit d:DiffList):Unit}): Unit = _indices.foreach(i => x.increment(i,1.0)(null))
  // TODO when we have Scala 2.8, add to the method below difflist argument with default value null
  // But will a += b syntax with with default arguments?
  def +=(value: T) : Unit = {
    val idx = domain.index(value);
    if (idx == CategoricalDomain.NULL_INDEX) {
      if (!skipNonCategories)
        throw new Error("BinaryVectorVariable += value " + value + " not found in domain " + domain)
      else
        return
    }
    if (!_indices.contains(idx)) _indices += idx
    _vector = null
  }
  def +=(index:Int): Unit = {
    if (!_indices.contains(index)) _indices += index
    _vector = null
  }
  //def +(value: T) = {this += value; this} // TODO Shouldn't this method actually return a new VectorVariable, leaving old one unchanged?  Yes.
  def ++=(vals: Iterable[T]) : Unit = vals.foreach(v => this += v)
  //def ++(vals: Iterable[T]) = {this ++= vals; this} // TODO this method should return a new Vector
  override def toString = {
    val s = new StringBuilder(printName + "(")
    val iter = vector.activeDomain.iterator
    if (iter.hasNext) { val i:Int = iter.next ; s ++= (domain.get(i).toString + "=" + i) }
    while (iter.hasNext) {
      val i:Int = iter.next
      s ++= ("," + domain.get(i).toString + "=" + i)
    }
    s ++= ")"
    s.toString
  }
}

/** A vector of Real values */
@DomainInSubclasses
abstract class RealVectorVariable[T](initVals:Iterable[(T,Double)]) extends CategoricalVectorValue[T] {
  def this() = this(null)
  type VariableType <: RealVectorVariable[T]
  lazy val vector: Vector = new SparseVector(domain.allocSize)
  if (initVals ne null) this ++= initVals
  def indices : Collection[Int] = if (vector == null) Nil else vector.activeDomain
  def +=(pair:(T,Double)) = vector((domain.index(pair._1))) = pair._2
  def ++=(pairs:Iterable[(T,Double)]) = pairs.foreach(pair => this += pair)
}

