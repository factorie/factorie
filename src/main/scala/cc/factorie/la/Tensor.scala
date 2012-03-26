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
import cc.factorie.util._

// Preliminary version of the upcoming "unflattening" of parameter and discrete statistics representations...

// Note: Many Tensor-like methods are actually implemented in DoubleSeq
trait Tensor extends DoubleSeq {
  def numDimensions: Int
  def dimensions: Array[Int]
  // For handling sparsity
  def activeDomain: IntSeq
  def activeDomains: Array[IntSeq]
  def isDense: Boolean
  def activeDomainSize: Int = activeDomain.length // Should be overridden for efficiency in many subclasses
  /** The default value at indices not covered by activeDomain.  Subclasses may override this  */
  def defaultValue: Double = 0.0
  def foreachActiveElement(f:(Int,Double)=>Unit): Unit = { val d = activeDomain; var i = 0; while (i < d.length) { f(d(i), apply(d(i))); i += 1 } }
  def activeElements: Iterator[(Int,Double)] = (for (i <- activeDomain.toArray) yield (i, apply(i))).iterator
  def forallActiveElements(f:(Int,Double)=>Boolean): Boolean = throw new Error("Not yet implemented.")
  // Override various methods that can use activeDomain for extra efficiency // TODO More of these should be overridden
  override def sum: Double = 
    if (isDense) super.sum
    else { val d = activeDomain; var s = 0.0; var i = 0; while (i < d.length) { s += apply(d(i)); i += 1 }; s } // assumes non-active are zero
  override def max: Double = 
    if (isDense) super.max
    else { val d = activeDomain; var m = defaultValue; var i = 0; while (i < d.length) { if (!(m >= apply(d(i)))) m = apply(d(i)); i += 1 }; m }
  // TODO Consider methods like +, -, *, /
  def stringPrefix = "Tensor"
  override def toString = this.asSeq.mkString(stringPrefix+"(", ",", ")")
}

/** Used by Proportions, where apply() is specially defined to make update() dangerous; hence this isn't fully Mutable. */
trait IncrementableTensor extends Tensor with IncrementableDoubleSeq

trait MutableTensor extends IncrementableTensor with MutableDoubleSeq

trait TensorWithMutableDefaultValue extends Tensor {
  def defaultValue_=(v:Double): Unit
  def defaultValue_+=(v:Double): Unit = defaultValue_=(defaultValue + v)
  def defaultValue_*=(v:Double): Unit = defaultValue_=(defaultValue * v)
}


/** A lazy product of a Vector and a scalar.
    @author Andrew McCallum */
class TensorTimesScalar(val tensor:Tensor, val scalar:Double) extends Tensor {
  def numDimensions: Int = tensor numDimensions
  def dimensions: Array[Int] = tensor.dimensions
  // For handling sparsity
  def activeDomain: IntSeq = tensor.activeDomain
  def activeDomains: Array[IntSeq] = tensor.activeDomains
  def isDense: Boolean = tensor.isDense
  def length = tensor.length
  //def activeDomainSize: Int = vector.activeDomainSize
  def dot(t:Tensor): Double = tensor.dot(t) * scalar
  def *(scalar:Double) = new TensorTimesScalar(tensor, scalar*this.scalar)
  //override def update(i:Int, v:Double): Unit = tensor.update(idx, value/scalar)
  //override def +=(v: Vector) { vector += v*(1.0/scalar) }
  def apply(index:Int) = tensor.apply(index) * scalar
}

