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

package cc.factorie.variable

import cc.factorie.la._
import cc.factorie.util.Cubbie

// TODO Consider name "DiscreteVectorDomain", since I think all our use cases are Tensor1 -akm

/** A Domain for variables whose value is a Tensor whose length matches the size of a DiscreteDomain. 
    This domain has a non-negative integer size.  The method 'dimensionDomain' is abstract. */
trait VectorDomain extends Domain {
  type Value <: Tensor1
  def dimensionDomain: DiscreteDomain
  /** A convenience method to get the size of the dimensionDomain.
      This method is often used to determine the dimensions of parameter Weights Tensors to allocate. */
  def dimensionSize: Int = dimensionDomain.size
  def dimensionName(i:Int): String = i.toString
  def freeze(): Unit = dimensionDomain.freeze()
}

// TODO Consider instead
// class VectorDomain(val dimensionDomain:DiscreteDomain) extends Domain[Tensor1]
//   def this(size:Int) = this(new DiscreteDomain(size))
//   def this(sizeProxy:Iterable[_]) = this(new DiscreteDomain(sizeProxy))

/** A Cubbie for serializing a VectorDomain.
    It only saves the dimensionDomain.size. */
class VectorDomainCubbie extends Cubbie {
  val size = IntSlot("size")
  def store(d: VectorDomain): Unit = size := d.dimensionDomain.size
  def fetch(): VectorDomain = new VectorDomain {
    def dimensionDomain = new DiscreteDomain(size.value)
  }
}


/** An abstract variable whose value is a one-dimensional Tensor whose length matches the size of a DiscreteDomain. */
trait VectorVar extends TensorVar {
  type Value <: Tensor1
  def value: Value
  def domain: VectorDomain
  def contains(index:Int): Boolean = value.apply(index) != 0.0
}

/** A concrete variable whose value is a one-dimensional Tensor whose length matches the size of a DiscreteDomain. */
abstract class VectorVariable extends MutableTensorVar with VectorVar {
  type Value = Tensor1
  def this(initialValue:Tensor1) = { this(); set(initialValue)(null) }
}
