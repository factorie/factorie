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

/** A Domain for variables whose Tensor1 value has length matching the size of a DiscreteDomain.
    The method 'dimensionDomain' is abstract and should return this DiscreteDomain. 
    @author Andrew McCallum */
trait VectorDomain extends Domain {
  type Value <: Tensor1
  def dimensionDomain: DiscreteDomain
  /** A convenience method to get the size of the dimensionDomain.
      This method is often used to determine the dimensions of parameter Weights Tensors to allocate. */
  def dimensionSize: Int = dimensionDomain.length
  def dimensionName(i:Int): String = i.toString
  def freeze(): Unit = dimensionDomain.freeze()
}

/** A Cubbie for serializing a VectorDomain.
    It only saves the dimensionDomain.size. 
    @author Andrew McCallum */
class VectorDomainCubbie extends Cubbie {
  val size = IntSlot("size")
  def store(d: VectorDomain): Unit = size := d.dimensionDomain.size
  def fetch(): VectorDomain = new VectorDomain {
    def dimensionDomain = new DiscreteDomain(size.value)
  }
}

/** An abstract variable whose value is a one-dimensional Tensor whose length matches the size of a DiscreteDomain. 
    @author Andrew McCallum */
trait VectorVar extends TensorVar {
  type Value <: Tensor1
  def value: Value
  def domain: VectorDomain
  def contains(index:Int): Boolean = value.apply(index) != 0.0
}

/** A concrete variable whose value is a one-dimensional Tensor whose length matches the size of a DiscreteDomain. 
    @author Andrew McCallum */
abstract class VectorVariable extends MutableTensorVar with VectorVar {
  type Value = Tensor1
  def this(initialValue:Tensor1) = { this(); set(initialValue)(null) }
}
