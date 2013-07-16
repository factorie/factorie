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
import java.io._
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import cc.factorie.util.Cubbie

// TODO Consider name "DiscreteVectorDomain", since I think all our use cases are Tensor1 -akm

/** A Domain for variables whose value is a Tensor whose length matches the size of a DiscreteDomain. 
    This domain has a non-negative integer size.  The method 'dimensionDomain' is abstract. */
trait DiscreteTensorDomain extends Domain[Tensor1] {
  def dimensionDomain: DiscreteDomain
  /** A convenience method to get the size of the dimensionDomain.
      This method is often used to determine the dimensions of parameter Weights Tensors to allocate. */
  def dimensionSize: Int = dimensionDomain.size
  def dimensionName(i:Int): String = i.toString
  def freeze(): Unit = dimensionDomain.freeze()
}

/** A Cubbie for serializing a DiscreteTensorDomain.
    It only saves the dimensionDomain.size. */
class DiscreteTensorDomainCubbie extends Cubbie {
  val size = IntSlot("size")
  def store(d: DiscreteTensorDomain): Unit = size := d.dimensionDomain.size
  def fetch(): DiscreteTensorDomain = new DiscreteTensorDomain {
    def dimensionDomain = new DiscreteDomain(size.value)
    type Value = Tensor1
  }
}

// TODO Consider renaming this to DiscreteVectorVar or DiscreteTensorVar1? -akm
// No, for now, saying "Discrete" and "Categorical" will be enough to indicate that it does not have higher rank.

/** An abstract variable whose value is a Tensor whose length matches the size of a DiscreteDomain. */
trait DiscreteTensorVar extends TypedTensorVar[Tensor1] {
  def domain: DiscreteTensorDomain
  def contains(index:Int): Boolean = value.apply(index) != 0.0
}

/** A concrete variable whose value is a Tensor1 whose length matches the size of a DiscreteDomain. */
abstract class DiscreteTensorVariable extends MutableTensorVar[Tensor1] with DiscreteTensorVar {
  def this(initialValue:Tensor1) = { this(); set(initialValue)(null) }
}
