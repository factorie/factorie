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
import scala.collection.mutable.ArrayBuffer

/** A Domain for variables whose value is a Tensor whose length matches the size of a DiscreteDomain. 
    This domain has a non-negative integer size.  The method 'dimensionDomain' is abstract. */
trait DiscreteTensorDomain[+T<:Tensor] extends TensorDomain with ValueType[T] {
  def dimensionDomain: DiscreteDomain
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


