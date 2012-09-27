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

/** A Domain for variables whose value is a Tensor whose length matches the size of a DiscreteDomain. 
    This domain has a non-negative integer size.  The method 'dimensionDomain' is abstract. */
trait DiscreteTensorDomain extends TensorDomain {
  def dimensionDomain: DiscreteDomain
  /** The maximum size to which this domain will be allowed to grow.  
      The 'dimensionDomain.size' method may return values smaller than this, however.
      This method is used to pre-allocate a Template's parameter arrays and is useful for growing domains. */
  // TODO But actually, I don't think this is used any more.
  def dimensionSize: Int = dimensionDomain.size
  //def size: Int = dimensionDomain.size // TODO Should we keep this convenience, or is it too confusing?  Is isn't really the number of possible values in the Domain.
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

trait DiscreteTensorVar extends TensorVar {
  def domain: DiscreteTensorDomain
  def contains(index:Int): Boolean = tensor.apply(index) != 0.0
}

/** A vector with dimensions corresponding to a DiscreteDomain, and with Double weights for each dimension, represented by a sparse vector. */
abstract class DiscreteTensorVariable extends MutableTensorVar[Tensor] with DiscreteTensorVar {
  def this(initialValue:Tensor) = { this(); set(initialValue)(null) }
  //thisVariable =>
  //_set(new SparseVector(domain.dimensionSize) with DiscreteVectorValue { def domain = thisVariable.domain })
  def freeze: Unit = throw new Error("Is this still really necessary? -akm")
}


//object DocumentDomain extends DiscreteTensorDomain { val dimensionDomain = new DiscreteDomain(100) }
//class Document extends DiscreteTensorVariable(new DenseTensor1(DocumentDomain.dimensionDomain.size)) {
//  def domain = DocumentDomain
//}
//object DocumentTest {
//  val doc = new Document
//  for (i <- 0 until DocumentDomain.dimensionDomain.size) 
//    doc.tensor(i) = i
//}
