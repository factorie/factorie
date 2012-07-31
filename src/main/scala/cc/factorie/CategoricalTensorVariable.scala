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

// TODO Just a placeholder for now
trait CategoricalTensorDomain[C] extends DiscreteTensorDomain { thisDomain =>
  type CategoryType = C
  def dimensionDomain: CategoricalDomain[C] = _dimensionDomain
  lazy val _dimensionDomain: CategoricalDomain[C] = new CategoricalDomain[C] {
    override def filename = thisDomain.filename
  }
}

trait CategoricalTensorVar[C] extends DiscreteTensorVar {
  def domain: CategoricalTensorDomain[C]
  /** If false, then when += is called with a value (or index) outside the Domain, an error is thrown.
      If true, then no error is thrown, and request to add the outside-Domain value is simply ignored. */
  def skipNonCategories = false
  protected def doWithIndexSafely(elt:C, v:Double, update:Boolean): Unit = {
    val i = domain.dimensionDomain.index(elt)
    if (i == CategoricalDomain.NULL_INDEX) {
      if (!skipNonCategories) throw new Error("CategoricalTensorVar += value " + value + " not found in domain " + domain)
    } else {
      if (update) tensor.update(i, v)
      else tensor.+=(i, v)
    }
  }
  // Consider re-adding this "update" method if necessary, but reconsider its name; should it have a Diff argument?
  //def update(elt:C, newValue:Double): Unit = doWithIndexSafely(elt, newValue, true)
  def +=(elt:C, incr:Double): Unit = doWithIndexSafely(elt, incr, false)
  def +=(elt:C): Unit = +=(elt, 1.0)
  @deprecated("Use this.tensor.+= instead.") def +=(index:Int): Unit = tensor.+=(index, 1.0) // For handling EnumDomain Values
  def ++=(elts:Iterable[C]): Unit = elts.foreach(this.+=(_))
  @deprecated("This method may be removed.") def zero(): Unit = tensor.zero()
  def activeCategories: Seq[C] = tensor.activeDomain.map(i => domain.dimensionDomain.category(i))
}
abstract class CategoricalTensorVariable[C] extends DiscreteTensorVariable with CategoricalTensorVar[C] {
  def this(initialValue:Tensor) = { this(); _set(initialValue) }
}

abstract class BinaryFeatureVectorVariable[C] extends CategoricalTensorVariable[C] {
  def this(initVals:Iterable[C]) = { this(); this.++=(initVals) }
  _set(new GrowableSparseBinaryTensor1(domain.dimensionDomain))
  override def toString: String = activeCategories.mkString(printName+"(", ",", ")")
}

abstract class FeatureVectorVariable[C] extends CategoricalTensorVariable[C] {
  def this(initVals:Iterable[C]) = { this(); this.++=(initVals) }
  _set(new GrowableSparseTensor1(domain.dimensionDomain))
  override def toString: String = {
    val b = new StringBuilder; b append printName; b append "("
    tensor.foreachActiveElement((i,v) => {
      b append domain.dimensionDomain.category(i)
      b append "="; b append v; b append ","
    })
    b.dropRight(1); b.append(")"); b.toString
  } 
}
