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

import scala.collection.mutable

/** A domain over Tensor values, where the dimensions of the Tensor correspond to a CategoricalDomain.
    This trait is often used for the domain of feature vectors.
    @author Andrew McCallum */
trait CategoricalVectorDomain[C] extends VectorDomain { thisDomain =>
  type CategoryType = C
  def dimensionDomain: CategoricalDomain[C] = _dimensionDomain
  /** Use for de-serialization */
  def stringToCategory(s:String): C = s.asInstanceOf[C]
  lazy val _dimensionDomain: CategoricalDomain[C] = new CategoricalDomain[C] {
    final override def stringToCategory(s:String): C = CategoricalVectorDomain.this.stringToCategory(s)
  }
}

/** A Cubbie for serializing CategoricalVectorDomain.
    It stores the sequence of categories.
    @author Luke Vilnis */
class CategoricalVectorDomainCubbie[T](val cdtd: CategoricalVectorDomain[T]) extends Cubbie {
  val dimensionDomainCubbie = new CategoricalDomainCubbie[T](cdtd.dimensionDomain)
  setMap(new mutable.Map[String, Any] {
    override def update(key: String, value: Any): Unit = {
      if (key == "dimensionDomain") {
        val map = value.asInstanceOf[mutable.Map[String, Any]]
        for((k,v) <- map) dimensionDomainCubbie._map(k) = v
      } else sys.error("Unknown cubbie slot key: \"%s\"" format key)
    }
    def += (kv: (String, Any)): this.type = { update(kv._1, kv._2); this }
    def -= (key: String): this.type = sys.error("Can't remove slots from cubbie map!")
    def get(key: String): Option[Any] =
      if (key == "dimensionDomain") Some(dimensionDomainCubbie._map)
      else None
    def iterator: Iterator[(String, Any)] = List("dimensionDomain").map(s => (s, get(s).get)).iterator
  })
}

/** An abstract variable whose value is a Tensor whose length matches the size of a CategoricalDomain,
    and whose dimensions each correspond to a category.
    These are commonly used for feature vectors, with String categories. */
trait CategoricalVectorVar[C] extends VectorVar {
  def domain: CategoricalVectorDomain[C]
  /** If false, then when += is called with a value (or index) outside the Domain, an error is thrown.
      If true, then no error is thrown, and request to add the outside-Domain value is simply ignored. */
  def skipNonCategories = domain.dimensionDomain.frozen
  protected def doWithIndexSafely(elt:C, v:Double, update:Boolean): Unit = {
    val i = domain.dimensionDomain.index(elt)
    if (i == CategoricalDomain.NULL_INDEX) {
      if (!skipNonCategories) throw new Error("CategoricalVectorVar.+= " + elt + " not found in domain " + domain)
    } else {
      if (update) value.update(i, v)
      else value.+=(i, v)
    }
  }
  // Consider re-adding this "update" method if necessary, but reconsider its name; should it have a Diff argument?
  //def update(elt:C, newValue:Double): Unit = doWithIndexSafely(elt, newValue, true)
  def +=(elt:C, incr:Double): Unit = doWithIndexSafely(elt, incr, update = false)
  def +=(elt:C): Unit = +=(elt, 1.0)
  def ++=(elts:Iterable[C]): Unit = elts.foreach(this.+=(_))
  def activeCategories: Seq[C] = value.activeDomain.toSeq.map(i => domain.dimensionDomain.category(i))
}

// TODO we should maybe refactor this to not set the Value type to "Tensor", as this makes things require casting down the road when using this for eg classifiers on tensor1 features -luke

/** An abstract variable whose value is a Tensor whose length matches the size of a CategoricalDomain,
    and whose dimensions each correspond to a category.
    These are commonly used for feature vectors, with String categories.
    The 'dimensionDomain' is abstract.
    @author Andrew McCallum */
abstract class CategoricalVectorVariable[C] extends VectorVar with MutableTensorVar with CategoricalVectorVar[C] {
  type Value = Tensor1
  def this(initialValue:Tensor1) = { this(); set(initialValue)(null) }
}

