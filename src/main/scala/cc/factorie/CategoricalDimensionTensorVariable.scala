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
import collection.mutable

/** A domain over Tensor values, where the dimensions of the Tensor correspond to a CategoricalDomain.
    This trait is often used for the domain of feature vectors.
    @author Andrew McCallum */
trait CategoricalDimensionTensorDomain[C] extends DiscreteDimensionTensorDomain { thisDomain =>
  type CategoryType = C
  def dimensionDomain: CategoricalDomain[C] = _dimensionDomain
  def stringToCategory(s:String): C = s.asInstanceOf[C]
  lazy val _dimensionDomain: CategoricalDomain[C] = new CategoricalDomain[C] {
    final override def stringToCategory(s:String): C = CategoricalDimensionTensorDomain.this.stringToCategory(s)
  }
}

/** A Cubbie for serializing CategoricalDimensionTensorDomain.
    It stores the sequence of categories.
    @author Luke Vilnis */
class CategoricalDimensionTensorDomainCubbie[T](val cdtd: CategoricalDimensionTensorDomain[T]) extends Cubbie {
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
trait CategoricalDimensionTensorVar[C] extends DiscreteDimensionTensorVar {
  def domain: CategoricalDimensionTensorDomain[C]
  /** If false, then when += is called with a value (or index) outside the Domain, an error is thrown.
      If true, then no error is thrown, and request to add the outside-Domain value is simply ignored. */
  def skipNonCategories = false
  protected def doWithIndexSafely(elt:C, v:Double, update:Boolean): Unit = {
    val i = domain.dimensionDomain.index(elt)
    if (i == CategoricalDomain.NULL_INDEX) {
      if (!skipNonCategories) throw new Error("CategoricalTensorVar.+= " + elt + " not found in domain " + domain)
    } else {
      if (update) tensor.update(i, v)
      else tensor.+=(i, v)
    }
  }
  // Consider re-adding this "update" method if necessary, but reconsider its name; should it have a Diff argument?
  //def update(elt:C, newValue:Double): Unit = doWithIndexSafely(elt, newValue, true)
  def +=(elt:C, incr:Double): Unit = doWithIndexSafely(elt, incr, update = false)
  def +=(elt:C): Unit = +=(elt, 1.0)
  def ++=(elts:Iterable[C]): Unit = elts.foreach(this.+=(_))
  @deprecated("Use this.tensor.+= instead?") def +=(index:Int): Unit = tensor.+=(index, 1.0) // For handling EnumDomain Values
  def activeCategories: Seq[C] = tensor.activeDomain.map(i => domain.dimensionDomain.category(i))
}

/** An abstract variable whose value is a Tensor whose length matches the size of a CategoricalDomain,
    and whose dimensions each correspond to a category.
    These are commonly used for feature vectors, with String categories.
    The 'dimensionDomain' is abstract.
    @author Andrew McCallum */
abstract class CategoricalDimensionTensorVariable[C] extends MutableTensorVar[Tensor] with CategoricalDimensionTensorVar[C] {
  def this(initialValue:Tensor) = { this(); set(initialValue)(null) }
}

/** The standard variable for holding binary feature vectors.
    It is a CategoricalDimensionTensorVariable initialized with a GrowableSparseBinaryTensor1 value.
    @author Andrew McCallum */
abstract class BinaryFeatureVectorVariable[C] extends CategoricalDimensionTensorVariable[C] {
  def this(initVals:Iterable[C]) = { this(); this.++=(initVals) }
  set(new GrowableSparseBinaryTensor1(domain.dimensionDomain))(null)
  override def toString: String = activeCategories.mkString(printName+"(", ",", ")")
}

/** The standard variable for holding feature vectors with non-binary values.
    It is a CategoricalDimensionTensorVariable initialized with a GrowableSparseBinaryTensor1 value.
    @author Andrew McCallum */
abstract class FeatureVectorVariable[C] extends CategoricalDimensionTensorVariable[C] {
  def this(initVals:Iterable[C]) = { this(); this.++=(initVals) }
  set(new GrowableSparseTensor1(domain.dimensionDomain))(null)
  override def toString: String = {
    val b = new StringBuilder; b append printName; b append "("
    tensor.foreachActiveElement((i,v) => {
      b append domain.dimensionDomain.category(i)
      b append "="; b append v; b append ","
    })
    b.dropRight(1); b.append(")"); b.toString
  } 
}
