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

import cc.factorie.util.Cubbie

import scala.collection.mutable

/** A Domain for sequences of CategoricalValues.
    @author Andrew McCallum */
class CategoricalSeqDomain[C] extends DiscreteSeqDomain with Domain {
  type Value = Seq[CategoricalValue[C]]
  lazy val elementDomain: CategoricalDomain[C] = new CategoricalDomain[C]
}

/** A Cubbie for serializing a CategoricalSeqDomain.
    It saves the elementDomain containing the sequence of categories.
    @author Luke Vilnis */
class CategoricalSeqDomainCubbie[T](val csd: CategoricalSeqDomain[T]) extends Cubbie {
  val elementDomainCubbie = new CategoricalDomainCubbie[T](csd.elementDomain)
  setMap(new mutable.Map[String, Any] {
    override def update(key: String, value: Any): Unit = {
      if (key == "elementDomain") {
        val map = value.asInstanceOf[mutable.Map[String, Any]]
        for((k,v) <- map) elementDomainCubbie._map(k) = v
      } else sys.error("Unknown cubbie slot key: \"%s\"" format key)
    }
    // TODO We should be using CategoricalDomain.stringToCategory somewhere here. -akm
    def += (kv: (String, Any)): this.type = { update(kv._1, kv._2); this }
    def -= (key: String): this.type = sys.error("Can't remove slots from cubbie map!")
    def get(key: String): Option[Any] =
      if (key == "elementDomain") Some(elementDomainCubbie._map)
      else None
    def iterator: Iterator[(String, Any)] = List("elementDomain").map(s => (s, get(s).get)).iterator
  })
}

/** A variable whose values are sequences of CategoricalValues.
    The method 'domain' is abstract.
    @author Andrew McCallum */
abstract class CategoricalSeqVariable[C] extends MutableDiscreteSeqVar[CategoricalValue[C]] with IndexedSeqVar[CategoricalValue[C]] /*VarAndElementType[CategoricalSeqVariable[C],CategoricalValue[C]]*/ {
  type Value = IndexedSeq[CategoricalValue[C]]
  def this(initialValue:Seq[C]) = {
    this()
    _setCapacity(if (initialValue.length > 0) initialValue.length else 1)
    val d = domain.elementDomain
    initialValue.foreach(c => this += d.value(c))
  }
  def domain: CategoricalSeqDomain[C]
  def skipNonCategories = domain.elementDomain.frozen
  def appendCategory(x:C): Unit = {
    val index = domain.elementDomain.index(x)
    if (index >= 0) _append(index)
    else if (!skipNonCategories) throw new Error("appendCategory "+x+" not found in domain.")
  }
  def appendCategories(xs:Iterable[C]): Unit = xs.foreach(appendCategory) //_appendAll(xs.map(c => domain.elementDomain.index(c)).toArray)
  def categoryValue(seqIndex:Int): C = domain.elementDomain.category(_apply(seqIndex))
  def categoryValues: Seq[C] = Seq.tabulate(length)(i => categoryValue(i))
}
