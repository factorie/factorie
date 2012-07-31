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

import scala.collection.mutable.ArrayBuffer
import scala.math
import java.util.Arrays

class CategoricalSeqDomain[C] extends DiscreteSeqDomain with Domain[Seq[CategoricalValue[C]]] {
  lazy val elementDomain: CategoricalDomain[C] = new CategoricalDomain[C]
}
abstract class CategoricalSeqVariable[C] extends DiscreteSeqVariable with IndexedSeqVar[CategoricalValue[C]] /*VarAndElementType[CategoricalSeqVariable[C],CategoricalValue[C]]*/ {
  def this(initialValue:Seq[C]) = {
    this()
    _setCapacity(if (initialValue.length > 0) initialValue.length else 1)
    val d = domain.elementDomain
    initialValue.foreach(c => this += d.value(c))
  }
  def domain: CategoricalSeqDomain[C]
  def appendCategory(x:C): Unit = {
    //this += domain.elementDomain.value(x)
    val index = domain.elementDomain.index(x)
    if (index >= 0) _append(index)
    // TODO Should we throw an error if that domain returns -1 (indicating it is locked, and can't add this category)?
  }
  def appendCategories(xs:Iterable[C]): Unit = xs.foreach(appendCategory(_)) //_appendAll(xs.map(c => domain.elementDomain.index(c)).toArray)
  def categoryValue(seqIndex:Int): C = domain.elementDomain.category(_apply(seqIndex))
  def categoryValues: Seq[C] = Seq.tabulate(length)(i => categoryValue(i))
}
