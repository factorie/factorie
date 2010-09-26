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

package cc.factorie.app.tokenseq.labeled
import cc.factorie._
import cc.factorie.er._

/** A Label associated with a Token. */
@DomainInSubclasses
abstract class Label[S<:TokenSeq[T,This,S],T<:Token[S,This,T],This<:Label[S,T,This]](labelname: String, val token: T) extends LabelVariable(labelname) with AbstractVarInSeq[This] with Entity[This] {
  this: This =>
  type GetterType <: LabelGetter[S,T,This]
  class GetterClass extends LabelGetter[S,T,This]
  def hasNext = token.hasNext && token.next.label != null
  def hasPrev = token.hasPrev && token.prev.label != null
  def next: This = if (token.next == null) null.asInstanceOf[This] else token.next.label
  def prev: This = if (token.prev == null) null.asInstanceOf[This] else token.prev.label
}

// Define boilerplate, to support access to attributes in the entity-attribute-relationship syntax
class LabelGetter[S<:TokenSeq[T,ThisLabel,S],T<:Token[S,ThisLabel,T],ThisLabel<:Label[S,T,ThisLabel]] extends EntityGetter[ThisLabel] {
  def newTokenGetter = new TokenGetter[S,ThisLabel,T]
  def newLabelGetter = new LabelGetter[S,T,ThisLabel]
  def token = initOneToOne[T](newTokenGetter, label => label.token.asInstanceOf[T], token => token.label)
  def next = initManyToMany[ThisLabel](newLabelGetter,
                                       label => if (!label.token.hasNext) Nil else List(label.token.next.label),
                                       label => if (!label.token.hasPrev) Nil else List(label.token.prev.label))
  def prev = initManyToMany[ThisLabel](newLabelGetter,
                                       label => if (!label.token.hasPrev) Nil else List(label.token.prev.label),
                                       label => if (!label.token.hasNext) Nil else List(label.token.next.label))
}
