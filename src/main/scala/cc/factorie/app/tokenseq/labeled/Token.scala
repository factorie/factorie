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

/** A word token in a linear sequence of tokens.  It is a constituent of a LabeledTokenSeq.
 Its value is a BinaryFeatureVectorVariable, its feature vector.
 It provides access to its neighbors in the sequence and its label.  It also has an entity-relationship counterpart. */
@DomainInSubclasses
abstract class Token[S<:TokenSeq[This,L,S],L<:Label[S,This,L], This<:Token[S,L,This] /*with VarInSeq[This]*/ ](theWord:String, features:Seq[String] = Nil)
extends cc.factorie.app.tokenseq.Token[S,This](theWord, features) {
  this: This =>
  type GetterType <: TokenGetter[S,L,This]
  class GetterClass extends TokenGetter[S,L,This]
  val label: L //= new Label(labelString, this)
}
  
/** Implementation of the entity-relationship language we can use with Token objects. */
class TokenGetter[S<:TokenSeq[T,L,S],L<:Label[S,T,L],T<:Token[S,L,T]] extends cc.factorie.app.tokenseq.TokenGetter[S,T] {
  def newLabelGetter = new LabelGetter[S,T,L]
  /** Go from a token to its label. */
  def label = initOneToOne[L](newLabelGetter,
                              token=>token.label.asInstanceOf[L], 
                              label => label.token)
}
