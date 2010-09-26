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

package cc.factorie.app.tokenseq
import cc.factorie._
import cc.factorie.er._
import scala.collection.mutable.{ArrayBuffer,HashSet,HashMap}
import scala.util.Sorting

/** A word token in a linear sequence of tokens.  It is a constituent of a TokenSeq.
 Its value is a BinaryFeatureVectorVariable (its feature vector).
 It provides access to its neighbors in the sequence and its label.  It also has an entity-relationship counterpart. */
@DomainInSubclasses
abstract class Token[S<:TokenSeq[This,S], This<:Token[S,This] with VarInTypedSeq[This,S]](theWord:String, features:Seq[String] = Nil)
extends BinaryFeatureVectorVariable[String](features) with VarInTypedSeq[This,S] with Entity[This] with TokenInSeq[This] 
{
  // TODO Consider unifying this with cc.factorie.app.classify.Instance
  this: This =>
  //this ++= features
  //def this(word:String) = this(word, Nil)
  type GetterType <: TokenGetter[S,This]
  class GetterClass extends TokenGetter[S,This]
  def word = theWord
  def matches(t2:Token[S,This]): Boolean = word == t2.word
  /** Return true if the first  character of the word is upper case. */
  def isCapitalized = java.lang.Character.isUpperCase(word(0))
  def isPunctuation = word.matches("\\{Punct}")
  def containsLowerCase = word.exists(c => java.lang.Character.isLowerCase(c))
  /* Return true if the word contains only digits. */
  def isDigits = word.matches("\\d+")
  /* Return true if the word contains at least one digit. */
  def containsDigit = word.matches(".*\\d.*")
  /** Return a string that captures the generic "shape" of the original word, 
   mapping lowercase alphabetics to 'a', uppercase to 'A', digits to '1', whitespace to ' '.
   Skip more than 'maxRepetitions' of the same character class. */
  def wordShape(maxRepetitions:Int) = cc.factorie.app.tokenseq.wordShape(word, maxRepetitions)
  def charNGrams(min:Int, max:Int): Seq[String] = cc.factorie.app.tokenseq.charNGrams(word, min, max)
  private lazy val svmap = new HashMap[String,BinaryFeatureVectorVariable[String]]
  def subVector(regex:String): BinaryFeatureVectorVariable[String] = svmap.getOrElseUpdate(regex, newSubVector(regex))
  private def newSubVector(regex:String): BinaryFeatureVectorVariable[String] = {
    val result = new BinaryFeatureVectorVariable[String] { override def printName = "TokenSubVector" }
    result ++= this.values.filter(s => s.matches(regex))
    result
  }
}


/** Implementation of the entity-relationship language we can use with Token objects. */
class TokenGetter[S<:TokenSeq[T,S],T<:Token[S,T]] extends EntityGetter[T] {
  def newTokenGetter = new TokenGetter[S,T];
  /** Go from a token to the next token. */
  def next = initManyToMany[T](newTokenGetter,
                               (token:T) => if (!token.hasNext) Nil else List(token.next), 
                               (token:T) => if (!token.hasPrev) Nil else List(token.prev))
  /** Go from a token to the previous token. */
  def prev = initManyToMany[T](newTokenGetter,
                               (token:T) => if (!token.hasPrev) Nil else List(token.prev), 
                               (token:T) => if (!token.hasNext) Nil else List(token.next))
  /** Go from a token to the collection of the next 'n' tokens. */
  def next(n:Int) = initManyToMany[T](newTokenGetter,
                                      (t:T) => { var i = n; var ret:List[T] = Nil; while (t.hasNext && i > 0) { ret = t.next :: ret; i += 1}; ret },
                                      (t:T) => { var i = n; var ret:List[T] = Nil; while (t.hasPrev && i > 0) { ret = t.prev :: ret; i += 1}; ret })
  /** Go from a token to the collection of the previous 'n' tokens. */
  def prev(n:Int) = initManyToMany[T](newTokenGetter,
                                      (t:T) => { var i = n; var ret:List[T] = Nil; while (t.hasPrev && i > 0) { ret = t.prev :: ret; i += 1}; ret },
                                      (t:T) => { var i = n; var ret:List[T] = Nil; while (t.hasNext && i > 0) { ret = t.next :: ret; i += 1}; ret })
  /** All the other tokens in the Sentence. */
  def sentenceTokens = initManyToMany[T](newTokenGetter,
                                         (token:T) => token.seq, 
                                         (token:T) => token.seq)
  /** Return a BooleanObservation with value true if the word of this Token is equal to 'w'.  
   Intended for use in tests in er.Formula, not as a feature itself.  
   If you want such a feature, you should += it to the Token (BinaryFeatureVectorVariable) */
  def isWord(w:String) = getOneToOne[BooleanObservationWithGetter](
    // TODO Consider making this more efficient by looking up an already-constructed instance, as in "object Bool"
    token => if (token.word == w) new BooleanObservationWithGetter(true) else new BooleanObservationWithGetter(false),
    bool => throw new Error("Constant bool shouldn't change"))
  /** Return a BooleanObservation with value true if the word of this Token is capitalized.  
   Intended for use in tests in er.Formula, not as a feature itself.  
   If you want such a feature, you should += it to the Token (BinaryFeatureVectorVariable) */
  def isCapitalized = getOneToOne[BooleanObservationWithGetter](
    // TODO Consider making this more efficient by looking up an already-constructed instance, as in "object Bool"
    token => if (java.lang.Character.isUpperCase(token.word.head)) new BooleanObservationWithGetter(true) else new BooleanObservationWithGetter(false),
    bool => throw new Error("Constant bool shouldn't change"))
  def subVector(regex:String) = getOneWay(t=>t.subVector(regex))
}
