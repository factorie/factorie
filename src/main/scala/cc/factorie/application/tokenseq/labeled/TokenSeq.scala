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

package cc.factorie.application.tokenseq.labeled
import cc.factorie._
import cc.factorie.er._
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

class TokenSeq[T<:Token[This,L,T],L<:Label[This,T,L],This<:TokenSeq[T,L,This]] extends cc.factorie.application.tokenseq.TokenSeq[T,This] {
  this: This =>
  /** Return the collection of Label instances attached to these tokens. */
  def labels = this.map(_.label)
  /** Return the proportion of Labels whose current value is their trueValue. */
  def accuracy: Double = this.foldLeft(0)((sum,token) => if (token.label.valueIsTruth) sum + 1 else sum) / size.toDouble
  /** Return a collection of Seq[Token] each of which are labeled with contiguous non-"background" label values. */
  def entities(background:String): Seq[(L,Seq[T])] = {
    val result = new ArrayBuffer[(L,Seq[T])]
    var label = head.label
    var entity: List[T] = Nil
    for (token <- this) {
      if (token.label.value != background) {
        if (token.label.value == label.value)
          entity = token :: entity
        else {
          if (entity.length > 0) result += ((label,entity.reverse))
          entity = token :: Nil
          label = token.label
        }
      } else {
        if (entity.length > 0) result += ((label,entity.reverse))
        entity = Nil
        label = token.label
      }
    }
    result
  }

}

/** Tools for creating and evaluating LabeledTokenSeq 
    @author Andrew McCallum
    @since 0.8 */
object TokenSeq {
  import scala.io.Source

  /** Construct and return a new LabeledTokenSeq (and its constituent Tokens and Labels) 
      from a source containing SGML markup to indicate the labels on some tokens. 
      Tokens not bounded by SGML will be given a Label with initial and true value 'backgroundLabelString'. 
      Token segmentation will be performed by the extent of regular expression matches to 'lexer'. */
  def fromSGML[S<:TokenSeq[T,L,S],T<:Token[S,L,T],L<:Label[S,T,L]](source:Source, 
                                                                         newTokenSeq:()=>S,
                                                                         newToken:(String,String)=>T, 
                                                                         backgroundLabelString:String = "O", 
                                                                         featureFunction: Seq[String]=>Seq[String], 
                                                                         labelFunction:String=>String = (s:String) => s,                                                                          wordSegmenter:Regex): 
  S = cc.factorie.application.tokenseq.TokenSeq.fromSGML[S,T](source, newTokenSeq, newToken, backgroundLabelString, featureFunction, labelFunction, wordSegmenter)

  /** Construct and return a new LabeledTokenSeq (and its constituent Tokens and Labels) 
      from a source containing plain text.  Since the labels are unknown, all Labels
      will be given the initial and true value 'defaultLabelString'. */
  def fromPlainText[S<:TokenSeq[T,L,S],T<:Token[S,L,T],L<:Label[S,T,L]](source:Source, 
                                                                               newTokenSeq:()=>S,
                                                                               newToken:(String,String)=>T, 
                                                                               defaultLabelString:String = "O", 
                                                                               featureFunction: Seq[String]=>Seq[String], 
                                                                               wordSegmenter:Regex): 
  S = cc.factorie.application.tokenseq.TokenSeq.fromPlainText[S,T](source, newTokenSeq, newToken, defaultLabelString, featureFunction, wordSegmenter)

  /** Create a LabeledTokenSeq from a source of characters that has "one word per line", 
      each line consisting of information about one token: a whitespace-separated list of elements, 
      in which the first element is the word itself and the last element is the true target label for the token.
      The CoNLL 2003 NER Shared Task is an example of such a format.
      Token.word will be set to the first element.
      All elements but the last will be passed to to 'featureFunction', 
      and its returned strings will be added as features to the BinaryFeatureVectorVariable.
      The initial and trueValue of the Label will be set from the last element.
      If ignoreLines is non-null, we skip any lines containing this pattern, for example pass "-DOCSTART-" for CoNLL 2003.
      */
  def fromOWPL[S<:TokenSeq[T,L,S],T<:Token[S,L,T],L<:Label[S,T,L]](source:Source, 
                                                                   newTokenSeq:()=>S,
                                                                   newToken:(String,String)=>T, 
                                                                   featureFunction:Seq[String]=>Seq[String] = cc.factorie.application.tokenseq.standardFeatureFunction,
                                                                   labelFunction:String=>String = (s:String) => s, 
                                                                   sentenceBoundary:Regex = "\\A\\s*\\z".r, 
                                                                   documentBoundary:Regex = "-DOCSTART-".r, 
                                                                   ignoreLines:Regex = null): 
  Seq[S] = cc.factorie.application.tokenseq.TokenSeq.fromOWPL[S,T](source, newTokenSeq, newToken, featureFunction, labelFunction, sentenceBoundary, documentBoundary, ignoreLines)


}
