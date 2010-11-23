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
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

class TokenSeq[T<:Token[This,L,T],L<:Label[This,T,L],This<:TokenSeq[T,L,This]]
extends cc.factorie.app.tokenseq.TokenSeq[T,This]
with ContiguousEncoding[T,L,This]
{
  this: This =>
  /** Return the collection of Label instances attached to these tokens. */
  def labels = this.map(_.label)
  /** Return the proportion of Labels whose current value is their trueValue. */
  def accuracy: Double = this.foldLeft(0)((sum,token) => if (token.label.valueIsTruth) sum + 1 else sum) / size.toDouble
}

trait Encoding[T<:Token[This,L,T],L<:Label[This,T,L],This<:TokenSeq[T,L,This]] {
  this: This =>
  def entities: Seq[(String,Seq[T])]
  def trueEntities: Seq[(String,Seq[T])]
  /* = {
    // TODO: there is better ways to do this which doesn't require undos
    val difflist = new DiffList
    labels.foreach(_ setToTruth difflist)
    val e = entities
    difflist.undo
    e
  } */
}

trait BIOEncoding[T<:Token[This,L,T],L<:Label[This,T,L],This<:TokenSeq[T,L,This]]
extends Encoding[T,L,This] {
  this: This =>
  override def entities: Seq[(String,Seq[T])] = TokenSeq.extractBIO[T](this, (_:T).label.value)
  override def trueEntities: Seq[(String,Seq[T])] = TokenSeq.extractBIO[T](this, (_:T).label.trueValue)
}

trait ContiguousEncoding[T<:Token[This,L,T],L<:Label[This,T,L],This<:TokenSeq[T,L,This]]
extends Encoding[T,L,This] {
  this: This =>
  override def entities: Seq[(String,Seq[T])] = TokenSeq.extractContiguous[T](this, (_:T).label.value)
  override def trueEntities: Seq[(String,Seq[T])] = TokenSeq.extractContiguous[T](this, (_:T).label.trueValue)
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
                                                                   labelFunction:String=>String = (s:String) => s,
                                                                   wordSegmenter:Regex):
  S = cc.factorie.app.tokenseq.TokenSeq.fromSGML[S,T](source, newTokenSeq, newToken, backgroundLabelString, featureFunction, labelFunction, wordSegmenter)

  /** Construct and return a new LabeledTokenSeq (and its constituent Tokens and Labels)
      from a source containing plain text.  Since the labels are unknown, all Labels
      will be given the initial and true value 'defaultLabelString'. */
  def fromPlainText[S<:TokenSeq[T,_,S],T<:Token[S,_,T]](source:Source,
                                                        newTokenSeq:()=>S,
                                                        newToken:(String,String)=>T,
                                                        defaultLabelString:String = "O",
                                                        featureFunction: Seq[String]=>Seq[String],
                                                        wordSegmenter:Regex):
  S = cc.factorie.app.tokenseq.TokenSeq.fromPlainText[S,T](source, newTokenSeq, newToken, defaultLabelString, featureFunction, wordSegmenter)

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
  def fromOWPL[S<:TokenSeq[T,_,S],T<:Token[S,_,T]](source:Source,
                                                   newTokenSeq:()=>S,
                                                   newToken:(String,String)=>T,
                                                   featureFunction:Seq[String]=>Seq[String] = cc.factorie.app.tokenseq.standardFeatureFunction,
                                                   labelFunction:String=>String = (s:String) => s,
                                                   sentenceBoundary:Regex = "\\A\\s*\\z".r,
                                                   documentBoundary:Regex = "-DOCSTART-".r,
                                                   ignoreLines:Regex = null):
  Seq[S] = cc.factorie.app.tokenseq.TokenSeq.fromOWPL[S,T](source, newTokenSeq, newToken, featureFunction, labelFunction, sentenceBoundary, documentBoundary, ignoreLines)


  /**
   * Extract a collection contiguous non-"background" labels
   *
   * Authors: Tim Vieira, Andrew McCallum
   */
  def extractContiguous[T](s:Seq[T], labeler:T=>String, background:String = "O"): Seq[(String,Seq[T])] = {
    val result = new ArrayBuffer[(String,Seq[T])]
    if (s.size == 0) return result
    var prevLabel = background
    var entity = new ArrayBuffer[T]
    for (token <- s) {
      val currLabel = labeler(token)
      if (currLabel != background) {
        if (currLabel == prevLabel) {
          entity += token
        } else {
          if (entity.length > 0) result += ((prevLabel, entity.reverse))
          entity = new ArrayBuffer
          entity += token 
        }
      } else {
        if (entity.length > 0) result += ((prevLabel, entity.reverse))
        entity = new ArrayBuffer[T]
      }
      prevLabel = currLabel
    }
    // add any lingering bits
    if (entity.length > 0) result += ((prevLabel, entity.reverse))
    result
  }

  /**
   * Given a sequence and a labeling function extract segments encoded in the BIO or IOB scheme.
   * Note: a hueristic correction is applied when a segment starts with "I-"
   *
   * Author: Tim Vieira
   * Since Oct. 3rd, 2010
   */
  def extractBIO[T](s:Seq[T], labeler:T=>String): Seq[(String,Seq[T])] = {
    val result = new ArrayBuffer[(String,Seq[T])]
    var phrase = new ArrayBuffer[T]
    var intag: String = null
    for (tk <- s) {
      val lbl = labeler(tk)
      if (lbl startsWith "B-") {
        if (intag != null && phrase.length > 0) {
          result += ((intag, phrase))
          phrase = new ArrayBuffer[T]
        }
        intag = lbl.substring(2)
        phrase += tk.asInstanceOf[T]
      } else if (lbl startsWith "I-") {
        if (intag == lbl.substring(2)) {  // and still in the same span
          phrase += tk
        } else {                            // you're in a new span (hueristic correction)
          if (phrase.length > 0) result += ((intag, phrase))
          intag = lbl.substring(2)
          phrase = ArrayBuffer[T](tk)
        }
      } else if (intag != null) {          // was in tag, now outiside
        result += ((intag, phrase))
        intag = null
        phrase = new ArrayBuffer[T]
      } else {
        // label is not B-* I-*, must be "O", AND not intag
      }
    }
    if (intag != null && phrase.length > 0) result += ((intag, phrase))  // close any lingering spans
    result
  }

}
