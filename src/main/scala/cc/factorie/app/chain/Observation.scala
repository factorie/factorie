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

package cc.factorie.app.chain
import cc.factorie.util.Attr
import cc.factorie.variable.{AbstractChainLink, CategoricalVectorVar}

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

/** A element of the input sequence to a linear-chain (CRF) having a string.  Its corresponding label will be an Attr attribute. */
trait Observation[+This<:Observation[This]] extends AbstractChainLink[This] with Attr {
  this: This =>
  def string: String
  //def vector: VectorVar
}

object Observations {
  /** Copy features into each token from its preceding and following tokens, 
   with preceding extent equal to preOffset and following extent equal to postOffset.
   In other words, to add features from the three preceding tokens and the two following tokens,
   pass arguments (-3,2).
   Features from preceding tokens will have suffixes like "@-1", "@-2", etc.
   Features from following tokens will have suffixes like "@+1", "@+2", etc. 
   The functionality of this method is completely covered as a special case of addNeighboringFeatureConjunctions,
   but for the simple case, this one is easier to call and much faster. */
  def addNeighboringFeatures[A<:Observation[A]](observations:IndexedSeq[A], vf:A=>CategoricalVectorVar[String], regex : Regex, preOffset:Int, postOffset:Int): Unit = {
    val size = observations.length
    // First gather all the extra features here, then add them to each Token
    val extraFeatures = Array.tabulate(size)(i => new scala.collection.mutable.ArrayBuffer[String])
    assert(preOffset < 1)
    val seqStart = observations.head.position
    val seqEnd = seqStart + size
    val preSize = -preOffset; val postSize = postOffset
    val prevOffsetStr = Array.tabulate(preSize+1)(i => "@-"+(i))
    val nextOffsetStr = Array.tabulate(postSize+1)(i => "@"+(i))
    var i = 0
    while (i < size) {
      val token = observations(i)
      val thisTokenExtraFeatures = extraFeatures(i)
      // Do the preWindow features
      var j = 1
      while (j <= preSize) {
        val t = token.prev(j)
        val suffix = prevOffsetStr(j)
        if ((t == null) || (t.position < seqStart)) {
          thisTokenExtraFeatures += "<START>" + suffix
        } else {
          thisTokenExtraFeatures ++= vf(t).activeCategories.filter(str => regex.findFirstIn(str).nonEmpty).map(str => str+suffix) // Only include features that match pattern
        }
        j += 1
      }
      // Do the postWindow features
      j = 1
      while (j <= postSize) {
        val t = token.next(j)
        val suffix = nextOffsetStr(j)
        if ((t == null) || (t.position >= seqEnd)) {
          thisTokenExtraFeatures += "<END>" + suffix
        } else {
          thisTokenExtraFeatures ++= vf(t).activeCategories.filter(str => regex.findFirstIn(str).nonEmpty).map(str => str+suffix) // Only include features that match pattern
        }
        j += 1
      }
      i += 1
    }
    // Put the new features in the Token
    i = 0
    while (i < size) {
      vf(observations(i)) ++= extraFeatures(i)
      i += 1
    }
  }

  /** Return null if index i is out of range. */
  private def safeApply[A](s:Seq[A], i:Int): A = if (i < 0 || i > s.length-1) null.asInstanceOf[A] else s(i)

  /** Add feature to each Observation. */
  def addFeatures[A<:Observation[A]](observations:Seq[A], vf:A=>CategoricalVectorVar[String], f1:A=>String, i1:Int): Unit = {
    for (i <- 0 until observations.length) {
      val o = observations(i)
      val o1 = safeApply(observations, i+i1); val s1 = if (o1 ne null) f1(o1) else null
      if (s1 != null) vf(o) += (s1 + "@" + i1)
    }
  }

  /** Add binary conjunction feature to each Observation. */
  def addFeatures[A<:Observation[A]](observations:Seq[A], vf:A=>CategoricalVectorVar[String], f1:A=>String, i1:Int, f2:A=>String, i2:Int): Unit = {
    for (i <- 0 until observations.length) {
      val o = observations(i)
      val o1 = safeApply(observations, i+i1); val s1 = if (o1 ne null) f1(o1) else null
      val o2 = safeApply(observations, i+i2); val s2 = if (o2 ne null) f2(o2) else null
      if (s1 != null && s2 != null) vf(o) += (s1 + "@" + i1 + "_&_" + s2 + "@" + i2)
    }
  }

  /** Add trinary conjunction feature to each Observation. */
  def addFeatures[A<:Observation[A]](observations:Seq[A], vf:A=>CategoricalVectorVar[String], f1:A=>String, i1:Int, f2:A=>String, i2:Int, f3:A=>String, i3:Int): Unit = {
    for (i <- 0 until observations.length) {
      val o = observations(i)
      val o1 = safeApply(observations, i+i1); val s1 = if (o1 ne null) f1(o1) else null
      val o2 = safeApply(observations, i+i2); val s2 = if (o2 ne null) f2(o2) else null
      val o3 = safeApply(observations, i+i3); val s3 = if (o3 ne null) f3(o3) else null
      if (s1 != null && s2 != null && s3 != null) vf(o) += (s1 + "@" + i1 + "_&_" + s2 + "@" + i2 + "_&_" + s3 + "@" + i3)
    }
  }


  // TODO Note that the methods below do not obey Sentence boundaries:  they will add offset conjunctions across the sentence boundary, 
  // even if "observations" contains only the words in the a Sentence.
  // Change the implementation to avoid Observation.next, and instead only use observations.apply(Int).

  def addNeighboringFeatureConjunctions[A<:Observation[A]](observations:IndexedSeq[A], vf:A=>CategoricalVectorVar[String], offsetConjunctions:Seq[Int]*): Unit =
    addNeighboringFeatureConjunctions(observations, vf, null.asInstanceOf[Regex], offsetConjunctions:_*)
  def addNeighboringFeatureConjunctions[A<:Observation[A]](observations:IndexedSeq[A], vf:A=>CategoricalVectorVar[String], regex:String, offsetConjunctions:Seq[Int]*): Unit = {
    val pattern = if (regex != null ) regex.r else null
    addNeighboringFeatureConjunctions[A](observations, vf, pattern, offsetConjunctions:_*)
  }
    /** Add new features created as conjunctions of existing features, with the given offsets, but only add features matching regex pattern. */
  def addNeighboringFeatureConjunctions[A<:Observation[A]](observations:IndexedSeq[A], vf:A=>CategoricalVectorVar[String], regex:Regex, offsetConjunctions:Seq[Int]*): Unit = {
    val size = observations.size
    if (size == 0) return
    val seqStart = observations.head.position
    val seqEnd = seqStart + size
    // First gather all the extra features here,...
    val newFeatures = Array.tabulate(size)(i => new ArrayBuffer[String])
    var i = 0
    while (i < size) {
      val token = observations(i)
      val thisTokenNewFeatures = newFeatures(i)
      for (offsets <- offsetConjunctions)
        thisTokenNewFeatures ++=
          appendConjunctions(token, seqStart, seqEnd, vf, regex, null, offsets)
            .map { list =>
            list.sortBy { case (f, o) => o + f }.map { case (f, o) => if (o == 0) f else f + "@" + o }.mkString("_&_")
          }
      // TODO "f+o" is doing string concatenation, consider something faster
      i += 1
    }
    // ... then add them to each Token
    i = 0
    while (i < size) {
      val token = observations(i)
      //vf(token).zero()  // TODO  Removed when transferring code from app.tokenseq.TokenSeq.  Is this still necessary? -akm
      vf(token) ++= newFeatures(i)
      i += 1
    }
    //if (size > 0) println("addNeighboringFeatureConjunctions "+first)
  }
  // Recursive helper function for previous method, expanding out cross-product of conjunctions in tree-like fashion.
  // 't' is the Token to which we are adding features; 'existing' is the list of features already added; 'offsets' is the list of offsets yet to be added
  private def appendConjunctions[A<:Observation[A]](t:A, seqStart:Int, seqEnd:Int, vf:A=>CategoricalVectorVar[String], regex:Regex, existing:ArrayBuffer[List[(String,Int)]], offsets:Seq[Int]): ArrayBuffer[List[(String,Int)]] = {
    val result = new ArrayBuffer[List[(String,Int)]]
    val offset: Int = offsets.head
    var t2 = t.next(offset); if ((t2 ne null) && (t2.position < seqStart || t2.position >= seqEnd)) t2 = null.asInstanceOf[A]  // Don't add features beyond the boundaries of the Seq given in addNeighboringFeatureConjunctions
    val adding: Seq[String] =
      if (t2 eq null) { if (/*t.position +*/ offset < 0) List("<START>") else List("<END>") }
      else if (regex != null) vf(t2).activeCategories.filter(str => regex.findFirstIn(str).nonEmpty) // Only include features that match pattern
      else vf(t2).activeCategories
    if (existing != null) {
      for (e <- existing; a <- adding) { val elt = (a,offset); if (!e.contains(elt)) result += (a,offset) :: e }
    } else {
      for (a <- adding) result += List((a,offset))
    }
    if (offsets.size == 1) result
    else appendConjunctions(t, seqStart, seqEnd, vf, regex, result, offsets.drop(1))
  }

  /** Extract a collection contiguous non-"background" labels
      @author Tim Vieira, Andrew McCallum */
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
          if (entity.length > 0) result += ((prevLabel, entity))
          entity = new ArrayBuffer
          entity += token
        }
      } else {
        if (entity.length > 0) result += ((prevLabel, entity))
        entity = new ArrayBuffer[T]
      }
      prevLabel = currLabel
    }
    // add any lingering bits
    if (entity.length > 0) result += ((prevLabel, entity))
    result
  }

  /** Given a sequence and a labeling function extract segments encoded in the BIO or IOB scheme.
      Note: a hueristic correction is applied when a segment starts with "I-"
      @author Tim Vieira */
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