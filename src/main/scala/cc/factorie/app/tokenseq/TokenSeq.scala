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
import scala.util.matching.Regex

/** A sequence of Tokens */
class TokenSeq[T<:Token[This,T],This<:TokenSeq[T,This]] extends VariableSeq[T] {
  this: This =>
  /** Add new features created as conjunctions of existing features, with the given offsets.
      For example addNeighboringFeatures(List(0,0),List(-2,-1,0),List(0,1)) */
  def addNeighboringFeatureConjunctions(offsetConjunctions:Seq[Int]*): Unit = 
    addNeighboringFeatureConjunctions(null.asInstanceOf[String], offsetConjunctions:_*)
  /** Add new features created as conjunctions of existing features, with the given offsets, but only add features matching regex pattern. */
  def addNeighboringFeatureConjunctions(regex:String, offsetConjunctions:Seq[Int]*): Unit = {
    // First gather all the extra features here,...
    val newFeatures = Array.tabulate(this.size)(i => new ArrayBuffer[String])
    for (i <- 0 until size) {
      val token = this(i)
      val thisTokenNewFeatures = newFeatures(i)
      for (offsets <- offsetConjunctions) 
        thisTokenNewFeatures ++= appendConjunctions(regex, token, null, offsets).
      map(list => list.sortBy({case(f,o)=>o+f}).map({case(f,o) => if (o == 0) f else f+"@"+o}).mkString("_&_")) // TODO "f+o" is doing string concatenation, consider something faster
    }
    // ... then add them to each Token
    for (i <- 0 until size) {
      val token = this(i)
      token.zero
      token ++= newFeatures(i)
    }
    //if (size > 0) println("addNeighboringFeatureConjunctions "+first)
  }
  // Recursive helper function for previous method, expanding out cross-product of conjunctions in tree-like fashion.
  // 't' is the Token to which we are adding features; 'existing' is the list of features already added; 'offsets' is the list of offsets yet to be added
  private def appendConjunctions(regex:String, t:T, existing:ArrayBuffer[List[(String,Int)]], offsets:Seq[Int]): ArrayBuffer[List[(String,Int)]] = {
    val result = new ArrayBuffer[List[(String,Int)]];
    val offset: Int = offsets.head
    val t2 = t.next(offset)
    val adding: Seq[String] = 
      if (t2 == null) { if (t.position + offset < 0) List("<START>") else List("<END>") }
      else if (regex != null) t2.values.filter(str => str.matches(regex)) // Only include features that match pattern 
      else t2.values
    if (existing != null) {
      for (e <- existing; a <- adding) { val elt = (a,offset); if (!e.contains(elt)) result += (a,offset) :: e }
    } else {
      for (a <- adding) result += List((a,offset))
    }
    if (offsets.size == 1) result
    else appendConjunctions(regex, t, result, offsets.drop(1))
  }
  /** Copy features into each token from its preceding and following tokens, 
   with preceding extent equal to preOffset and following extent equal to -postOffset.
   In other words, to add features from the three preceeding tokens and the two following tokens,
   pass arguments (-3,2).
   Features from preceding tokens will have suffixes like "@-1", "@-2", etc.
   Features from following tokens will have suffixes like "@+1", "@+2", etc. 
   The functionality of this method is completely covered as a special case of addNeighboringFeatureConjunctions,
   but for the simple case, this one is easier to call. */
  def addNeighboringFeatures(preOffset:Int, postOffset:Int): Unit = {
    // First gather all the extra features here, then add them to each Token
    val extraFeatures = Array.tabulate(this.size)(i => new ArrayBuffer[String])
    assert(preOffset < 1)
    val preSize = -preOffset; val postSize = postOffset
    for (i <- 0 until size) {
      val token = this(i)
      val thisTokenExtraFeatures = extraFeatures(i)
      // Do the preWindow features
      var t = token; var j = 0
      while (j < preSize && t.hasPrev) {
        t = t.prev; j += 1; val suffix = "@+"+j
        thisTokenExtraFeatures ++= t.values.map(str => str+suffix) // t.values is the list of Strings representing the current features of token t
      }
      // Do the postWindow features
      t = token; j = 0
      while (j < postSize && t.hasNext) {
        t = t.next; j += 1; val suffix = "@-"+j
        thisTokenExtraFeatures ++= t.values.map(str => str+suffix) // t.values is the list of Strings representing the current features of token t
      }
    }
    // Put the new features in the Token
    for (i <- 0 until size) (this(i)) ++= extraFeatures(i)
  }
  
  def print(out:java.io.OutputStream): Unit = {
    throw new Error("Not yet implemented")
  }
}



/** Tools for creating a TokenSeq 
    @author Andrew McCallum
    @since 0.8 */
object TokenSeq {
  import scala.io.Source

  /** Construct and return a new TokenSeq (and its constituent Tokens and Labels) 
   from a source containing SGML markup to indicate the labels on some tokens. 
   Tokens not bounded by SGML will be given a Label with initial and true value 'backgroundLabelString'. 
   Token segmentation will be performed by the extent of regular expression matches to 'lexer'. */
  def fromSGML[S<:TokenSeq[T,S],T<:Token[S,T]](source:Source, 
                                               newTokenSeq:()=>S,
                                               newToken:(String,String)=>T, 
                                               backgroundLabelString:String = "O", 
                                               featureFunction: Seq[String]=>Seq[String] = standardFeatureFunction, 
                                               labelFunction:String=>String = (s:String) => s, 
                                               wordSegmenter:Regex = defaultWordSegmenter):
  S = {
    val words = wordSegmenter.findAllIn(source.mkString)
    throw new Error("Not implemented yet.")
  }

  /** Construct and return a new TokenSeq (and its constituent Tokens and Labels) 
   from a source containing plain text.  Since the labels are unknown, all Labels
   will be given the initial and true value 'defaultLabelString'. */
  def fromPlainText[S<:TokenSeq[T,S],T<:Token[S,T]](source:Source, 
                                                    newTokenSeq:()=>S,
                                                    newToken:(String,String)=>T, 
                                                    defaultLabelString:String = "O", 
                                                    featureFunction: Seq[String]=>Seq[String] = standardFeatureFunction,
                                                    wordSegmenter:Regex = defaultWordSegmenter): 
  S = {
    val seq = newTokenSeq()
    wordSegmenter.findAllIn(source.mkString).foreach(word => {
      if (word.length > 0) {
        val token = newToken(word, defaultLabelString)
        token ++= featureFunction(List(word))
        seq += token
      }
    })
    seq
  }

  /** Create a TokenSeq from a source of characters that has "one word per line", (OWPL, pronounced "opel"),
      each line consisting of information about one token: a whitespace-separated list of elements, 
      in which the first element is the word itself and the last element is the true target label for the token.
      The CoNLL 2003 NER Shared Task is an example of such a format.
      Token.word will be set to the first element.
      All elements but the last will be passed to to 'featureFunction', 
      and its returned strings will be added as features to the BinaryFeatureVectorVariable.
      The initial and trueValue of the Label will be set from the last element.
      If ignoreLines is non-null, we skip any lines containing this pattern, for example pass "-DOCSTART-" for CoNLL 2003.
   */
  def fromOWPL[S<:TokenSeq[T,S],T<:Token[S,T]](source:Source, 
                                               newTokenSeq:()=>S, 
                                               newToken:(String,String)=>T, 
                                               featureFunction:Seq[String]=>Seq[String] = standardFeatureFunction, 
                                               labelFunction:String=>String = (s:String) => s, 
                                               sentenceBoundary:Regex = "\\A\\s*\\z".r, 
                                               documentBoundary:Regex = "-DOCSTART-".r, 
                                               ignoreLines:Regex = null): 
  Seq[S] = {
    import scala.collection.mutable.ArrayBuffer
    var tokenCount = 0
    var seqs = new ArrayBuffer[S];
    var seq = newTokenSeq()
    for (line <- source.getLines()) {
      if (sentenceBoundary != null && sentenceBoundary.findAllIn(line).hasNext && seq.length > 0) {
        //println("Completed sentence size=" + seq.size + " num sentences="+seqs.size)   
        seqs += seq
        seq = newTokenSeq()
      } else if (documentBoundary != null && documentBoundary.findAllIn(line).hasNext) {
        //println("Completed document with boundary "+documentBoundary)
        if (seq.length > 0) { seqs += seq; seq = newTokenSeq() }
        seqs += newTokenSeq() // Insert an empty sentence to mark document boundary
      } else if (line.length < 2 || (ignoreLines != null && ignoreLines.findAllIn(line).hasNext)) {
        // Skip this line
      } else {
        val fields = line.split("\\s+")
        assert(fields.length >= 2, "line \"%s\" must have a word and a label." format line)
        val word = fields(0)
        val inFeatures = fields.slice(0, fields.length-1)   // This used to be with ".force"
        val label = labelFunction(fields.last.stripLineEnd)
        val token = newToken(word, label)
        token ++= featureFunction(inFeatures)
        seq += token
        tokenCount += 1
      }
    }
    // gdruck: This is necessary because a file might not end with a sentence boundary.
    if (seq.length > 0) seqs += seq
    //println("Loaded "+seqs.length+" sentences with "+wordCount+" words total from file "+filename)
    seqs
  }


}  
