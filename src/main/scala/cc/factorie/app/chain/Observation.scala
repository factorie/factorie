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

package cc.factorie.app.chain
import cc.factorie._
import scala.collection.mutable.ArrayBuffer

trait Observation[+This<:Observation[This]] extends AbstractInChain[This] {
  this: This =>
  def string: String
  //def vector: VectorVar
}

object Observations {
  /** Copy features into each token from its preceding and following tokens, 
   with preceding extent equal to preOffset and following extent equal to -postOffset.
   In other words, to add features from the three preceeding tokens and the two following tokens,
   pass arguments (-3,2).
   Features from preceding tokens will have suffixes like "@-1", "@-2", etc.
   Features from following tokens will have suffixes like "@+1", "@+2", etc. 
   The functionality of this method is completely covered as a special case of addNeighboringFeatureConjunctions,
   but for the simple case, this one is easier to call. */
  def addNeighboringFeatures[A<:Observation[A]](observations:Seq[A], vf:A=>BinaryCategoricalVectorVar[String], preOffset:Int, postOffset:Int): Unit = {
    val size = observations.length
    // First gather all the extra features here, then add them to each Token
    val extraFeatures = Array.tabulate(size)(i => new scala.collection.mutable.ArrayBuffer[String])
    assert(preOffset < 1)
    val preSize = -preOffset; val postSize = postOffset
    for (i <- 0 until size) {
      val token = observations(i)
      val thisTokenExtraFeatures = extraFeatures(i)
      // Do the preWindow features
      var t = token; var j = 0
      while (j < preSize && t.hasPrev) {
        t = t.prev; j += 1; val suffix = "@+"+j
        thisTokenExtraFeatures ++= vf(t).activeCategories.map(str => str+suffix) // t.values is the list of Strings representing the current features of token t
      }
      // Do the postWindow features
      t = token; j = 0
      while (j < postSize && t.hasNext) {
        t = t.next; j += 1; val suffix = "@-"+j
        thisTokenExtraFeatures ++= vf(t).activeCategories.map(str => str+suffix) // t.values is the list of Strings representing the current features of token t
      }
    }
    // Put the new features in the Token
    for (i <- 0 until size) (vf(observations(i))) ++= extraFeatures(i)
  }
  
  def addNeighboringFeatureConjunctions[A<:Observation[A]](observations:Seq[A], vf:A=>BinaryCategoricalVectorVar[String], offsetConjunctions:Seq[Int]*): Unit = 
    addNeighboringFeatureConjunctions(observations, vf, null.asInstanceOf[String], offsetConjunctions:_*)
  /** Add new features created as conjunctions of existing features, with the given offsets, but only add features matching regex pattern. */
  def addNeighboringFeatureConjunctions[A<:Observation[A]](observations:Seq[A], vf:A=>BinaryCategoricalVectorVar[String], regex:String, offsetConjunctions:Seq[Int]*): Unit = {
    val size = observations.size
    // First gather all the extra features here,...
    val newFeatures = Array.tabulate(size)(i => new ArrayBuffer[String])
    for (i <- 0 until size) {
      val token = observations(i)
      val thisTokenNewFeatures = newFeatures(i)
      for (offsets <- offsetConjunctions) 
        thisTokenNewFeatures ++= appendConjunctions(token, vf, regex, null, offsets).
      map(list => list.sortBy({case(f,o)=>o+f}).map({case(f,o) => if (o == 0) f else f+"@"+o}).mkString("_&_")) // TODO "f+o" is doing string concatenation, consider something faster
    }
    // ... then add them to each Token
    for (i <- 0 until size) {
      val token = observations(i)
      // vf(token).zero  // TODO  Removed when transferring code from app.tokenseq.TokenSeq.  Is this still necessary? -akm
      vf(token) ++= newFeatures(i)
    }
    //if (size > 0) println("addNeighboringFeatureConjunctions "+first)
  }
  // Recursive helper function for previous method, expanding out cross-product of conjunctions in tree-like fashion.
  // 't' is the Token to which we are adding features; 'existing' is the list of features already added; 'offsets' is the list of offsets yet to be added
  private def appendConjunctions[A<:Observation[A]](t:A, vf:A=>BinaryCategoricalVectorVar[String], regex:String, existing:ArrayBuffer[List[(String,Int)]], offsets:Seq[Int]): ArrayBuffer[List[(String,Int)]] = {
    val result = new ArrayBuffer[List[(String,Int)]];
    val offset: Int = offsets.head
    val t2 = t.next(offset)
    val adding: Seq[String] = 
      if (t2 == null) { if (/*t.position +*/ offset < 0) List("<START>") else List("<END>") }
      else if (regex != null) vf(t2).activeCategories.filter(str => str.matches(regex)) // Only include features that match pattern 
      else vf(t2).activeCategories
    if (existing != null) {
      for (e <- existing; a <- adding) { val elt = (a,offset); if (!e.contains(elt)) result += (a,offset) :: e }
    } else {
      for (a <- adding) result += List((a,offset))
    }
    if (offsets.size == 1) result
    else appendConjunctions(t, vf, regex, result, offsets.drop(1))
  }  
  
}