/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
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
package cc.factorie.app

package object nlp {

  /** Mapping from annotation class (usually stored in an attr) and the DocumentAnnotor from which it can be obtained. */
  type DocumentAnnotatorMap = collection.Map[Class[_], () => DocumentAnnotator]

  /** Given a sequence of strings describing labels in IOB format, such as O I-PER I-LOC B-LOC I-LOC O I-ORG,
      (where I, B prefixes are separated by a dash from the type suffix)
      return a sequence of tuples indicating span start, length and label suffix, such as (3, 2, "LOC"). */
  def iobBoundaries(labels:Seq[String]): Seq[(Int,Int,String)] = {
    val result = new scala.collection.mutable.ArrayBuffer[(Int,Int,String)]
    val strings = labels.map(_.split('-'))
    val iobs = strings.map(_.apply(0))
    val types = strings.map(a => if (a.length > 1) a(1) else "")
    var start = -1; var prevType = ""
    for (i <- 0 until labels.length) {
      val atBoundary = types(i) != prevType || iobs(i) == "B"
      if (start >= 0 && atBoundary) { result.+=((start, i-start, types(i-1))); start = -1 }
      if (types(i) != "" && atBoundary){
        start = i
        if (i == labels.length-1)
          result.+=((start, 1, types(i)))
      }
      prevType = types(i)
    }
    result
  }

  def bilouBoundaries(labels:Seq[String]): Seq[(Int,Int,String)] = {
    val result = new scala.collection.mutable.ArrayBuffer[(Int,Int,String)]
    val strings = labels.map(_.split('-'))
    val bilous = strings.map(_.apply(0))
    val types = strings.map(a => if (a.length > 1) a(1) else "")
    var start = -1; var prevType = ""
    for (i <- 0 until labels.length) {
      val atBoundary = types(i) != prevType || bilous(i) == "B" || bilous(i) == "U"
      if (bilous(i) == "U") result.+=((i, 1, types(i)))
      else if (start >= 0 && atBoundary) { result.+=((start, i-start, types(i-1))); start = -1 }
      if (types(i) != "" && atBoundary){
        start = i
        if (i == labels.length-1)
          result.+=((start, 1, types(i)))
      }
      prevType = types(i)
    }
    result
  }

  /** Convenience alias for @see cc.factorie.app.nlp.iobBoundaries */
  def bioBoundaries(labels:Seq[String]): Seq[(Int,Int,String)] = iobBoundaries(labels)


  /** Command-line options available on all NLP model trainers.
      @author David Belanger */
  trait SharedNLPCmdOptions extends cc.factorie.util.CmdOptions  {
    val targetAccuracy = new CmdOption("target-accuracy", "", "FLOAT", "target accuracy for this NLP model. It will throw an exception if you don't hit this")
    val trainPortion = new CmdOption("train-portion", 1.0, "FLOAT", "portion of train to load")
    val testPortion = new CmdOption("test-portion", 1.0, "FLOAT", "portion of test to load")
  
  }

}
