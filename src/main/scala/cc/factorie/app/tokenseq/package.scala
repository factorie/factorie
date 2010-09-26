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

package cc.factorie.app
import cc.factorie._
import scala.util.matching.Regex

package object tokenseq {
  /** Be default we make words from contiguous sequences of one of the three following character classes: alphabetic, digits, punctuation. */
  val defaultLexer = nonWhitespaceClasses
  val alphaLexer = new Regex("\\p{Alpha}+")
  val wordLexer = new Regex("\\w+")
  val nonWhitespaceLexer = new Regex("\\S+")
  val wordClassesLexer = new Regex("\\p{Alpha}+|\\p{Digit}+")
  val nonWhitespaceClasses = new Regex("\\p{Alpha}+|\\p{Digit}+|\\p{Punct}")

  def defaultWordSegmenter = defaultLexer

  // Feature extraction aids
  /** Return a string that captures the generic "shape" of the original word, 
      mapping lowercase alphabetics to 'a', uppercase to 'A', digits to '1', whitespace to ' '.
      Skip more than 'maxRepetitions' of the same character class. */
  def wordShape(word:String, maxRepetitions:Int): String = {
    val sb = new StringBuffer
    var i = 0; var c = 'x'; var prevc = 'x'; var repetitions = 0
    while (i < word.length) {
      val char = word(i); 
      if (Character.isUpperCase(char)) c = 'A'
      else if (Character.isLowerCase(char)) c = 'a'
      else if (Character.isDigit(char)) c = '1'
      else if (Character.isWhitespace(char)) c = ' '
      else c = char
      if (c == prevc) repetitions += 1
      else { prevc = c; repetitions = 0 }
      if (repetitions < maxRepetitions) sb.append(c)
      i += 1
    }
    sb.toString
  }
  def charNGrams(word:String, min:Int, max:Int): Seq[String] = {
    val w = "<"+word+">"
    val prefixes = for (e <- min+1 to math.min(max+1, word.length)) yield w.substring(0, e)
    val suffices = for (b <- math.max(w.length-1-max, 0) to w.length-1-min) yield w.substring(b, w.length)
    prefixes ++ suffices
    //for (i <- 0 until w.length; j <- min to max; if (i+j < w.length)) yield w.substring(i,i+j)
  }


  /** Given some String features read from raw data, in which the first feature is always the word String itself, add some more features.  */
  def standardFeatureFunction(inFeatures:Seq[String]): Seq[String] = {
    val result = new scala.collection.mutable.ArrayBuffer[String]
    // Assume the first feature is the word
    result += "W="+inFeatures(0)
    result += "SHAPE="+wordShape(inFeatures(0), 2)
    result ++= charNGrams(inFeatures(0), 2, 5)
    result ++= inFeatures.drop(1)
    result
  }

}
