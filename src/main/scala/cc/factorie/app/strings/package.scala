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

package object strings {

  def inputStreamToString(is:java.io.InputStream, encoding:String = "UTF-8"): String = {
    readerToString(new java.io.InputStreamReader(is, encoding))
  }

  def readerToString(reader:java.io.Reader): String = {
    val buffer = new Array[Char](0x10000)
    val out = new StringBuilder()
    var read = 0
    do {
      read = reader.read(buffer, 0, buffer.length)
      if (read > 0)
        out.appendAll(buffer, 0, read)
    } while (read >= 0)
    out.toString
  }

  /** Return a string that captures the generic "shape" of the original word, 
      mapping lowercase alphabetics to 'a', uppercase to 'A', digits to '1', whitespace to ' '.
      Skip more than 'maxRepetitions' of the same character class. */
  def stringShape(word:String, maxRepetitions:Int): String = {
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
  
  // Simplified form of word for feature generation
  def simplifyDigits(word:String): String = {
    if (word.matches("(19|20)\\d\\d")) "<YEAR>" 
    else if (word.matches("\\d+")) "<NUM>"
    else if (word.matches(".*\\d.*")) word.replaceAll("\\d","#").toLowerCase
    else word
  }

}
