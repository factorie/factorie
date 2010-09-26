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

package cc.factorie.app.strings
import java.text.BreakIterator
import java.io.Reader
import java.io.InputStream

trait StringSegmenter {
  def apply(s:String): Iterator[String]
  def apply(file:java.io.File): Iterator[String] = apply(scala.io.Source.fromFile(file).mkString)
  def apply(is:InputStream): Iterator[String] = apply(inputStreamToString(is))
  def apply(reader:Reader): Iterator[String] = throw new Error("Not yet implemented")
}

class RegexSegmenter(regex:scala.util.matching.Regex) extends StringSegmenter {
  def apply(s:String): Iterator[String] = regex.findAllIn(s)
}

class BreakIteratorSegmenter(bi:BreakIterator) extends StringSegmenter {
  def apply(s:String): Iterator[String] = new Iterator[String] {
    bi.setText(s)
    var start: Int = bi.first
    var end: Int = bi.next()
    def hasNext: Boolean = end != BreakIterator.DONE
    def next: String = {
      val result = s.substring(start, end)
      start = end // TODO Is this right?
      end = bi.next() // TODO Fix this!
      result
    }
  }
}

object sentenceSegmenter extends BreakIteratorSegmenter(BreakIterator.getSentenceInstance(java.util.Locale.US))

object alphaSegmenter extends RegexSegmenter("\\p{Alpha}+".r)
object wordSegmenter extends RegexSegmenter("\\w+".r)
object wordClassesSegmenter extends RegexSegmenter("\\p{Alpha}+|\\p{Digit}+".r)
object nonWhitespaceSegmenter extends RegexSegmenter("\\S+".r)
object nonWhitespaceClassesSegmenter extends RegexSegmenter("\\p{Alpha}+|\\p{Digit}+|\\p{Punct}".r)
