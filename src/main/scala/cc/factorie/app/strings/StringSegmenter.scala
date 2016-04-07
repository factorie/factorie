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

package cc.factorie.app.strings
import java.io.{InputStream, Reader}
import java.text.BreakIterator

trait StringSegmentIterator extends Iterator[String] {
  def start: Int
  def end: Int
}

trait StringSegmenter {
  def apply(s:String): StringSegmentIterator
  def apply(file:java.io.File): StringSegmentIterator = apply(scala.io.Source.fromFile(file, "ISO-8859-1").mkString)
  def apply(is:InputStream): StringSegmentIterator = apply(cc.factorie.app.strings.inputStreamToString(is))
  def apply(reader:Reader): StringSegmentIterator = apply(cc.factorie.app.strings.readerToString(reader))
}

class RegexSegmenter(val regex:scala.util.matching.Regex) extends StringSegmenter with Serializable{
  def apply(s:String): StringSegmentIterator = new StringSegmentIterator {
    val matchIterator: scala.util.matching.Regex.MatchIterator = regex.findAllIn(s)
    def hasNext = matchIterator.hasNext
    def next() = matchIterator.next()
    def start = matchIterator.start
    def end = matchIterator.end
  }
}

class BreakIteratorSegmenter(val bi:BreakIterator) extends StringSegmenter {
  def apply(s:String): StringSegmentIterator = new StringSegmentIterator {
    bi.setText(s)
    var start: Int = -1
    var end: Int = -1
    var nextStart: Int = bi.first()
    var nextEnd: Int = bi.next()
    def hasNext: Boolean = nextEnd != BreakIterator.DONE
    def next(): String = {
      start = nextStart
      end = nextEnd
      val result = s.substring(start, end)
      nextStart = end // TODO Is this right?
      nextEnd = bi.next()
      result
    }
  }
}

// This seems totally broken.  I tried:
// scala -cp ~/workspace/factorie/target cc.factorie.app.strings.sentenceSegmenter ~/research/data/text/nipstxt/nips11/0620.txt
// and got garbage
//object sentenceSegmenter extends BreakIteratorSegmenter(BreakIterator.getSentenceInstance(java.util.Locale.US)) {
//  def main(args:Array[String]): Unit = {
//    for (filename <- args) {
//      println("\n"+filename)
//      val iterator = apply(new java.io.File(filename))
//      for (sentence <- iterator) println(sentence)
//    }
//  }
//}

object alphaSegmenter extends RegexSegmenter("\\p{Alpha}+".r)
object wordSegmenter extends RegexSegmenter("\\w+".r)
object wordClassesSegmenter extends RegexSegmenter("\\p{Alpha}+|\\p{Digit}+".r)
object nonWhitespaceSegmenter extends RegexSegmenter("\\S+".r) 
object nonWhitespaceClassesSegmenter extends RegexSegmenter("\\p{Alpha}+|\\p{Digit}+|\\p{Punct}".r)
object foreignWordSegmenter extends RegexSegmenter("[\\p{L}\\p{P}]*\\p{L}".r)
object urlSegmenter extends RegexSegmenter("\\b(https?|ftp|file)://[-A-Z0-9+&@#/%?=~_|!:,.;]*[-A-Z0-9+&@#/%=~_|]".r)

/** For segmenting fields of a comma-separated-value file.
    Handles commas nested in quotes, 
    but note that the outer quotes will be part of the returned segments. */
object csvSegmenter extends RegexSegmenter("(?:\"([^\"]*)\")|(?:(?<=,|^)([^,]*)(?=,|$))".r)

// Attempts to get rid of the quotes, which didn't work for me:
//object csvSegmenter extends RegexSegmenter("\"([^\"]+?)\",?|([^,]+),?|,".r)
//object csvSegmenter extends RegexSegmenter("(?<=\")([^\"]*)(?=\")|(?<=,|^)([^,]*)(?=,|$)".r)
//object csvSegmenter extends RegexSegmenter("(?:(?<=\")([^\"]*)(?=\"))|(?<=,|^)([^,]*)(?=,|$)".r)

//object csvSegmenter extends RegexSegmenter("\"([^\"]+?)\",?|([^,]+),?|,".r)
//object csvSegmenter extends RegexSegmenter("(?:\"([^\"]*)\")|(?:(?<=,|^)([^,]*)(?=,|$))".r)
//object csvSegmenter extends RegexSegmenter("(?:(?<=\")([^\"]*)(?=\"))|(?<=,|^)([^,]*)(?=,|$)".r)
