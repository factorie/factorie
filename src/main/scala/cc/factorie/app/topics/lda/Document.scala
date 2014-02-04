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

package cc.factorie.app.topics.lda
import cc.factorie._
import cc.factorie.directed._
import scala.collection.mutable.ArrayBuffer
import java.io.{File,Reader,StringReader,InputStreamReader,FileInputStream,BufferedReader,PrintWriter}
import cc.factorie.variable._

/** The abstract document variable required by LDA. */
trait Doc extends SeqBreaks {
  var name: String
  var theta: ProportionsVariable
  def thetaArray: Array[Double] = theta.value.toArray
  var zs: DiscreteSeqVariable
  def ws: CategoricalSeqVariable[String]
  def time: Long
  override def toString: String = {
    val sb = new StringBuilder
    sb append name; sb += '\n'
    for (i <- 0 until ws.length) {
      if (breaks.contains(i)) sb += ' ' else sb += '_'
      sb append ws.categoryValue(i)
      //if (zs ne null) { sb += ':'; sb append zs.intValue(i) }
    }
    sb.mkString
  }
    
  // Serialization
  def writeNameWordsZs(p:PrintWriter): Unit = {
    p.println(name)
    for (i <- 0 until ws.length) {
      p.println(ws.categoryValue(i))
      if (breaks.contains(i)) p.print("0") // Mark phrase break points with a (redundant) leading zero on the topic index
      p.println(zs.intValue(i))
    }
    p.println()
    //p.flush()
  }
  def readNameWordsZs(p:BufferedReader): Int = {
    name = p.readLine()
    if (name == null) return -1
    val words = new ArrayBuffer[String]
    val zints = new ArrayBuffer[Int]
    var line: String = p.readLine()
    while (line != null && line.length > 0) {
      words += line
      val topicIndexString = p.readLine()
      if (topicIndexString(0) == '0') breaks += (words.length-1) // Remember phrase break points
      zints += topicIndexString.toInt
      line = p.readLine()
    }
    ws.clear(); ws.appendCategories(words)
    if (zs eq null) throw new Error("Doc.zs must be set non-null and empty before calling this method.") 
    zs.clear(); zs.appendInts(zints)
    ws.length
  }
  /** Read from the BufferedReader, filling the document with words and Z assignments,
      but allow map function to alter the Z assignment, and skips word/z pairs for which the map function returns a value less than 0. */ 
  def readNameWordsMapZs(p:BufferedReader, map:Int=>Int): Int = {
    name = p.readLine()
    if (name == null) return -1
    val words = new ArrayBuffer[String]
    val zints = new ArrayBuffer[Int]
    var line: String = p.readLine()
    while (line != null && line.length > 0) {
      val topicIndexString = p.readLine()
      if (topicIndexString(0) == '0' && words.length > 1) breaks += (words.length-1) // Remember phrase break points
      val topicIndex = topicIndexString.toInt
      val newZ = map(topicIndex)
      if (newZ >= 0) {
        words += line
        zints += newZ
      }
      line = p.readLine()
    }
    ws.clear(); ws.appendCategories(words)
    if (zs eq null) throw new Error("Doc.zs must be set non-null and empty before calling this method.") 
    zs.clear(); zs.appendInts(zints)
    ws.length
  }

}

/** A simple concrete implementation of DocumentVar. */
class Document(val domain:CategoricalSeqDomain[String], var name:String, tokens:Seq[String]) extends CategoricalSeqVariable(tokens) with Doc {
  var theta: ProportionsVariable = null
  var zs: DiscreteSeqVariable = null
  var time: Long = -1
  def ws = this
}

object Document {
  import cc.factorie.app.strings.{StringSegmenter,StringSet,EmptyStringSet,Stopwords,alphaSegmenter}
  import scala.util.control.Breaks._
  def addWords(doc:Document,
      reader:Reader, 
      segmenter:StringSegmenter = alphaSegmenter, 
      stopwords:StringSet = Stopwords, 
      wordCountMax:Int = Int.MaxValue): Unit = 
  {
    //val allWords = new ArrayBuffer[String]
    breakable { for (word <- segmenter(reader)) {
      val w = word.toLowerCase
      //allWords += w
      if (stopwords.contains(w)) doc.breaks += doc.length // break goes at the index of the next word---the word that will begin the next phrase
      else doc.appendCategory(w)
      if (doc.length == wordCountMax) break()
    }}
    doc.trimCapacity
    /*if (allWords.contains("logistic")) {
      println("Document allWords "+allWords.mkString(" "))
      forIndex(doc.length)(i => { if (doc.breaks.contains(i)) print("#"); print(doc.categoryValue(i)+" ") })
      println()  
      //println("Document %s %s".format(doc.name, doc.breaks.toString))
    }*/
  }
//  def wordIterator(reader:Reader, 
//      segmenter:StringSegmenter = alphaSegmenter, 
//      stopwords:Stopwords = Stopwords, 
//      wordCountMax:Int = Int.MaxValue): Iterator[String] = 
//    segmenter(reader).map(_ toLowerCase).filter(!stopwords.contains(_)).take(wordCountMax)
    
  // Convenience methods for creating new documents
  def fromStringIterator(domain:CategoricalSeqDomain[String], name:String, tokens:Iterator[String], stopwords:StringSet = EmptyStringSet) = new Document(domain, name, tokens.filter(stopwords.contains(_)).toIndexedSeq) // TODO create breaks here
  def fromReader(domain:CategoricalSeqDomain[String], name:String, reader:Reader, segmenter:StringSegmenter = alphaSegmenter, stopwords:StringSet = Stopwords, wordCountMax:Int = Int.MaxValue): Document = { val d = new Document(domain, name, Nil); addWords(d, reader, segmenter, stopwords, wordCountMax); d }
  def fromString(domain:CategoricalSeqDomain[String], name:String, contents:String, segmenter:StringSegmenter = alphaSegmenter, stopwords:StringSet = Stopwords, wordCountMax:Int = Int.MaxValue): Document = fromReader(domain, name, new StringReader(contents), segmenter, stopwords, wordCountMax)
  def fromFile(domain:CategoricalSeqDomain[String], file:File, encoding:String = "UTF-8", segmenter:StringSegmenter = alphaSegmenter, stopwords:StringSet = Stopwords, wordCountMax:Int = Int.MaxValue): Document = fromReader(domain, file.getPath, new BufferedReader(new InputStreamReader(new FileInputStream(file), encoding)), segmenter, stopwords, wordCountMax)
}