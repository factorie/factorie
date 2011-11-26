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
import cc.factorie.generative._
import scala.collection.mutable.ArrayBuffer
import java.io.{File,Reader,StringReader,InputStreamReader,FileInputStream,BufferedReader,PrintWriter}

/** The abstract document variable required by LDA. */
// TODO Consider changing to "extends Variable", and in LDA using doc.ws ~ ...  DONE
// TODO Rename to something that doesn't have Var in the name
trait Doc /* extends PlatedCategoricalVariable[String]*/ {
  var name: String
  var theta: CountsProportions
  //def theta_=(p:CountsProportions): Unit
  var zs: PlatedGateVariable
  //def zs_=(theZs:PlatedGateVariable): Unit
  def ws: CategoricalSeqVariable[String]
  
  // Experimental thoughts about serialization with Facades
  type WordsZs = { def words: Seq[String]; def zInts: Array[Int] } 
  def toFacade: WordsZs = {
    new {
      val words = ws.categoryValues
      val zInts = zs.intValues
    }
  }
  def setFromFacade(f:WordsZs): Unit = {
    ws.clear()
    zs.clear()
    ws.appendCategories(f.words)
    zs.appendInts(f.zInts)
  }
  
  // Serialization
  def writeNameWordsZs(p:PrintWriter): Unit = {
    p.println(name)
    for (i <- 0 until ws.length) {
      p.println(ws.categoryValue(i))
      p.println(zs.intValue(i))
    }
    p.println()
    p.flush()
  }
  def readNameWordsZs(p:BufferedReader): Int = {
    name = p.readLine()
    if (name == null) return -1
    val words = new ArrayBuffer[String]
    val zints = new ArrayBuffer[Int]
    var line: String = p.readLine()
    while (line != null && line.length > 0) {
      words += line
      zints += p.readLine().toInt
      line = p.readLine()
    }
    ws.clear(); ws.appendCategories(words)
    if (zs eq null) throw new Error("Doc.zs must be set non-null and empty before calling this method.") 
    zs.clear(); zs.appendInts(zints)
    ws.length
  }

}

/** A simple concrete implementation of DocumentVar. */
class Document(val domain:CategoricalSeqDomain[String], var name:String, tokens:Seq[String]) extends PlatedCategoricalVariable(tokens) with Doc {
  var theta: CountsProportions = null
  var zs: PlatedGateVariable = null
  def ws = this
}

object Document {
  import cc.factorie.app.strings.{StringSegmenter,Stopwords,alphaSegmenter}
  def wordIterator(reader:Reader, 
      segmenter:StringSegmenter = alphaSegmenter, 
      stopwords:Stopwords = Stopwords, 
      wordCountMax:Int = Int.MaxValue): Iterator[String] = 
    segmenter(reader).map(_ toLowerCase).filter(!stopwords.contains(_)).take(wordCountMax)
  // Convenience methods for creating new documents
  def apply(domain:CategoricalSeqDomain[String], name:String, tokens:Iterator[String]) = 
    new Document(domain, name, tokens.toIndexedSeq)
  def apply(domain:CategoricalSeqDomain[String], name:String, reader:Reader) = 
    new Document(domain, name, wordIterator(reader).toIndexedSeq)
  def apply(domain:CategoricalSeqDomain[String], name:String, contents:String) = 
    new Document(domain, name, wordIterator(new StringReader(contents)).toIndexedSeq)
  def apply(domain:CategoricalSeqDomain[String], file:File, encoding:String = "UTF-8") = 
    new Document(domain, file.getPath, wordIterator(new BufferedReader(new InputStreamReader(new FileInputStream(file), encoding))).toIndexedSeq)
}