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
import java.io.{File,Reader,StringReader,InputStreamReader,FileInputStream,BufferedReader,PrintWriter}

trait DocumentVar extends PlatedCategoricalVariable[String] {
  def name: String
  def theta: CountsProportions
  def theta_=(p:CountsProportions): Unit
  def zs: PlatedGateVariable
  def ws: PlatedCategoricalVariable[String]
}

class Document(val domain:CategoricalSeqDomain[String], val name:String, tokens:Seq[String]) extends PlatedCategoricalVariable(tokens) with DocumentVar {
  var theta: CountsProportions = null
  def zs = null //parentFactor.asInstanceOf[PlatedDiscreteMixture.Factor]._3
  def ws = this
  // Getting tokens from character strings
  def stopwords = cc.factorie.app.strings.Stopwords
  def wordRegex = "[A-Za-z]+".r
  def wordSegmenter = new cc.factorie.app.strings.RegexSegmenter(wordRegex)
  def wordCountMax = Int.MaxValue
  def wordIterator(reader:Reader): Iterator[String] = wordSegmenter(reader).map(_ toLowerCase).filter(!stopwords.contains(_)).take(wordCountMax)
}

object Document {
  import cc.factorie.app.strings.{StringSegmenter,Stopwords,alphaSegmenter}
  def wordIterator(reader:Reader, segmenter:StringSegmenter = alphaSegmenter, stopwords:Stopwords = Stopwords, wordCountMax:Int = Int.MaxValue): Iterator[String] = 
    segmenter(reader).map(_ toLowerCase).filter(!stopwords.contains(_)).take(wordCountMax)
  // Convenience methods for creating new documents
  def apply(domain:CategoricalSeqDomain[String], name:String, tokens:Iterator[String]) = new Document(domain, name, tokens.toIndexedSeq)
  def apply(domain:CategoricalSeqDomain[String], name:String, reader:Reader) = new Document(domain, name, wordIterator(reader).toIndexedSeq)
  def apply(domain:CategoricalSeqDomain[String], name:String, contents:String) = new Document(domain, name, wordIterator(new StringReader(contents)).toIndexedSeq)
  def apply(domain:CategoricalSeqDomain[String], file:File, encoding:String = "UTF-8") = new Document(domain, file.getPath, wordIterator(new BufferedReader(new InputStreamReader(new FileInputStream(file), encoding))).toIndexedSeq)
}