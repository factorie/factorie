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

package cc.factorie.app.nlp
import java.io.File

object LoadPlainText {
  def fromFile(file:File, segmentSentences:Boolean): Document = {
    val string = scala.io.Source.fromFile(file).mkString
    fromString(file.getCanonicalPath, string, segmentSentences)
  }

  def fromString(name:String, contents:String, segmentSentences:Boolean): Document = {
    val document = new Document(name, contents)
    if (segmentSentences) {
      val sentenceIterator = cc.factorie.app.strings.sentenceSegmenter(document.string)
      while (sentenceIterator.hasNext) {
        val sentenceString = sentenceIterator.next
        val tokenIterator = cc.factorie.app.strings.nlpTokenSegmenter(sentenceString)
        if (tokenIterator.hasNext) {
          val sentence = new Sentence(document)(null) // Automatically sets the correct sentence start boundary
          while (tokenIterator.hasNext) {
            tokenIterator.next
            val start = sentenceIterator.start + tokenIterator.start
            val end = tokenIterator.end
            val length = end - start
            if (length > 0)
              new Token(sentence, start, end) // This will automatically extend the sentence end boundary
          }
        }
      }
    } else {
      val tokenIterator = cc.factorie.app.strings.nlpTokenSegmenter(document.string)
      println("LoadPlainText: <START>"+document.string+"<END>")
      while (tokenIterator.hasNext) {
        tokenIterator.next
        val start = tokenIterator.start
        val end = tokenIterator.end
        val length = end - start
        if (length > 0)
          new Token(document, start, end)
      }
    }
    document
  }
  
  def fromDirectory(dir:File, segmentSentences:Boolean): Seq[Document] = {
    for (file <- files(dir)) yield fromFile(file, segmentSentences)
  }
  
  // Recursively descend directory, returning a list of files.
  // TODO This function should get moved into util/package.scala or somesuch.
  def files(directory:File): Seq[File] = {
    if (!directory.exists) throw new Error("File "+directory+" does not exist")
    if (directory.isFile) return List(directory)
    val result = new scala.collection.mutable.ArrayBuffer[File]
    for (entry <- directory.listFiles) {
      if (entry.isFile) result += entry
      else if (entry.isDirectory) result ++= files(entry)
    }
    result
  }

  
}