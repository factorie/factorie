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

package cc.factorie.app.nlp
import cc.factorie.app.nlp.phrase.Phrase
import cc.factorie.util.{FastLogging, Cubbie, Threading}
import cc.factorie.app.nlp.coref.Mention
import scala.reflect.ClassTag

trait DocumentAnnotator extends Serializable {
  def process(document: Document): Document  // NOTE: this method may mutate and return the same document that was passed in
  def prereqAttrs: Iterable[Class[_]]
  def postAttrs: Iterable[Class[_]]

  def processSequential(documents: Iterable[Document]): Iterable[Document] = documents.map(process)
  def processParallel(documents: Iterable[Document], nThreads: Int = Runtime.getRuntime.availableProcessors()): Iterable[Document] = Threading.parMap(documents, nThreads)(process)


  /** How the annotation of this DocumentAnnotator should be printed in one-word-per-line (OWPL) format.
      If there is no per-token annotation, return null.  Used in Document.owplString. */
  def tokenAnnotationString(token:Token): String
  
  /** How the annotation of this DocumentAnnotator should be printed as extra information after a one-word-per-line (OWPL) format.
      If there is no document annotation, return the empty string.  Used in Document.owplString. */
  def documentAnnotationString(document:Document): String = ""
  def phraseAnnotationString(phrase:Phrase): String = ""
  def mentionAnnotationString(mention:Mention): String = ""
}

/** Used as a stand-in dummy DocumentAnnotator in the DocumentAnnotatorMap when an annotation was added but not by a real DocumentAnnotator. */
object UnknownDocumentAnnotator extends DocumentAnnotator {
  def process(document: Document): Document = document
  def prereqAttrs: Iterable[Class[_]] = Nil
  def postAttrs: Iterable[Class[_]] = Nil
  def tokenAnnotationString(token: Token) = null
}

object NoopDocumentAnnotator extends DocumentAnnotator {
  def process(document: Document): Document = document
  def prereqAttrs: Iterable[Class[_]] = Nil
  def postAttrs: Iterable[Class[_]] = Nil
  def tokenAnnotationString(token: Token) = null
}

/** A Document Annotator that asserts that an annotation is present and does nothing */
class CheckingDocumentAnnotator[Anno](implicit ct:ClassTag[Anno]) extends DocumentAnnotator {
  def tokenAnnotationString(token: Token) = null

  def postAttrs = Seq(ct.runtimeClass)

  def prereqAttrs = Seq.empty[Class[_]]

  def process(document: Document) = {
    require(document.annotators.keySet.contains(ct.runtimeClass))
    document
  }
}

class CompoundDocumentAnnotator(val annos:Seq[DocumentAnnotator]) extends DocumentAnnotator {
  // for java compat
  def this(annoArr:Array[DocumentAnnotator]) = this(annoArr.toSeq)
  def tokenAnnotationString(token: Token) = annos.map(anno => Option(anno.tokenAnnotationString(token))).mkString("\t")

  lazy val prereqAttrs = annos.flatMap(_.prereqAttrs).toSet diff postAttrs
  lazy val postAttrs = annos.flatMap(_.postAttrs).toSet

  def process(document: Document) = {
    // left fold, but faster, thanks scala
    var doc = document
    val iter = annos.iterator
    while(iter.hasNext) {
      val anno = iter.next()
      //println(s"annotating document ${doc.name} with ${anno.getClass.getName}")
      doc = anno.process(doc)
    }
    doc
  }
}
