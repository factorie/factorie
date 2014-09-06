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

package cc.factorie.app.nlp
import cc.factorie.app.nlp.phrase.Phrase
import cc.factorie.util.{Cubbie, Threading}
import cc.factorie.app.nlp.coref.Mention
import scala.reflect.ClassTag
import cc.factorie.app.nlp.pos.PennPosTag
import java.util.Date
import scala.util.Random
import cc.factorie.variable.DiffList

trait DocumentAnnotator {
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

trait TypedDocumentAnnotator[Annotation] extends DocumentAnnotator {
  implicit val ct:ClassTag[Annotation]
  lazy val postAttrs = Seq(ct.runtimeClass)

  def process1(document:Document):Document
  final def process(document: Document) = {
    document.annotators += postAttrs.head -> this.getClass
    process1(document)
  }
}

object RandPOSAnnotator extends TypedDocumentAnnotator[PennPosTag] {

  implicit val ct: ClassTag[PennPosTag] = ClassTag(classOf[PennPosTag])

  def prereqAttrs = Seq(classOf[Token], classOf[Sentence])

  def process1(document: Document) = {
    implicit val r = new Random()
    implicit val d = null.asInstanceOf[DiffList]

    document.sentences.foreach { sent =>
      sent.tokens.foreach { tok =>
        val pos = new PennPosTag(tok, "NN")
        pos.setRandomly
        tok.attr += pos
      }
    }
    document
  }

  def tokenAnnotationString(token: Token) = { val label = token.attr[PennPosTag]; if (label ne null) label.categoryValue else "(null)" }
}

trait AnnotatorSerializer[Serialized] {
  def serialize(doc:Document):Serialized

  def deserialize(ser:Serialized, doc:Document):Document
}

class TokenAnnotatorCubbie extends Cubbie {
  val annotator = StringSlot("annotator")
  val annotation = StringSlot("annotation")
  val timestamp = DateSlot("ts")
  val data = IntListSlot("data")
}

object PennPosCubbieSerializer extends AnnotatorSerializer[TokenAnnotatorCubbie] {
  def serialize(doc: Document) = {
    assert(doc.hasAnnotation(classOf[PennPosTag]))
    val ser = new TokenAnnotatorCubbie
    ser.annotation := classOf[PennPosTag].toString
    ser.annotator := doc.annotatorFor(classOf[PennPosTag]).get.toString
    ser.timestamp := new Date
    ser.data := doc.tokens.map(_.posTag.intValue).toSeq
    ser
  }

  def deserialize(ser: TokenAnnotatorCubbie, doc: Document) = {
    assert(ser.data.value.length == doc.tokenCount)
    //doc.annotators += Class.forName(ser.annotation.value) -> Class.forName(ser.annotator.value)
    doc.tokens.zip(ser.data.value).foreach { case(tok, posCat) =>
      val pos = new PennPosTag(tok, "NN")
      pos.set(posCat)(null)
      tok.attr += pos
    }
    doc
  }
}