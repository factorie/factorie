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
import cc.factorie._

trait DocumentAnnotator {
  def process1(document: Document): Document  // NOTE: this method may mutate and return the same document that was passed in
  def prereqAttrs: Iterable[Class[_]]
  def postAttrs: Iterable[Class[_]]
  
  def process(document: Document): Document = process(document, defaultAnnotatorMap)
  def process(document: Document, annotatorMap: DocumentAnnotatorMap): Document = {
    preProcess(document, annotatorMap)
    val doc = process1(document)
    postAttrs.foreach(p => document.annotators(p) = this) // record which attributes this processor added
    //println("DocumentAnnotator.process adding attrs "+postAttrs.mkString(" "))
    doc
  }
  
  /** How the annotation of this DocumentAnnotator should be printed in one-word-per-line (OWPL) format.
      If there is no per-token annotation, return null.  Used in Document.owplString. */
  def tokenAnnotationString(token:Token): String = null
  
  def defaultAnnotatorMap: DocumentAnnotatorMap = DefaultDocumentAnnotatorMap
  
  def preProcess(doc: Document, annotatorMap: DocumentAnnotatorMap): Unit = {
    for (prereq <- prereqAttrs) if (!doc.hasAnnotation(prereq)) {
      //println("DocumentAnnotator.preProcess needing to add "+prereq)
      val annotator = annotatorMap(prereq)
      // Make sure we won't over-write some pre-existing annotation
      for (a <- annotator.postAttrs) 
        if (doc.annotators.contains(a)) throw new Error(getClass.toString+": annotation collision conflict: prereq "+prereq+" would be satisfied by "+annotator.getClass+", but it provides "+a+" which has already been added by "+doc.annotators(a)+".  If the conflict is for Sentence, you may simply need to ask for Sentence segmentation before Token segmentation.")
      annotator.process(doc, annotatorMap)
    }
  }
}

/** Used as a stand-in dummy DocumentAnnotator in the DocumentAnnotatorMap when an annotation was added but not by a real DocumentAnnotator. */
object UnknownDocumentAnnotator extends DocumentAnnotator {
  def process1(document: Document): Document = document
  def prereqAttrs: Iterable[Class[_]] = Nil
  def postAttrs: Iterable[Class[_]] = Nil
}

object NoopDocumentAnnotator extends DocumentAnnotator {
  def process1(document: Document): Document = document
  def prereqAttrs: Iterable[Class[_]] = Nil
  def postAttrs: Iterable[Class[_]] = Nil
}

class DocumentAnnotatorMap extends scala.collection.mutable.HashMap[Class[_],DocumentAnnotator]

object DefaultDocumentAnnotatorMap extends DocumentAnnotatorMap {
  import cc.factorie.app.nlp._
  //this.update(classOf[pos.PTBPosLabel], pos.POS1)
  //this.update(classOf[parse.ParseTree], parse.DepParser2)
  this.update(classOf[segment.SimplifyPTBTokenString], segment.SimplifyPTBTokenNormalizer)
  this.update(classOf[Token], cc.factorie.app.nlp.segment.ClearTokenizer) // If you ask for this first, and then ask for Sentence, you will get a conflict. -akm
  this.update(classOf[Sentence], cc.factorie.app.nlp.segment.ClearSegmenter)
  this.update(classOf[lemma.SimplifyDigitsTokenLemma], lemma.SimplifyDigitsLemmatizer)
  this.update(classOf[lemma.CollapseDigitsTokenLemma], lemma.CollapseDigitsLemmatizer)
  this.update(classOf[lemma.PorterTokenLemma], lemma.PorterLemmatizer)
  this.update(classOf[lemma.LowercaseTokenLemma], lemma.LowercaseLemmatizer)
  //this.update(classOf[ner.BilouConllNerLabel], ner.NER1) // TODO Add this once DocumentAnnotatorMap actually contains lazily-evaluated values
}
