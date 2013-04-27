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

/** Performs some NLP on a Document and returns a Document (perhaps the same one) containing the new annotations. */
trait DocumentProcessor {
  // NOTE: this method may mutate and return the same document that was passed in
  def process1(document: Document): Document
  def prereqAttrs: Iterable[Class[_]]
  def postAttrs: Iterable[Class[_]]
  
  def process(document: Document): Document = process(document, defaultProcessorMap)
  def process(document: Document, processorMap: DocumentProcessorMap): Document = {
    preProcess(document, processorMap)
    val doc = process1(document)
    postAttrs.foreach(p => document.documentProcessors(p) = this) // record which attributes this processor added
    //println("DocumentProcessor.process adding attrs "+postAttrs.mkString(" "))
    doc
  }
  
  /** How the annotation of this DocumentProcessor should be printed in one-word-per-line (OWPL) format.
      If there is no per-token annotation, return null.  Used in Document.owplString. */
  def tokenAnnotationString(token:Token): String = null
  
  def defaultProcessorMap: DocumentProcessorMap = DefaultDocumentProcessorMap
  
  def preProcess(doc: Document, processorMap: DocumentProcessorMap): Unit = {
    // TODO This "contains" is a bug; it should instead mirror the same "subclass lookup" used by attr.apply
    for (prereq <- prereqAttrs) if (!doc.documentProcessors.contains(prereq.asInstanceOf[Class[AnyRef]])) { // TODO Why is this cast necessary?
      //println("DocumentProcessor.preProcess needing to add "+prereq)
      val processor = processorMap(prereq)
      // Make sure we won't over-write some pre-existing annotation
      for (a <- processor.postAttrs) 
        if (doc.documentProcessors.contains(a)) throw new Error(getClass.toString+": annotation collision conflict: "+doc.documentProcessors(a)+" already added attr "+a)
      processor.process(doc, processorMap)
    }
  }
}

class DocumentProcessorMap extends scala.collection.mutable.HashMap[Class[_],DocumentProcessor]

object DefaultDocumentProcessorMap extends DocumentProcessorMap {
  import cc.factorie.app.nlp._
  //this.update(classOf[pos.PTBPosLabel], new pos.POS3) // TODO where should this find its parameters?
  this.update(classOf[lemma.SimplifyDigitsTokenLemma], new lemma.SimplifyDigitsLemmatizer)
  this.update(classOf[Token], cc.factorie.app.nlp.segment.ClearTokenizer)
  this.update(classOf[Sentence], cc.factorie.app.nlp.segment.ClearSegmenter)
}


//trait DocumentAnnotator {
//  // NOTE: this method may mutate and return the same document that was passed in
//  def process1(document: Document): Document
//  def prereqAttrs: Iterable[Class[_]]
//  def postAttrs: Iterable[Class[_]]
//  
//  def process(document: Document): Document = process(document, defaultProcessorMap)
//  def process(document: Document, processorMap: DocumentProcessorMap): Document = {
//    preProcess(document, processorMap)
//    val doc = process1(document)
//    postAttrs.foreach(p => document.documentProcessors(p) = this) // record which attributes this processor added
//    //println("DocumentProcessor.process adding attrs "+postAttrs.mkString(" "))
//    doc
//  }
//  
//  /** How the annotation of this DocumentProcessor should be printed in one-word-per-line (OWPL) format.
//      If there is no per-token annotation, return null.  Used in Document.owplString. */
//  def tokenAnnotationString(token:Token): String = null
//  
//  def defaultProcessorMap: DocumentProcessorMap = DefaultDocumentProcessorMap
//  
//  def preProcess(doc: Document, processorMap: DocumentProcessorMap): Unit = {
//    // TODO This "contains" is a bug; it should instead mirror the same "subclass lookup" used by attr.apply
//    for (prereq <- prereqAttrs) if (!doc.documentProcessors.contains(prereq.asInstanceOf[Class[AnyRef]])) { // TODO Why is this cast necessary?
//      //println("DocumentProcessor.preProcess needing to add "+prereq)
//      val processor = processorMap(prereq)
//      // Make sure we won't over-write some pre-existing annotation
//      for (a <- processor.postAttrs) 
//        if (doc.documentProcessors.contains(a)) throw new Error(getClass.toString+": annotation collision conflict: "+doc.documentProcessors(a)+" already added attr "+a)
//      processor.process(doc, processorMap)
//    }
//  }
//}
//
//class DocumentAnnotatorMap extends scala.collection.mutable.HashMap[Class[_],DocumentAnnotator]
//
//object DefaultDocumentAnnotatorMap extends DocumentAnnotatorMap {
//  import cc.factorie.app.nlp._
//  //this.update(classOf[pos.PTBPosLabel], new pos.POS3) // TODO where should this find its parameters?
//  //this.update(classOf[lemma.SimplifyDigitsTokenLemma], new lemma.SimplifyDigitsLemmatizer)
//  //this.update(classOf[Token], cc.factorie.app.nlp.segment.ClearTokenizer)
//  //this.update(classOf[Sentence], cc.factorie.app.nlp.segment.ClearSegmenter)
//}
