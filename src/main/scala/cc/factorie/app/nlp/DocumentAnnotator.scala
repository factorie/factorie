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
import cc.factorie.app.nlp.mention._
import scala.annotation.tailrec
import cc.factorie.optimize.TrainerHelpers

trait DocumentAnnotator {
  def process(document: Document): Document  // NOTE: this method may mutate and return the same document that was passed in
  def prereqAttrs: Iterable[Class[_]]
  def postAttrs: Iterable[Class[_]]
  
  /** How the annotation of this DocumentAnnotator should be printed in one-word-per-line (OWPL) format.
      If there is no per-token annotation, return null.  Used in Document.owplString. */
  def tokenAnnotationString(token:Token): String
  
  /** How the annotation of this DocumentAnnotator should be printed as extra information after a one-word-per-line (OWPL) format.
      If there is no document annotation, return the empty string.  Used in Document.owplString. */
  def documentAnnotationString(document:Document): String = ""
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

class DocumentAnnotatorMap extends scala.collection.mutable.LinkedHashMap[Class[_], ()=>DocumentAnnotator] {
  def computePipeline(goals: Iterable[Class[_]]): Seq[DocumentAnnotator] = {
    val pipeSet = collection.mutable.LinkedHashSet[DocumentAnnotator]()
    def recursiveSatisfyPrereqs(goal: Class[_]) {
      val provider = this(goal)()
      if (!pipeSet.contains(provider)) {
        provider.prereqAttrs.foreach(recursiveSatisfyPrereqs)
        pipeSet += provider
      }
    }
    goals.foreach(recursiveSatisfyPrereqs)
    checkPipeline(pipeSet.toSeq)
    pipeSet.toSeq
  }

  def checkPipeline(pipeline: Seq[DocumentAnnotator]) {
    val satisfiedSet = collection.mutable.HashSet[Class[_]]()
    for (annotator <- pipeline) {
      for (requirement <- annotator.prereqAttrs; if !satisfiedSet.contains(requirement))
        assert(1 == 0, s"Prerequisite $requirement not satisfied before $annotator gets called in pipeline ${pipeline.mkString(" ")}")
      for (provision <- annotator.postAttrs) {
        assert(!satisfiedSet.contains(provision), s"Pipeline attempting to provide $provision twice. Pipeline: ${pipeline.mkString(" ")}")
        satisfiedSet += provision
      }
    }
  }

  def +=(annotator: DocumentAnnotator) = annotator.postAttrs.foreach(a => this(a) = () => annotator)

  def applyPipeline(annotators: Seq[DocumentAnnotator], document: Document): Document = {
    var doc = document
    for (annotator <- annotators; if annotator.postAttrs.forall(!doc.hasAnnotation(_))) doc = annotator.process(doc)
    doc
  }

  def process(goals: Iterable[Class[_]], document: Document): Document = applyPipeline(computePipeline(goals), document)
  def process(annotator: DocumentAnnotator, document: Document): Document = {
    val other = new DocumentAnnotatorMap
    this.foreach(k => other += k)
    other += annotator
    other.process(annotator.postAttrs, document)
  }
  def processSequential(goals: Iterable[Class[_]], documents: Iterable[Document]): Iterable[Document] = {
    val pipeline = computePipeline(goals)
    documents.map(applyPipeline(pipeline, _))
  }
  def processParallel(goals: Iterable[Class[_]], documents: Iterable[Document], nThreads: Int = Runtime.getRuntime.availableProcessors()): Iterable[Document] = {
    val pipeline = computePipeline(goals)
    TrainerHelpers.parMap(documents, nThreads) { applyPipeline(pipeline, _) }
  }
}
