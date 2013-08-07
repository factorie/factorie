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
import cc.factorie.app.nlp.mention._
import cc.factorie.optimize.TrainerHelpers

trait DocumentAnnotator {
  def process(document: Document): Document  // NOTE: this method may mutate and return the same document that was passed in
  def prereqAttrs: Iterable[Class[_]]
  def postAttrs: Iterable[Class[_]]

  def processSequential(documents: Iterable[Document]): Iterable[Document] = documents.map(process)
  def processParallel(documents: Iterable[Document], nThreads: Int = Runtime.getRuntime.availableProcessors()): Iterable[Document] = TrainerHelpers.parMap(documents, nThreads)(process)


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

class DocumentAnnotationPipeline(val annotators: Seq[DocumentAnnotator], val prereqAttrs: Seq[Class[_]] = Seq()) extends DocumentAnnotator {
  def postAttrs = annotators.flatMap(_.postAttrs).distinct
  def process(document: Document) = {
    var doc = document
    for (annotator <- annotators; if annotator.postAttrs.forall(!doc.hasAnnotation(_))) {
      doc = annotator.process(doc)
      annotator.postAttrs.foreach(a => document.annotators(a) = annotator.getClass)
    }
    doc
  }
  def tokenAnnotationString(token: Token) = annotators.map(_.tokenAnnotationString(token)).mkString("\t")
}

class MutableDocumentAnnotatorMap extends collection.mutable.HashMap[Class[_], () => DocumentAnnotator] {
  def +=(annotator: DocumentAnnotator) = annotator.postAttrs.foreach(a => this(a) = () => annotator)
}

object DocumentAnnotator {
  type DocumentAnnotatorMap = Map[Class[_], () => DocumentAnnotator]
  val defaultDocumentAnnotationMap: DocumentAnnotatorMap = Seq(classOf[pos.PTBPosLabel] -> (() => pos.POS1),
    classOf[parse.ParseTree] -> (() => parse.DepParser1),
    classOf[segment.SimplifyPTBTokenString] -> (() => segment.SimplifyPTBTokenNormalizer),
    classOf[Token] -> (() => cc.factorie.app.nlp.segment.ClearTokenizer), // If you ask for this first, and then ask for Sentence, you will get a conflict. -akm),
    classOf[Sentence] -> (() => cc.factorie.app.nlp.segment.ClearSegmenter),
    classOf[lemma.WordNetTokenLemma] -> (() => cc.factorie.app.nlp.lemma.WordNetLemmatizer),
    classOf[lemma.SimplifyDigitsTokenLemma] -> (() => lemma.SimplifyDigitsLemmatizer),
    classOf[lemma.CollapseDigitsTokenLemma] -> (() => lemma.CollapseDigitsLemmatizer),
    classOf[lemma.PorterTokenLemma] -> (() => lemma.PorterLemmatizer),
    classOf[lemma.LowercaseTokenLemma] -> (() => lemma.LowercaseLemmatizer),
    classOf[ner.BilouConllNerLabel] -> (() => ner.NER1),
    classOf[ner.BilouOntonotesNerLabel] -> (() => ner.NER2),
    classOf[mention.ParseBasedMentionList] -> (() => mention.ParseBasedMentionFinding),
    classOf[mention.NerMentionList] -> (() => mention.NerAndPronounMentionFinder),
    classOf[cc.factorie.util.coref.GenericEntityMap[mention.Mention]] -> (() => coref.WithinDocCoref1)).toMap

  def apply(goal: Class[_]): DocumentAnnotationPipeline = apply(Seq(goal), defaultDocumentAnnotationMap)
  def apply(goal: Class[_], map: DocumentAnnotatorMap): DocumentAnnotationPipeline = apply(Seq(goal), map)

  def apply(goals: Iterable[Class[_]]): DocumentAnnotationPipeline = apply(goals, Seq(), defaultDocumentAnnotationMap)
  def apply(goals: Iterable[Class[_]], prereqs: Seq[Class[_]]): DocumentAnnotationPipeline = apply(goals, prereqs, defaultDocumentAnnotationMap)
  def apply(goals: Iterable[Class[_]], map: DocumentAnnotatorMap): DocumentAnnotationPipeline = apply(goals, Seq(), map)
  def apply(goals: Iterable[Class[_]], prereqs: Seq[Class[_]], map: DocumentAnnotatorMap): DocumentAnnotationPipeline = {
    val pipeSet = collection.mutable.LinkedHashSet[DocumentAnnotator]()
    val preSet = prereqs.toSet
    def recursiveSatisfyPrereqs(goal: Class[_]) {
      if (!preSet.contains(goal)) {
        val provider = map(goal)()
        if (!pipeSet.contains(provider)) {
          provider.prereqAttrs.foreach(recursiveSatisfyPrereqs)
          pipeSet += provider
        }
      }
    }
    goals.foreach(recursiveSatisfyPrereqs)
    checkPipeline(pipeSet.toSeq)
    new DocumentAnnotationPipeline(pipeSet.toSeq)
  }

  def apply(annotator: DocumentAnnotator, map: Map[Class[_], () => DocumentAnnotator] = defaultDocumentAnnotationMap): DocumentAnnotationPipeline = {
    val other = new MutableDocumentAnnotatorMap
    map.foreach(k => other += k)
    other += annotator
    apply(annotator.postAttrs, prereqs=Seq(), map=other.toMap)
  }

  def process(goals: Iterable[Class[_]], document: Document): Document = apply(goals, map=defaultDocumentAnnotationMap).process(document)
  def process(annotator: DocumentAnnotator, document: Document): Document = apply(annotator, map=defaultDocumentAnnotationMap).process(document)
  def process(goals: Iterable[Class[_]], document: Document, map: DocumentAnnotatorMap): Document = apply(goals, map=map).process(document)
  def process(annotator: DocumentAnnotator, document: Document, map: DocumentAnnotatorMap): Document = apply(annotator, map=map).process(document)

  def checkPipeline(pipeline: Seq[DocumentAnnotator]) {
    val satisfiedSet = collection.mutable.HashSet[Class[_]]()
    for (annotator <- pipeline) {
      for (requirement <- annotator.prereqAttrs
           if !satisfiedSet.contains(requirement)
           if !satisfiedSet.exists(c => requirement.isAssignableFrom(c)))
        assert(1 == 0, s"Prerequisite $requirement not satisfied before $annotator gets called in pipeline ${pipeline.mkString(" ")}")
      for (provision <- annotator.postAttrs) {
        assert(!satisfiedSet.contains(provision), s"Pipeline attempting to provide $provision twice. Pipeline: ${pipeline.mkString(" ")}")
        satisfiedSet += provision
      }
    }
  }
}

