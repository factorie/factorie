package cc.factorie.app.nlp

/**
 * User: apassos
 * Date: 8/7/13
 * Time: 2:48 PM
 */

/** A sequence of DocumentAnnotators packaged as a single DocumentAnnotator.
    This class also properly populates the Document.annotators with a record of which DocumentAnnotator classes provided which annotation classes. */
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

/** A Map from annotation class to DocumentAnnotator that provides that annotation. 
    Used to store default ways of getting certain prerequisite annotations. */
class MutableDocumentAnnotatorMap extends collection.mutable.HashMap[Class[_], () => DocumentAnnotator] {
  def +=(annotator: DocumentAnnotator) = annotator.postAttrs.foreach(a => this(a) = () => annotator)
}

/** A factory for creating DocumentAnnotatorPipelines given requirements about which annotations or which DocumentAnnotators are desired. */
object DocumentAnnotatorPipeline {
  type DocumentAnnotatorMap = collection.Map[Class[_], () => DocumentAnnotator]
  val defaultDocumentAnnotationMap: DocumentAnnotatorMap = Seq(
    classOf[pos.PTBPosLabel] -> (() => pos.POS1),
    classOf[parse.ParseTree] -> (() => parse.DepParser1),
    classOf[segment.PlainNormalizedTokenString] -> (() => segment.PlainTokenNormalizer),
    classOf[Token] -> (() => cc.factorie.app.nlp.segment.Tokenizer1),
    classOf[Sentence] -> (() => cc.factorie.app.nlp.segment.SentenceSegmenter1),
    classOf[lemma.WordNetTokenLemma] -> (() => cc.factorie.app.nlp.lemma.WordNetLemmatizer),
    classOf[lemma.SimplifyDigitsTokenLemma] -> (() => lemma.SimplifyDigitsLemmatizer),
    classOf[lemma.CollapseDigitsTokenLemma] -> (() => lemma.CollapseDigitsLemmatizer),
    classOf[lemma.PorterTokenLemma] -> (() => lemma.PorterLemmatizer),
    classOf[lemma.LowercaseTokenLemma] -> (() => lemma.LowercaseLemmatizer),
    classOf[ner.BilouConllNerLabel] -> (() => ner.NER1),
    classOf[ner.BilouOntonotesNerLabel] -> (() => ner.NER2),
    classOf[mention.ParseBasedMentionList] -> (() => mention.ParseBasedMentionFinding),
    classOf[mention.NerMentionList] -> (() => mention.NerAndPronounMentionFinder),
    classOf[cc.factorie.util.coref.GenericEntityMap[mention.Mention]] -> (() => coref.WithinDocCoref1)
  ).toMap

  def apply(goal: Class[_]): DocumentAnnotationPipeline = apply(Seq(goal), defaultDocumentAnnotationMap)
  def apply[A](implicit m:Manifest[A]): DocumentAnnotationPipeline = apply(m.erasure)
  def apply[A,B](implicit m1:Manifest[A], m2:Manifest[B]): DocumentAnnotationPipeline = apply(Seq(m1.erasure, m2.erasure))
  def apply[A,B,C](implicit m1:Manifest[A], m2:Manifest[B], m3:Manifest[C]): DocumentAnnotationPipeline = apply(Seq(m1.erasure, m2.erasure, m3.erasure))
  def apply[A,B,C,D](implicit m1:Manifest[A], m2:Manifest[B], m3:Manifest[C], m4:Manifest[D]): DocumentAnnotationPipeline = apply(Seq(m1.erasure, m2.erasure, m3.erasure, m4.erasure))
  def apply(goal: Class[_], map: DocumentAnnotatorMap): DocumentAnnotationPipeline = apply(Seq(goal), map)
  def apply[A](map: DocumentAnnotatorMap)(implicit m:Manifest[A]): DocumentAnnotationPipeline = apply(m.erasure, map)
  def apply[A,B](map: DocumentAnnotatorMap)(implicit m1:Manifest[A], m2:Manifest[B]): DocumentAnnotationPipeline = apply(Seq(m1.erasure, m2.erasure), map)
  def apply[A,B,C](map: DocumentAnnotatorMap)(implicit m1:Manifest[A], m2:Manifest[B], m3:Manifest[C]): DocumentAnnotationPipeline = apply(Seq(m1.erasure, m2.erasure, m3.erasure), map)
  def apply[A,B,C,D](map: DocumentAnnotatorMap)(implicit m1:Manifest[A], m2:Manifest[B], m3:Manifest[C], m4:Manifest[D]): DocumentAnnotationPipeline = apply(Seq(m1.erasure, m2.erasure, m3.erasure, m4.erasure), map)

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

  def apply(annotator: DocumentAnnotator, map: DocumentAnnotatorMap = defaultDocumentAnnotationMap): DocumentAnnotationPipeline = {
    val other = new MutableDocumentAnnotatorMap
    map.foreach(k => other += k)
    other += annotator
    apply(annotator.postAttrs, prereqs=Seq(), map=other)
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

