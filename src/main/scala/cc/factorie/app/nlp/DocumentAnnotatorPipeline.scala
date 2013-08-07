package cc.factorie.app.nlp

/**
 * User: apassos
 * Date: 8/7/13
 * Time: 2:48 PM
 */

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

object DocumentAnnotatorPipeline {
  type DocumentAnnotatorMap = collection.Map[Class[_], () => DocumentAnnotator]
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
    classOf[mention.MentionEntityType] ->  (() => mention.MentionEntityTypeLabeler),
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

