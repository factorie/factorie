package cc.factorie.app.nlp
import org.junit.Test
import cc.factorie.app.nlp.pos.PTBPosLabel
import cc.factorie.app.nlp.parse.ParseTree
import cc.factorie.app.nlp.lemma.WordNetTokenLemma
import cc.factorie.app.nlp.ner.{BilouOntonotesNerLabel, BilouConllNerLabel}
import cc.factorie.app.nlp.mention._
import cc.factorie.util.coref.GenericEntityMap

/**
 * User: apassos
 * Date: 7/18/13
 * Time: 9:06 AM
 */
class TestDocumentAnnotatorMap {
  @Test def testDefaultPipelines() {
    // this map mirrors the default one without loading the models themselves. There should
    // be a less awkward way of doing this
    val map = new MutableDocumentAnnotatorMap
    object pos1 extends DocumentAnnotator {
      def prereqAttrs: Iterable[Class[_]] = List(classOf[Sentence], classOf[segment.SimplifyPTBTokenString])
      def postAttrs: Iterable[Class[_]] = List(classOf[PTBPosLabel])
      def process(document: Document) = document
      def tokenAnnotationString(token: Token) = ""
    }
    map += pos1
    object parser1 extends DocumentAnnotator {
      def prereqAttrs = Seq(classOf[Sentence], classOf[PTBPosLabel], classOf[lemma.WordNetTokenLemma]) // Sentence also includes Token
      def postAttrs = Seq(classOf[ParseTree])
      def process(d: Document) = d
      def tokenAnnotationString(t: Token) = ""
    }
    map += parser1
    map += segment.SimplifyPTBTokenNormalizer
    map += cc.factorie.app.nlp.segment.ClearSegmenter
    object wnLemma extends DocumentAnnotator {
      def prereqAttrs: Iterable[Class[_]] = List(classOf[PTBPosLabel])
      def postAttrs: Iterable[Class[_]] = List(classOf[WordNetTokenLemma])
      def process(d: Document) = d
      def tokenAnnotationString(t: Token) = ""
    }
    map += wnLemma
    map += lemma.SimplifyDigitsLemmatizer
    map += lemma.CollapseDigitsLemmatizer
    map += lemma.PorterLemmatizer
    map += lemma.LowercaseLemmatizer
    object ner1 extends DocumentAnnotator {
      def tokenAnnotationString(token:Token): String = token.attr[BilouConllNerLabel].categoryValue
      def prereqAttrs: Iterable[Class[_]] = List(classOf[Sentence])
      def postAttrs: Iterable[Class[_]] = List(classOf[BilouConllNerLabel])
      def process(d: Document) = d
    }
    map += ner1
    object ner2 extends DocumentAnnotator {
      override def tokenAnnotationString(token:Token): String = token.attr[BilouOntonotesNerLabel].categoryValue
      def prereqAttrs: Iterable[Class[_]] = List(classOf[Token])
      def postAttrs: Iterable[Class[_]] = List(classOf[BilouOntonotesNerLabel])
      def process(document:Document): Document = document
    }
    map += ner2
    object parseBasedMentionFinding extends DocumentAnnotator {
      def prereqAttrs: Iterable[Class[_]] = Seq(classOf[parse.ParseTree])
      def postAttrs: Iterable[Class[_]] = Seq(classOf[MentionList])
      override def tokenAnnotationString(token:Token): String = token.document.attr[MentionList].filter(mention => mention.span.contains(token)) match { case ms:Seq[Mention] if ms.length > 0 => ms.map(m => m.attr[MentionType].categoryValue+":"+m.span.indexOf(token)).mkString(","); case _ => "_" }
      def process(d: Document) = d
    }
    map += parseBasedMentionFinding
    object coref1 extends DocumentAnnotator {
      def tokenAnnotationString(token: Token) = ""
      def prereqAttrs = Seq(classOf[MentionList], classOf[MentionEntityType], classOf[MentionGenderLabel], classOf[MentionNumberLabel])
      def postAttrs = Seq(classOf[GenericEntityMap[Mention]])
      def process(document: Document) = document
    }
    map += coref1
    map += MentionGenderLabeler
    map += MentionNumberLabeler
    object mentionEntityType extends DocumentAnnotator {
      def tokenAnnotationString(token:Token): String = { val mentions = token.document.attr[MentionList].filter(_.span.contains(token)); mentions.map(_.attr[MentionEntityType].categoryValue).mkString(",") }
      def prereqAttrs: Iterable[Class[_]] = List(classOf[MentionList])
      def postAttrs: Iterable[Class[_]] = List(classOf[MentionEntityType])
      def process(d: Document) = d
    }
    map += mentionEntityType
    for (key <- map.keys) {
      DocumentAnnotatorPipeline(key, map.toMap)
      // println(s"Pipeline for $key is ${pipeline.mkString(" ")}")
    }
    DocumentAnnotatorPipeline(map.keys, map.toMap)
  }
}
