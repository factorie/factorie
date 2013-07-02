package cc.factorie.app.nlp

/**
 * User: apassos
 * Date: 6/14/13
 * Time: 12:48 PM
 */

object Implicits {
  implicit val defaultDocumentAnnotatorMap = new DocumentAnnotatorLazyMap {
    this.update(classOf[pos.PTBPosLabel], ()=>pos.POS1)
    this.update(classOf[parse.ParseTree], ()=>parse.DepParser2)
    this.update(classOf[segment.SimplifyPTBTokenString], ()=>segment.SimplifyPTBTokenNormalizer)
    this.update(classOf[Token], ()=>cc.factorie.app.nlp.segment.ClearTokenizer) // If you ask for this first, and then ask for Sentence, you will get a conflict. -akm
    this.update(classOf[Sentence], ()=>cc.factorie.app.nlp.segment.ClearSegmenter)
    this.update(classOf[lemma.WordNetTokenLemma], ()=>cc.factorie.app.nlp.lemma.WordNetLemmatizer)
    this.update(classOf[lemma.SimplifyDigitsTokenLemma], ()=>lemma.SimplifyDigitsLemmatizer)
    this.update(classOf[lemma.CollapseDigitsTokenLemma], ()=>lemma.CollapseDigitsLemmatizer)
    this.update(classOf[lemma.PorterTokenLemma], ()=>lemma.PorterLemmatizer)
    this.update(classOf[lemma.LowercaseTokenLemma], ()=>lemma.LowercaseLemmatizer)
    this.update(classOf[ner.BilouConllNerLabel], ()=>ner.NER1)
    this.update(classOf[ner.BilouOntonotesNerLabel], ()=>ner.NER2)
    this.update(classOf[mention.MentionList], ()=>mention.ParseBasedMentionFinding)
    this.update(classOf[cc.factorie.util.coref.GenericEntityMap[mention.Mention]], ()=>coref.WithinDocCoref1)
  }
}