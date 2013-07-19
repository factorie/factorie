package cc.factorie.app.nlp

/**
 * User: apassos
 * Date: 6/14/13
 * Time: 12:48 PM
 */

object DefaultAnnotationPipelineFactory extends AnnotationPipelineFactory {
    map.update(classOf[pos.PTBPosLabel], ()=>pos.POS1)
    map.update(classOf[parse.ParseTree], ()=>parse.DepParser1)
    map.update(classOf[segment.SimplifyPTBTokenString], ()=>segment.SimplifyPTBTokenNormalizer)
    map.update(classOf[Token], ()=>cc.factorie.app.nlp.segment.ClearTokenizer) // If you ask for this first, and then ask for Sentence, you will get a conflict. -akm
    map.update(classOf[Sentence], ()=>cc.factorie.app.nlp.segment.ClearSegmenter)
    map.update(classOf[lemma.WordNetTokenLemma], ()=>cc.factorie.app.nlp.lemma.WordNetLemmatizer)
    map.update(classOf[lemma.SimplifyDigitsTokenLemma], ()=>lemma.SimplifyDigitsLemmatizer)
    map.update(classOf[lemma.CollapseDigitsTokenLemma], ()=>lemma.CollapseDigitsLemmatizer)
    map.update(classOf[lemma.PorterTokenLemma], ()=>lemma.PorterLemmatizer)
    map.update(classOf[lemma.LowercaseTokenLemma], ()=>lemma.LowercaseLemmatizer)
    map.update(classOf[ner.BilouConllNerLabel], ()=>ner.NER1)
    map.update(classOf[ner.BilouOntonotesNerLabel], ()=>ner.NER2)
    map.update(classOf[mention.MentionList], ()=>mention.ParseBasedMentionFinding)
    map.update(classOf[cc.factorie.util.coref.GenericEntityMap[mention.Mention]], ()=>coref.WithinDocCoref1)
}
