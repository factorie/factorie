package cc.factorie.app.nlp.lexicon.mandarin

import cc.factorie.app.nlp.lexicon.ProvidedTriePhraseLexicon
import cc.factorie.util.ModelProvider

import scala.io.Source

/**
 * @author johnsullivan
 */
class SurnamePinyin()(implicit mp:ModelProvider[SurnamePinyin, Source]) extends ProvidedTriePhraseLexicon[SurnamePinyin]
class GivenNamePinyin()(implicit mp:ModelProvider[GivenNamePinyin, Source]) extends ProvidedTriePhraseLexicon[GivenNamePinyin]
