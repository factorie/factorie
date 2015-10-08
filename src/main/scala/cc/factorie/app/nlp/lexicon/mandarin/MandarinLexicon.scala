package cc.factorie.app.nlp.lexicon.mandarin

import cc.factorie.app.nlp.lexicon.ProvidedTriePhraseLexicon
import cc.factorie.util.ModelProvider

/**
 * @author johnsullivan
 */
class SurnamePinyin()(implicit mp:ModelProvider[SurnamePinyin]) extends ProvidedTriePhraseLexicon[SurnamePinyin]
class GivenNamePinyin()(implicit mp:ModelProvider[GivenNamePinyin]) extends ProvidedTriePhraseLexicon[GivenNamePinyin]
