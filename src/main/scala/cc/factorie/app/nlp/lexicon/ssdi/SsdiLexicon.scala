package cc.factorie.app.nlp.lexicon.ssdi

import cc.factorie.app.nlp.lexicon.ProvidedTriePhraseLexicon
import cc.factorie.util.ModelProvider

/**
 * @author johnsullivan
 */
class PersonFirstHighest()(implicit mp: ModelProvider[PersonFirstHighest]) extends ProvidedTriePhraseLexicon[PersonFirstHighest]
class PersonFirstHigh()(implicit mp: ModelProvider[PersonFirstHigh]) extends ProvidedTriePhraseLexicon[PersonFirstHigh]
class PersonFirstMedium()(implicit mp: ModelProvider[PersonFirstMedium]) extends ProvidedTriePhraseLexicon[PersonFirstMedium]

class PersonLastHighest()(implicit mp: ModelProvider[PersonLastHighest]) extends ProvidedTriePhraseLexicon[PersonLastHighest]
class PersonLastHigh()(implicit mp: ModelProvider[PersonLastHigh]) extends ProvidedTriePhraseLexicon[PersonLastHigh]
class PersonLastMedium()(implicit mp: ModelProvider[PersonLastMedium]) extends ProvidedTriePhraseLexicon[PersonLastMedium]
