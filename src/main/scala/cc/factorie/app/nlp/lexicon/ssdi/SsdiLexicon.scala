package cc.factorie.app.nlp.lexicon.ssdi

import cc.factorie.app.nlp.lexicon.ProvidedTriePhraseLexicon
import cc.factorie.util.ModelProvider

import scala.io.Source

/**
 * @author johnsullivan
 */
class PersonFirstHighest()(implicit mp: ModelProvider[PersonFirstHighest, Source]) extends ProvidedTriePhraseLexicon[PersonFirstHighest]
class PersonFirstHigh()(implicit mp: ModelProvider[PersonFirstHigh, Source]) extends ProvidedTriePhraseLexicon[PersonFirstHigh]
class PersonFirstMedium()(implicit mp: ModelProvider[PersonFirstMedium, Source]) extends ProvidedTriePhraseLexicon[PersonFirstMedium]

class PersonLastHighest()(implicit mp: ModelProvider[PersonLastHighest, Source]) extends ProvidedTriePhraseLexicon[PersonLastHighest]
class PersonLastHigh()(implicit mp: ModelProvider[PersonLastHigh, Source]) extends ProvidedTriePhraseLexicon[PersonLastHigh]
class PersonLastMedium()(implicit mp: ModelProvider[PersonLastMedium, Source]) extends ProvidedTriePhraseLexicon[PersonLastMedium]
