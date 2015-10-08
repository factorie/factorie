package cc.factorie.app.nlp.lexicon.uscensus

import cc.factorie.app.nlp.lexicon.ProvidedTriePhraseLexicon
import cc.factorie.util.ModelProvider

/**
 * @author johnsullivan
 */

class PersonFirstFemale()(implicit mp :ModelProvider[PersonFirstFemale]) extends ProvidedTriePhraseLexicon[PersonFirstFemale]
class PersonFirstMale()(implicit mp :ModelProvider[PersonFirstMale]) extends ProvidedTriePhraseLexicon[PersonFirstMale]
class PersonLast()(implicit mp :ModelProvider[PersonLast]) extends ProvidedTriePhraseLexicon[PersonLast]
