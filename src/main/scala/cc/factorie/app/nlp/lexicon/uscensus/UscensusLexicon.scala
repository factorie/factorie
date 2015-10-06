package cc.factorie.app.nlp.lexicon.uscensus

import cc.factorie.app.nlp.lexicon.ProvidedTriePhraseLexicon
import cc.factorie.util.ModelProvider

import scala.io.Source

/**
 * @author johnsullivan
 */

class PersonFirstFemale()(implicit mp :ModelProvider[PersonFirstFemale, Source]) extends ProvidedTriePhraseLexicon[PersonFirstFemale]
class PersonFirstMale()(implicit mp :ModelProvider[PersonFirstMale, Source]) extends ProvidedTriePhraseLexicon[PersonFirstMale]
class PersonLast()(implicit mp :ModelProvider[PersonLast, Source]) extends ProvidedTriePhraseLexicon[PersonLast]
