package cc.factorie.app.nlp.lexicon

import org.scalatest._


/**
 * @author Kate Silverstein 
 *         created on 1/12/15
 */

class TestTriePhraseLexicon extends FlatSpec{
  val phrase = "The quick brown fox jumped over the lazy dog"


  "TriePhraseLexicon" should "contain 'fox'" in {
    val lexicon = new TriePhraseLexicon("test")
    phrase.split(" ").foreach(lexicon += _)
    assert(lexicon.containsLemmatizedWord("fox"))
  }

  it should "contain 'the quick brown fox...'" in {
    val lexicon = new TriePhraseLexicon("test")
    lexicon += phrase
    assert(lexicon.containsLemmatizedWords(phrase.split(" ")))
  }

  it should "contain both 'fox' and 'quick brown'" in {
    val words = List("the", "quick brown", "fox")
    val lexicon = new TriePhraseLexicon("test")
    words.foreach(lexicon += _)
    assert(lexicon.containsLemmatizedWord("fox"))
    assert(lexicon.containsLemmatizedWords(Seq("quick", "brown")))
  }

}


