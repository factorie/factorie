package cc.factorie.app.nlp.lexicon

import org.junit.Test
import org.scalatest.junit.JUnitSuite

/**
 * @author Kate Silverstein 
 *         created on 1/12/15
 */

class TestTriePhraseLexicon extends JUnitSuite{
  val phrase = "the quick brown fox jumped over the lazy dog"

  @Test
  def testContainsLemmatizedWord(): Unit = {
    val lexicon = new TriePhraseLexicon("test")
    phrase.split(" ").foreach(lexicon += _)
    assert(lexicon.containsLemmatizedWord("fox"))
  }

  @Test
  def testContainsLemmatizedWords(): Unit = {
    val lexicon = new TriePhraseLexicon("test")
    lexicon += phrase
    assert(lexicon.containsLemmatizedWords(phrase.split(" ")))
  }

  @Test
  def testMultiword(): Unit = {
    val words = List("the", "quick brown", "fox")
    val lexicon = new TriePhraseLexicon("test")
    words.foreach(lexicon += _)
    assert(lexicon.containsLemmatizedWord("fox"))
    assert(lexicon.containsLemmatizedWords(Seq("quick", "brown")))
  }

}


