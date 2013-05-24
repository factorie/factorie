package cc.factorie.app.nlp.lexicon

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import cc.factorie.app.nlp._

class TestLexicon extends JUnitSuite {
  
  @Test def testLexiconSingleWords(): Unit = {
    val lexicon = new Lexicon
    lexicon += "one"
    lexicon += "two"
    lexicon += "three"
    assert(lexicon.contains("one"))
    assert(lexicon.contains("three"))
    assert(!lexicon.contains("four"))
  }
  
  @Test def testLexiconPhrases(): Unit = {
    val lexicon = new Lexicon
    lexicon += "Paris"
    assert(lexicon.contains("Paris"))
    lexicon += "San Fransisco"
    assert(lexicon.contains("Paris"))
    lexicon += "San Diego"
    lexicon += "New York"
    lexicon += "Oklahoma City"
    lexicon += "London"
    assert(lexicon.contains("Paris"))
    assert(lexicon.contains("Paris"))
    assert(lexicon.contains("New York"))
    assert(lexicon.containsWords(List("New", "York")))
    assert(!lexicon.containsWords(List("New", "Hampshire")))
    assert(!lexicon.contains("England"))
    assert(!lexicon.containsWord("England"))
    
    val string = "Yesterday I flew from Paris to New York."
    val doc = new Document(string)
    segment.RegexTokenizer.process(doc)
    assert(doc.tokens(4).string == "Paris")
    assert(lexicon.contains(doc.tokens(4)))
    assert(doc.tokens(7).string == "York")
    assert(lexicon.contains(doc.tokens(7)))
    assert(lexicon.contains(doc.tokens(6)))
    assert(doc.tokens(5).string == "to")
    assert(!lexicon.contains(doc.tokens(5)))
    
    //println(lexicon.phrases)
    assert(lexicon.phrases.contains("new york"))
  }

}
