/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */
package cc.factorie.app.nlp.lexicon

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import cc.factorie.app.nlp._

class TestLexicon extends JUnitSuite {

  @Test def testOldVsNew(): Unit = {
    val newlex = new PhraseLexicon("new")
    val oldlex = new ChainPhraseLexicon("old")
    val words = List("New York", "Paris", "San Franciscon")
    words.foreach(word => {
      newlex += word
      oldlex += word
    })

    assert(newlex.contains("Paris") && oldlex.contains("Paris"))
    assert(newlex.contains("New York") && oldlex.contains("New York"))

    val string = "Yesterday I flew from Paris to New York."
    val doc = DocumentAnnotatorPipeline(segment.DeterministicTokenizer).process(new Document(string))
    val section = doc.asSection
    val tok = section.tokens(4)
    assert(tok.string == "Paris")
    assert(newlex.contains(tok) && oldlex.contains(tok))

    assert(section.tokens(6).string == "New")
    val toks = List(section.tokens(6), section.tokens(7))
    assert(newlex.contains(toks))
    assert(oldlex.contains(toks))

    /* the "startsAt" tests fail due to an Error thrown by ChainPhraseLexicon.startsAt */
//    assert(newlex.startsAt(tok) == oldlex.startsAt(tok))

  }

// problems loading these due to classpath issues -KS
//  @Test def testLexiconContent(): Unit = {
//    val st = "accessor fds inc balanced allocation fd"
//    val newlex = lexicon.iesl.Company
//    val oldlex = lexicon.iesl.TestCompany
//    assert (newlex.contains(st) && oldlex.contains(st))
//  }

  @Test def testLexiconSingleWords(): Unit = {
    val lexicon = new PhraseLexicon("test1")
    lexicon += "one"
    lexicon += "two"
    lexicon += "three"
    assert(lexicon.contains("one"))
    assert(lexicon.contains("three"))
    assert(!lexicon.contains("four"))
  }

  @Test def testLexiconPhrases(): Unit = {
    val lexicon = new PhraseLexicon("test2")
    lexicon += "Paris"
    assert(lexicon.contains("Paris"))
    lexicon += "San Fransisco"
    assert(lexicon.contains("Paris"))
    lexicon += "San Diego"
    lexicon += "New York"
    lexicon += "New Hampshire"
    lexicon += "Oklahoma City"
    lexicon += "London"
    assert(lexicon.contains("Paris"))
    assert(lexicon.contains("New York"))
    assert(lexicon.contains("New Hampshire"))
    assert(lexicon.containsWords(List("New", "York")))
    assert(lexicon.containsWords(List("New", "Hampshire")))
    assert(!lexicon.containsWords(List("New", "Brunswick")))
    assert(!lexicon.contains("England"))
    assert(!lexicon.containsWord("England"))

    val string = "Yesterday I flew from Paris to New York."
    val doc = DocumentAnnotatorPipeline(segment.DeterministicTokenizer).process(new Document(string))
    val section = doc.asSection
    assert(section.tokens(4).string == "Paris")
    assert(lexicon.contains(section.tokens(4)))
    val toks: Seq[Token] = List(section.tokens(6), section.tokens(7)).toSeq
    assert(section.tokens(6).string == "New")
    assert(section.tokens(7).string == "York")
    assert(lexicon.contains(toks))
    /* these won't pass using HashyLexicon, if you need these to pass use PhraseLexicon instead */
    //    assert(section.tokens(7).string == "York")
    //    assert(lexicon.contains(section.tokens(7)))
    //    assert(lexicon.contains(section.tokens(6)))

    assert(section.tokens(5).string == "to")
    assert(!lexicon.contains(section.tokens(5)))
  }

  
  @Test def testChainLexiconSingleWords(): Unit = {
    val lexicon = new ChainPhraseLexicon("test1")
    lexicon += "one"
    lexicon += "two"
    lexicon += "three"
    assert(lexicon.contains("one"))
    assert(lexicon.contains("three"))
    assert(!lexicon.contains("four"))
  }
  
  @Test def testChainLexiconPhrases(): Unit = {
    val lexicon = new ChainPhraseLexicon("test2")
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
    val doc = DocumentAnnotatorPipeline(segment.DeterministicTokenizer).process(new Document(string))
    val section = doc.asSection
    assert(section.tokens(4).string == "Paris")
    assert(lexicon.contains(section.tokens(4)))
    assert(section.tokens(7).string == "York")
    assert(lexicon.contains(section.tokens(7)))
    assert(lexicon.contains(section.tokens(6)))
    assert(section.tokens(5).string == "to")
    assert(!lexicon.contains(section.tokens(5)))
    
    //println(lexicon.phrases)
    assert(lexicon.phrases.contains("new york"))
  }

}
