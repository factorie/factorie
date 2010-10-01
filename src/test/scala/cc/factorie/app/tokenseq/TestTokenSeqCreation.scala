package cc.factorie.app.tokenseq

import cc.factorie._

import scala.io.{Source}
import scala.collection.mutable.{ArrayBuffer}

import org.junit.Assert._
import org.junit.Before
import org.junit.Test
import org.scalatest.junit.JUnitSuite
import cc.factorie.TestUtils

/**
 * @author Tim Vieira
 * @since Sep 30, 2010
 */

// The Goal of this test is to ensure that all of the customizable parameters
// from creating a labeled.TokenSeq will work properly.
class TestTokenSeqCreation extends JUnitSuite {

  // define classes for Token, Label, and Sequence, currently the base classes
  // cannot be instantiated.. they must be subclassed.
  class MyToken(w:String, l:String)
  extends labeled.Token[MySentence,MyLabel,MyToken](w) {
    val label = new MyLabel(l, this)
  }

  class MyLabel(tag:String, token:MyToken)
  extends labeled.Label[MySentence,MyToken,MyLabel](tag, token)

  class MySentence
  extends labeled.TokenSeq[MyToken,MyLabel,MySentence] {
    override def toString = this.map(_.word).mkString("MySentence(", ", ", ")")
  }

  /** Custom feature function adds a custom prefix to all its features */
  def featureFunction(x: Seq[String]) = List("myfeature[word=%s]" format x(0),
                                             "myfeature[length=%s]" format x(0).length)
  /** Custom label function addes a prefix to all labels */
  def labelFunction(x:String) = "MYPREFIX-" + x

  val sentenceBoundary = "\\A(?:\\s*|-NEWSENTENCE-)\\z".r  // an empty line or a line == "-NEWSENTENCE-"
  val documentBoundary = "-DOCSTART-".r
  val ignoreLines = "^#".r                                 // ignore lines beginning with '#'
  val wordLexer = ("\\$?[0-9]+(?:[0-9,]+[0-9][0-9][0-9])?(?:\\.[0-9]+)?"
                    + "|\\w+"
                    + "|[()]"
                    + "|\\S+").r
  val defaultLabel = "O"
  val newToken = new MyToken(_,_)
  val newSentence = () => new MySentence

  @Test
  def test_fromPlainText = {
    // checks if lexer worked as expected
    // makes sure feature function was called
    // makes sure defaultLabel was used

    def fromPlainText(s:String) = labeled.TokenSeq.fromPlainText(
      Source.fromString(s), newSentence, newToken, defaultLabel, featureFunction, wordLexer)

    def testTokenization(s:String, t:Seq[String]) = assertEquals("Tokenization Error",
                                                                 t, fromPlainText(s).map(_.word))

    testTokenization("The man saw the boy with the telescope.",
                     List("The", "man", "saw", "the", "boy", "with", "the", "telescope", "."))

    testTokenization("I lost $500 gambling in 1995.",
                     List("I", "lost", "$500", "gambling", "in", "1995", "."))

    testTokenization("$5,100.34 (EU800) Mar. 18, 2008.",
                     List("$5,100.34", "(", "EU800", ")", "Mar", ".", "18", ",", "2008", "."))

    // all labels should equal defaultLabel.
    val seq = fromPlainText("The man saw the boy with the telescope.")
    for (lbl <- seq.labels) assertEquals(lbl.value, defaultLabel)

    // assert that the types of Sequence, Token, and Label are correct
    assertTrue(seq          .isInstanceOf[MySentence])
    assertTrue(seq(0)       .isInstanceOf[MyToken])
    assertTrue(seq(0).label .isInstanceOf[MyLabel])

    // assert that all features came from our custom feature function.
    // We can check this because our features function puts a strange
    // prefix, "myfeature", on each of the features it generates.
    for (tk <- seq; feature <- tk.values) assertTrue("Unexpected features found.",
                                                     feature startsWith "myfeature")
  }

  @Test
  def test_fromOWPL_ignoreLines = {
    // Make sure that we ignore commented out lines
    val data = labeled.TokenSeq.fromOWPL(ExampleData.sampleOwpl,
      newSentence, newToken, featureFunction, labelFunction, sentenceBoundary,
      documentBoundary, ignoreLines)

    assertEquals(4, data.filter(_.size>0).size)

    // none of the words should have the comment character
    assertTrue(data.flatten[MyToken].map(_.word).forall(_ != "##"))

    // Now, what happens if we don't ignore commented lines
    val data_not_ignoring = labeled.TokenSeq.fromOWPL(ExampleData.sampleOwpl,
      newSentence, newToken, featureFunction, labelFunction, sentenceBoundary,
      documentBoundary, null)

    assertEquals(6, data_not_ignoring.filter(_.size>0).size)

    // comment characters *should* be present when the filter is not used.
    assertTrue(data_not_ignoring.flatten[MyToken].map(_.word).exists(_ == "##"))
  }

  @Test
  def test_fromOWPL_labelFunction = {
    val data = labeled.TokenSeq.fromOWPL(ExampleData.sampleOwpl,
      newSentence, newToken, featureFunction, labelFunction, sentenceBoundary,
      documentBoundary, ignoreLines)
    // make sure that our prefix was added
    assertTrue(data.flatMap(_.labels).forall(_.value startsWith "MYPREFIX-"))
  }

  @Test
  def test_fromSGML = {
    // TODO: TokenSeq.fromSGML is not implemented yet.
  }

}


// Example data used placed here so it wont't get in the way.
object ExampleData {

  def sampleOwpl = Source.fromString("""
-DOCSTART- -X- O

SOCCER NN I-NP
- : O
COLOMBIA NNP I-NP
BEAT NN I-INTJ
CHILE NN I-NP
4-1 CD I-NP
IN IN I-PP
WORLD NN I-NP
CUP RP I-PRT
QUALIFIER VBN I-VP
. . O
-NEWSENTENCE-
1996-09-01 NNP I-NP

Chile NNP I-NP
4-1 CD I-NP
( ( I-NP
halftime NN I-NP
3-0 CD I-NP
) ) O
in IN I-PP
a DT I-NP
South NNP I-NP
American NNP I-NP
World NNP I-NP
Cup NNP I-NP

## Something commented out
## -DOCSTART- -X- O
## This I-NP
## text O
## should I-NP
## not I-INTJ
## be I-NP
## processed I-NP
## ! . O

-DOCSTART-

SOCCER   I-NP
-        O
COLOMBIA I-NP
BEAT     I-INTJ
CHILE    I-NP
4-1      I-NP
IN       I-PP
WORLD    I-NP
CUP      I-PRT
QUAL     I-VP
.        O


""")

}
