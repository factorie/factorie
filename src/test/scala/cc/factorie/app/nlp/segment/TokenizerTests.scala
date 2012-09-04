package cc.factorie.app.nlp.segment

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import cc.factorie.app.nlp.{Sentence, Document}

class TokenizerTests extends JUnitSuite {

  @Test def allTests(): Unit = {

    val testText1 =
      """
        Something *less* subtle differentiates them. "f" is a method in *both*
        cases. In the first case, it's a method with one parameter list of
        arity 1 taking an Int, and returning an Int. I'll try to use the word U.S.A. In the second case, f is
        a nullary method returning an Int => Int.

        Now, Int => Int, to be clear about it, is the same thing as Function1[Int, Int].

        Methods are not values, functions are.
      """.stripMargin

//    val tokenized1 = getTokenizedSentences(testText1)
//    printTokenizedSentences(tokenized1)

    val testText2 =
      """
        The problem with MacroTypeTag is that it can be used outside macros.

        A fact about FullTypeTag that I don't like is that it implies that
        it's somehow more full-fledged than TypeTag.

        What about ArbTypeTag (from arbitrary)? I agree the name is cryptic,
        but at least it's not misleading and it doesn't imply that this type
        tag carries more information that a vanilla TypeTag.
      """.stripMargin

    val testText3 = """"""


//    val tokenized2 = getTokenizedSentences(testText2)
//    printTokenizedSentences(tokenized2)

    val jointTokenized = getTokenizedSentences(Seq(testText1, testText2))

    // this should be 7 and 4
    // known bugs:
    // 1. ?/! aren't handled properly for ending sentences
    // 2. ending a sentence with "]." doesn't find the boundary - this is probably ok.
    assert(jointTokenized(0).length == 6, jointTokenized(0).length)
    assert(jointTokenized(1).length == 3, jointTokenized(1).length)

    jointTokenized.foreach(printTokenizedSentences(_))

    val noInference = getTokenizedSentences(Seq(testText1, testText2), None)
    noInference.foreach(printTokenizedSentences(_))
  }

  def printTokenizedSentences(sentences: Seq[Sentence]): Unit = sentences.foreach(sen => println(sen.tokens.map(t => t.stringStart + ": " + t.string)))

  def getTokenizedSentences(text: Seq[String], inference: SentenceBoundaryInference = JointlyAcrossDocuments): Seq[Seq[Sentence]] = {
    val docs = text.map(t => new Document("", t))
    new PunktTokenizer { override def sentenceBoundaryInference = inference }.process(docs)
    docs.map(_.sentences)
  }
}
