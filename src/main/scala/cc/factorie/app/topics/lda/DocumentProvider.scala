package cc.factorie.app.topics.lda

import scala.collection.mutable.ArrayBuffer
import java.io.File
import cc.factorie._
import scala.util.matching.Regex
import cc.factorie.app.strings.StringSegmenter

/**
 * Created with IntelliJ IDEA.
 * User: vineet
 * Date: 6/27/13
 * Time: 7:35 PM
 * To change this template use File | Settings | File Templates.
 */

class DocumentProvider(val fileName: String, mySegmenter:StringSegmenter)(implicit val random:scala.util.Random) extends WordSeqProvider {
  val docBuffer = new ArrayBuffer[Doc]
  val minDocLength = 3

  object WordSeqDomain extends CategoricalSeqDomain[String]
  def numDocs = docBuffer.length
  def getWordDomain = WordSeqDomain.elementDomain

  def nextDocument(): Stream[CategoricalSeqVariable[String]] = {
    Stream.cons(getRandomDocument(), nextDocument())
  }

  //Adds documents and returns the word domain
  def initializeDocuments(): Stream[CategoricalSeqVariable[String]] = {
    val source = scala.io.Source.fromFile(new File(fileName))
    var count = 0
    for (line <- source.getLines()) {
      val text: String = line
      val doc = Document.fromString(WordSeqDomain, fileName +":"+count, text, segmenter = mySegmenter)
      if (doc.length >= minDocLength) docBuffer += doc
      count += 1
      if (count % 1000 == 0) { print(" "+count); Console.flush() }; if (count % 10000 == 0) println()
    }
    source.close()
    nextDocument()
  }

  def getRandomDocument(): CategoricalSeqVariable[String] = {
    val docIndex = random.nextInt(numDocs)
    docBuffer(docIndex).ws
  }
}