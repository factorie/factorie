package cc.factorie.app.topics.lda

import scala.collection.mutable.ArrayBuffer
import java.io.File
import cc.factorie._
import scala.util.matching.Regex

/**
 * Created with IntelliJ IDEA.
 * User: vineet
 * Date: 6/27/13
 * Time: 7:35 PM
 * To change this template use File | Settings | File Templates.
 */

class DocumentProvider(val fileName: String, val randomSeed: Long = 0) extends WordSeqProvider {
  val tokenRegex = new Regex("\\p{Alpha}+")
  val random = new scala.util.Random(randomSeed)
  val docBuffer = new ArrayBuffer[Doc]
  val minDocLength = 3

  object WordSeqDomain extends CategoricalSeqDomain[String]
  override def numDocs = docBuffer.length

  val mySegmenter = new cc.factorie.app.strings.RegexSegmenter(tokenRegex)
  def getWordDomain = WordSeqDomain.elementDomain

  //Adds documents and returns the word domain
  override def processDocuments() {
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
  }

  override def getRandomDocument(): CategoricalSeqVariable[String] = {
    val docIndex = random.nextInt(numDocs)
    docBuffer(docIndex).ws
  }

}
