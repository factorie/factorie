package cc.factorie.tutorial
import cc.factorie._
import cc.factorie.app.nlp._

/** A simple example of running an NLP document processor. */
object NLP1 extends App {
  val doc = new Document("Mr. Jones took a job at Google in New York.  He and his Australian wife moved from New South Wales on 4/1/12.")
  println(doc.string.length)
  segment.DeterministicTokenizer.process(doc)
  println(doc.tokens.map(_.string).mkString("\n"))
}
