package cc.factorie.tutorial

object NlpPipeline extends App {
  import cc.factorie._
  import cc.factorie.app.nlp._
  val doc = new Document("Education is the most powerful weapon which you can use to change the world.")
  DocumentAnnotatorPipeline(pos.OntonotesForwardPosTagger).process(doc)
  for (token <- doc.tokens)
    println("%-10s %-5s".format(token.string, token.posTag.categoryValue))
}
