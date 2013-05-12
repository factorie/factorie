package cc.factorie.app.nlp.lemma
import cc.factorie._
import cc.factorie.app.nlp._

class WordNetLemmatizer(val wordNetDir:String) extends DocumentAnnotator {
  val map = new scala.collection.mutable.HashMap[String,String] {
    override def default(key:String): String = key
  }
  for (f <- Seq("adj.exc", "adv.exc", "noun.exc", "verb.exc")) {
    for (line <- scala.io.Source.fromFile(new java.io.File(wordNetDir+"/"+f)).getLines) {
      val fields = line.split(" ")
      if (fields(0).indexOf('_') == -1) // For now skip multi-word phrases (indicated by underscore in WordNet)
        map(fields(0)) = fields(1)
    }
  }
  def lemma(raw:String): String = {
    map(raw.toLowerCase)
    // TODO Still needs simple de-pluralization and other rules
  }
  def process1(document:Document): Document = {
    for (token <- document.tokens) token.attr += new WordNetTokenLemma(token, lemma(token.string))
    document
  }
  override def tokenAnnotationString(token:Token): String = { val l = token.attr[WordNetTokenLemma]; l.value }
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token])
  def postAttrs: Iterable[Class[_]] = List(classOf[WordNetTokenLemma])
}
//object WordNetLemmatizer extends WordNetLemmatizer

class WordNetTokenLemma(token:Token, s:String) extends TokenLemma(token, s)
