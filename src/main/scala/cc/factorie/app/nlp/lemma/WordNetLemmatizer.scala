package cc.factorie.app.nlp.lemma
import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.pos.{PTBPosLabel,PTBPosDomain}

class WordNetLemmatizer(val wordNetDir:String) extends DocumentAnnotator {
  val map = new scala.collection.mutable.HashMap[String,String] {
    override def default(key:String): String = key
  }
  val singularNouns = new scala.collection.mutable.HashSet[String]
  for (f <- Seq("adj.exc", "adv.exc", "noun.exc", "verb.exc")) {
    for (line <- scala.io.Source.fromFile(new java.io.File(wordNetDir+"/"+f)).getLines) {
      val fields = line.split(" ")
      if (fields(0).indexOf('_') == -1) // For now skip multi-word phrases (indicated by underscore in WordNet)
        map(fields(0)) = fields(1)
    }
    for (line <- scala.io.Source.fromFile(new java.io.File(wordNetDir+"/index.noun")).getLines) {
      val word = line.split(" ")(0)
      if (!word.contains('_')) singularNouns += word.toLowerCase
    }
  }
  /** This is a rough, temporary stand-in. */
  def lemma(raw:String, pos:String): String = {
    val rawlc = raw.toLowerCase
    val len = rawlc.length
    val wn = map(rawlc)
    if (wn ne null) wn
    else if (singularNouns.contains(rawlc)) rawlc
    else if (PTBPosDomain.isNoun(pos) && !PTBPosDomain.isProperNoun(pos) && rawlc(len-1) == 's') {
      if (!("aeious".contains(rawlc(len-2)) || rawlc.takeRight(3).matches(".*(ics|ngs)$"))) rawlc.dropRight(1) // Remove last 's'
      else rawlc
//    } else if (PTBPosDomain.isVerb(pos)) { rawlc
    } else rawlc
  }
  def process1(document:Document): Document = {
    for (token <- document.tokens) token.attr += new WordNetTokenLemma(token, lemma(token.string, token.posLabel.categoryValue))
    document
  }
  override def tokenAnnotationString(token:Token): String = { val l = token.attr[WordNetTokenLemma]; l.value }
  def prereqAttrs: Iterable[Class[_]] = List(classOf[PTBPosLabel])
  def postAttrs: Iterable[Class[_]] = List(classOf[WordNetTokenLemma])
}

object WordNetLemmatizer {
  val singularNounExceptions = Set("news", "mathematics", "politics")  
}

class WordNetTokenLemma(token:Token, s:String) extends TokenLemma(token, s)
