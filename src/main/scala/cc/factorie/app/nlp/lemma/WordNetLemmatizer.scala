package cc.factorie.app.nlp.lemma
import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.pos.{PennPosLabel,PennPosDomain}
import java.io.{FileInputStream,InputStream}
import cc.factorie.util.ClasspathURL
import cc.factorie.app.nlp.wordnet.WordNet

// TODO Rather than reading the WordNet files here, I think this object should simply depend on newly-written methods in wordnet.WordNet. -akm 

class WordNetLemmatizer(val inputStreamFactory: String=>InputStream) extends DocumentAnnotator {
  def this(wordNetDir:java.io.File) = this((string:String) => new FileInputStream(new java.io.File(wordNetDir, string)))
 // def this(url:java.net.URL) = ???
  //def this() = this(util.InputStreamFromJar(classOf[WordNetLemmatizer]))
  
  val resourcePath = "dict/"
  def sourceFactory(string:String): io.Source = io.Source.fromInputStream(inputStreamFactory(resourcePath+string))
  
  val NOUN = "n"
  val VERB = "v"
  val ADJC = "aj"
  val ADVB = "av"

  /* Wordnet suffixes - from file morph.c in wordnet
   * Look at each word, check its suffix, and change its suffix to
   * the corresponding "end" as defined by wordnet (if you can find
   * the suffix in one of the following suffix lists)*/
  val nounSufx = List("s", "ses", "xes", "zes", "ches", "shes", "men", "ies")
  val nounEnds = List("", "s", "x", "z", "ch", "sh", "man", "y")
  val verbSufx = List("s", "es", "es", "ed",  "ed", "ies", "ing", "ing")
  val verbEnds = List("", "e", "", "e", "", "y", "e", "")
  val adjcSufx = List("er", "est")
  val adjcEnds = List("", "", "e", "e")

  val sufxMap = Map (NOUN -> nounSufx.zip(nounEnds), VERB -> verbSufx.zip(verbEnds), ADJC -> adjcSufx.zip(adjcEnds))

  /* Store wordnet exceptions */
  val exceptionMap = Map[String, scala.collection.mutable.HashMap[String,String]](
    NOUN -> new scala.collection.mutable.HashMap[String, String]() { override def default(key:String): String = key },
    VERB -> new scala.collection.mutable.HashMap[String, String]() { override def default(key:String): String = key },
    ADJC -> new scala.collection.mutable.HashMap[String, String]() { override def default(key:String): String = key },
    ADVB -> new scala.collection.mutable.HashMap[String, String]() { override def default(key:String): String = key }
  )

  val wordNetWords = Map[String, scala.collection.mutable.HashSet[String]](
    NOUN -> new scala.collection.mutable.HashSet[String](),
    VERB -> new scala.collection.mutable.HashSet[String](),
    ADJC -> new scala.collection.mutable.HashSet[String](),
    ADVB -> new scala.collection.mutable.HashSet[String]()
  )

  for ((f, pos) <- Seq(("adj", ADJC), ("adv", ADVB), ("noun", NOUN), ("verb", VERB))) {
    for (line <- sourceFactory(f + ".exc").getLines()) {
      val fields = line.split(" ")
      if (fields(0).indexOf('_') == -1) // For now skip multi-word phrases (indicated by underscore in WordNet)
        exceptionMap(pos)(fields(0)) = fields(1)
    }

    for (line <- sourceFactory("index." + f).getLines()) {
      val word = line.split(" ")(0)
      if (!word.contains('_')) wordNetWords(pos) += word.toLowerCase
    }
  }

  def lemma(raw:String, partOfSpeech:String): String = {
    val rawlc = raw.toLowerCase
    val pos = {
      if (PennPosDomain.isAdjective(partOfSpeech)) ADJC
      else if (PennPosDomain.isNoun(partOfSpeech)) NOUN
      else if (PennPosDomain.isVerb(partOfSpeech)) VERB
      else ADVB
    }

    if (exceptionMap(pos).contains(rawlc)) exceptionMap(pos)(rawlc)
    else if (wordNetWords(pos).contains(rawlc)) rawlc
    else if (pos == ADVB && rawlc.endsWith("ly")) rawlc.dropRight(2)  /* this rule does not appear in wordnet */
    else if (pos == ADVB) rawlc                                       /* wordnet contains many unlemmatized adverbs */
    else if (rawlc.length <= 2) rawlc
    else if (PennPosDomain.isNoun(pos) && rawlc.endsWith("ss")) rawlc
    else if (PennPosDomain.isNoun(pos) && rawlc.endsWith("ful")) this.wordbase(rawlc.dropRight(3), NOUN) + "ful"
    else wordbase(rawlc, pos)
  }
  def process(document:Document): Document = {
    for (token <- document.tokens) token.attr += new WordNetTokenLemma(token, lemma(token.string, token.posLabel.categoryValue))
    document
  }
  override def tokenAnnotationString(token:Token): String = { val l = token.attr[WordNetTokenLemma]; l.value }
  def prereqAttrs: Iterable[Class[_]] = List(classOf[PennPosLabel])
  def postAttrs: Iterable[Class[_]] = List(classOf[WordNetTokenLemma])

  private def wordbase(w: String, pos: String): String = {
    val candidates  = this.sufxMap(pos).filter(sufxAndEnd => w.endsWith(sufxAndEnd._1))
    val transformed = candidates.map(sufxAndEnd => w.dropRight(sufxAndEnd._1.length).concat(sufxAndEnd._2))
    val areDefined  = transformed.filter(word => wordNetWords(pos).contains(word))

    if (areDefined.length <= 0 ) w
    else areDefined.last           /* TODO: be smarter than taking the last? this at least take longest endsWith match */
  }
}

object WordNetLemmatizer extends WordNetLemmatizer(string => ClasspathURL.fromDirectory[WordNet](string).openConnection().getInputStream)

//string => {
//import java.io.File
//import java.util.jar.JarFile
//  val propertyName = "cc.factorie.app.nlp.wordnet.jar"
//  val jarLocationProperty = System.getProperty(propertyName, null)
//  if (jarLocationProperty ne null) {
//    // Try to load from .jar in filesystem location specified by System property cc.factorie.app.nlp.lexicon.jar
//    val file = new File(jarLocationProperty)
//    if (!file.exists) throw new Error("File not found at System Property "+propertyName+" value: "+jarLocationProperty)
//    try {
//      val jarFile = new JarFile(file)
//      val jarEntry = jarFile.getJarEntry(string)
//      jarFile.getInputStream(jarEntry)
//    } catch {
//      case e:Exception => throw new Error("Error loading resource '"+string+"' from jar '"+file+"'", e)
//    }
//  } else {
//    // Try to load from .jar on classpath
//    try {
//      wordnet.WordNet.getClass.getResourceAsStream(string)
//    } catch {
//      case e:Exception => throw new Error("Could not find resource for cc.factorie.app.nlp.lexicon: "+string+".  \nDownload factorie-nlp-lexicon.jar and then either add it classpath or set Java System Property 'cc.factorie.app.nlp.lexicon.jar' to its file system location.", e)
//    }
//  }
//})

//object WordNetLemmatizer {
//  // TODO Move this to a JUnit test. -akm
//  def main(args: Array[String]) {
//    val wnl = new cc.factorie.app.nlp.lemma.WordNetLemmatizer(new java.io.File(args(0)))
//    val testWords = List(
//      ("grass", "N"),
//      ("blue", "J"),
//      ("makings", "N"),
//      ("gooey", "J"),
//      ("aklsdjflk", "N"),
//      ("aggressively", "adverb"),
//      ("sparked", "V"),
//      ("walking", "V"),
//      ("loves", "V"),
//      ("polyhedra", "N"),
//      ("orogami", "N"),
//      ("watches", "V"),
//      ("watches", "N"),
//      ("spying", "V"),
//      ("news", "N"),
//      ("mathematics", "N"),
//      ("POLITICS", "N"),
//      ("wonderful", "J")
//    )
//
//    testWords.map(x => println(x._1 + " -> " + wnl.lemma(x._1, x._2)))
//  }
//}

class WordNetTokenLemma(token:Token, s:String) extends TokenLemma(token, s)

