package cc.factorie.example

import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.app.chain._
import cc.factorie.app.classify._
import cc.factorie.app.strings.Stopwords

object TokenLDA {
  
  object ChainNerFeaturesDomain extends CategoricalVectorDomain[String]
  val lexicons = new scala.collection.mutable.ArrayBuffer[Lexicon]
  
  
  class ChainNerFeatures(val token:Token) extends BinaryFeatureVectorVariable[String] {
    def domain = ChainNerFeaturesDomain
    override def skipNonCategories = true
  }
  
  object LexiconLabelDomain extends CategoricalDomain[String] { this += "TRUE" }
  class LexiconLabel(value:String, val token:Token) extends LabelVariable(value) {
    def domain = LexiconLabelDomain
  }

  
  def initFeatures(document:Document): Unit = {
    import cc.factorie.app.strings.simplifyDigits
    for (token <- document) {
      val rawWord = token.string
      val word = simplifyDigits(rawWord).toLowerCase
      val features = token.attr += new ChainNerFeatures(token)
      features += "W="+word
      //features += "SHAPE="+cc.factorie.app.strings.stringShape(rawWord, 2)
      //features += "POS="+token.attr[cc.factorie.app.nlp.pos.PosLabel].categoryValue
      //if (token.isPunctuation) features += "PUNCTUATION"
      if (lexicons.size > 0) {
        if (lexicons.head.contains(token)) token.attr += new LexiconLabel("TRUE", token)
        else token.attr += new LexiconLabel("NONE", token)
      }
    }
    for (sentence <- document.sentences)
      cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions(sentence, (t:Token)=>t.attr[ChainNerFeatures], List(0), List(1), List(2), List(-1), List(-2))
    // If the sentence contains no lowercase letters, tell all tokens in the sentence they are part of an uppercase sentence
    //document.sentences.foreach(s => if (!s.exists(_.containsLowerCase)) s.foreach(t => t.attr[ChainNerFeatures] += "SENTENCEUPPERCASE"))
    // Add features for character n-grams between sizes 2 and 5
    //document.foreach(t => if (t.string.matches("[A-Za-z]+")) t.attr[ChainNerFeatures] ++= t.charNGrams(2,5).map(n => "NGRAM="+n))
    // Add features from window of 4 words before and after
    document.foreach(t => t.attr[ChainNerFeatures] ++= t.prevWindow(6).dropRight(2).filter(t2 => t2.string.length > 1 && !Stopwords.contains(t2.string)).map(t2 => "PREVWINDOW="+simplifyDigits(t2.string).toLowerCase))
    document.foreach(t => t.attr[ChainNerFeatures] ++= t.nextWindow(6).drop(2).     filter(t2 => t2.string.length > 1 && !Stopwords.contains(t2.string)).map(t2 => "NEXTWINDOW="+simplifyDigits(t2.string).toLowerCase))
  }
  
  def main(args:Array[String]): Unit = {
    val numTopics = 100
    
    for (filename <- List("cities", "companies", "companysuffix", "countries", "days", "firstname.high", "firstname.highest", "firstname.med", "jobtitle", "lastname.high", "lastname.highest", "lastname.med", "months", "states")) {
      println("Reading lexicon "+filename)
      lexicons += new Lexicon("/Users/mccallum/research/data/resources/lexicons/"+filename)
    }
    
    val docs = LoadConll2003.fromFilename("/Users/mccallum/research/data/ie/ner2003/eng.train")
    docs.foreach(doc => initFeatures(doc))
    
    val labels = new LabelList[LexiconLabel](_.token.attr[ChainNerFeatures]) ++= docs.flatMap(_.map(_.attr[LexiconLabel]))
    val trainer = new MaxEntSampleRankTrainer()
    val classifier = trainer.train(labels)
    val trial = new Trial(classifier) ++= labels
    val uniqueTokens = new scala.collection.mutable.HashSet[String]
    trial.sortBy(_.proportions(0)).reverse.filter(classification => !lexicons.head.contains(classification.label.token)).map(_.label.token.string).take(200).foreach(word => if (!uniqueTokens.contains(word)) { println(word); uniqueTokens.add(word) })
    
    
    
    
//    object WordSeqDomain extends CategoricalSeqDomain[String]
//    val model = cc.factorie.generative.GenerativeModel()
//    val lda = new cc.factorie.app.topics.lda.LDA(WordSeqDomain, numTopics)(model)
//    
//    for (doc <- docs; token <- doc; if (token.isCapitalized)) {
//      val ldaDoc = new cc.factorie.app.topics.lda.Document(WordSeqDomain, doc.name+":"+token.position, Nil)
//      for (feature <- token.attr[ChainNerFeatures].activeCategories)
//        ldaDoc.appendCategory(feature)
//      //for (i <- 1 to 5) 
//      //if (token.nerLabel.categoryValue != "O") ldaDoc.appendCategory("LABEL="+token.nerLabel.categoryValue)
//      lda.addDocument(ldaDoc)
//    }
//    
//    println("\nRead "+lda.documents.size+" documents, "+WordSeqDomain.elementDomain.size+" word types, "+lda.documents.map(_.ws.length).sum+" word tokens.")
//    lda.inferTopics(iterations = 100, fitAlphaInterval = 20, diagnosticInterval = 10)
//    
//    println(lda.topicsSummary(50))
//    println()
//    
//    println("\nCentral words")
//    for (ti <- 0 until numTopics) {
//      print("Topic  "+ti+" ")
//      println(lda.topicWords(ti, 200).filter(s => s.startsWith("W=") && !s.contains("@")).take(20).mkString(" "))
//    }
      
  }

}