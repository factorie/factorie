package cc.factorie.example

import cc.factorie.app.nlp._
import cc.factorie.app.chain._
import cc.factorie.app.nlp.ner._
import cc.factorie.app.classify._
import cc.factorie.app.strings.Stopwords
import cc.factorie._
//import bp.optimized.{FullBeam, BeamSearch}
import ner.ChainNerLabel

object TokenLDA {
  
  object ChainNerFeaturesDomain extends CategoricalTensorDomain[String]
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
    for (token <- document.tokens) {
      val rawWord = token.string
      val word = simplifyDigits(rawWord).toLowerCase
      val features = token.attr += new ChainNerFeatures(token)
      features += "W="+word
      features += "SHAPE="+cc.factorie.app.strings.stringShape(rawWord, 2)
      features += "POS="+token.attr[cc.factorie.app.nlp.pos.PosLabel].categoryValue
      if (token.isPunctuation) features += "PUNCTUATION"
      for (lex <- lexicons) {
        if (lex.contains(token)) features += "lexicon::"+lex.hashCode()
      }
      if (lexicons.size > 0) {
        if (lexicons.head.contains(token)) token.attr += new LexiconLabel("TRUE", token)
        else token.attr += new LexiconLabel("NONE", token)
      }
    }
    for (sentence <- document.sentences)
      cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions(sentence.tokens, (t:Token)=>t.attr[ChainNerFeatures], List(0), List(1), List(2), List(-1), List(-2))
    // If the sentence contains no lowercase letters, tell all tokens in the sentence they are part of an uppercase sentence
    //document.sentences.foreach(s => if (!s.exists(_.containsLowerCase)) s.foreach(t => t.attr[ChainNerFeatures] += "SENTENCEUPPERCASE"))
    // Add features for character n-grams between sizes 2 and 5
    //document.foreach(t => if (t.string.matches("[A-Za-z]+")) t.attr[ChainNerFeatures] ++= t.charNGrams(2,5).map(n => "NGRAM="+n))
    // Add features from window of 4 words before and after
    document.tokens.foreach(t => t.attr[ChainNerFeatures] ++= t.prevWindow(6).dropRight(2).filter(t2 => t2.string.length > 1 && !Stopwords.contains(t2.string)).map(t2 => "PREVWINDOW="+simplifyDigits(t2.string).toLowerCase))
    document.tokens.foreach(t => t.attr[ChainNerFeatures] ++= t.nextWindow(6).drop(2).     filter(t2 => t2.string.length > 1 && !Stopwords.contains(t2.string)).map(t2 => "NEXTWINDOW="+simplifyDigits(t2.string).toLowerCase))
  }
  
  def main(args:Array[String]): Unit = {
    val numTopics = 100
    
    for (filename <- List("cities", "companies", "companysuffix", "countries", "days", "firstname.high", "firstname.highest", "firstname.med", "jobtitle", "lastname.high", "lastname.highest", "lastname.med", "months", "states")) {
      println("Reading lexicon "+filename)
      lexicons += new Lexicon("/Users/mccallum/research/data/resources/lexicons/"+filename)
    }
    
    val trainDocs = LoadConll2003.fromFilename("/Users/mccallum/research/data/ie/ner2003/eng.train")
    val testDocs = LoadConll2003.fromFilename("/Users/mccallum/research/data/ie/ner2003/eng.testb")
    val docs = (trainDocs ++ testDocs)
    docs.foreach(doc => initFeatures(doc))

    object WordSeqDomain extends CategoricalSeqDomain[String]
    val model = cc.factorie.generative.GenerativeModel()
    val lda = new cc.factorie.app.topics.lda.LDA(WordSeqDomain, numTopics)(model)

    var i = 0
    for (doc <- docs; token <- doc.tokens; if (token.isCapitalized)) {
      val ldaDoc = new cc.factorie.app.topics.lda.Document(WordSeqDomain, doc.name+":"+token.position+"::"+i, Nil)
      i += 1
      for (feature <- token.attr[ChainNerFeatures].activeCategories)
        ldaDoc.appendCategory(feature)
      for (i <- 1 to 5)
        if (token.nerLabel.categoryValue != "O")
          ldaDoc.appendCategory("LABEL="+token.nerLabel.categoryValue)
      lda.addDocument(ldaDoc)
    }

    println("\nRead "+lda.documents.size+" documents, "+WordSeqDomain.elementDomain.size+" word types, "+lda.documents.map(_.ws.length).sum+" word tokens.")
    lda.inferTopics(iterations = 100, fitAlphaInterval = 20, diagnosticInterval = 10)

    println(lda.topicsSummary(50))
    println()

    println("\nCentral words")
    for (ti <- 0 until numTopics) {
      print("Topic  "+ti+" ")
      println(lda.topicWords(ti, 200).filter(s => s.startsWith("W=") && !s.contains("@")).take(20).mkString(" "))
    }

    object ChainNerFeaturesDomain2 extends CategoricalTensorDomain[String]
    class ChainNerFeatures2(t: Token) extends BinaryFeatureVectorVariable[String] {
      def domain = ChainNerFeaturesDomain2
      override def skipNonCategories = true
    }

    docs.foreach(d => {
      d.tokens.foreach(t => {
        val feats = t.attr += new ChainNerFeatures2(t)
        t.attr[ChainNerFeatures].activeCategories.foreach(f => {
          if (!f.startsWith("PREV") && !f.startsWith("NEXT"))
            feats += f
        })
        val ldaDoc = new cc.factorie.app.topics.lda.Document(WordSeqDomain, d.name+":"+t.position+":2::"+i, Nil)
        i += 1
        for (feature <- t.attr[ChainNerFeatures].activeCategories)
          ldaDoc.appendCategory(feature)
        lda.inferDocumentTheta(ldaDoc, 2)
        ldaDoc.theta.tensor.toSeq.zipWithIndex.sortBy(t => -t._1).take(10).foreach(t => {
          feats += "topic:"+t._2
        })
      })
    })

    val transTemplate = new TemplateWithDotStatistics2[ChainNerLabel, ChainNerLabel]  {
       def statisticsDomains = Tuple(Conll2003NerDomain, Conll2003NerDomain) 
       factorName = "LabelLabelToken"
       override def unroll1(l: ChainNerLabel) = if (l.token.sentenceHasNext) List(Factor(l, l.token.sentenceNext.nerLabel)) else Nil
       override def unroll2(l: ChainNerLabel) = if (l.token.sentenceHasPrev) List(Factor(l.token.sentencePrev.nerLabel, l)) else Nil
     }
     val localTemplate = new TemplateWithDotStatistics2[ChainNerLabel, ChainNerFeatures2] {
       def statisticsDomains = Tuple(Conll2003NerDomain, ChainNerFeaturesDomain2)
       override def unroll1(l: ChainNerLabel) = List(Factor(l, l.token.attr[ChainNerFeatures2]))
       override def unroll2(t: ChainNerFeatures2) = throw new Error("Do not change the token variables")
     }
     val MyModel = new TemplateModel(transTemplate, localTemplate)

     val obj = new TemplateModel(new HammingLossTemplate[ChainNerLabel]())
     val tester = new VariableSettingsSampler[ChainNerLabel](MyModel)

     val trainLabels = trainDocs.flatMap(_.tokens.map(_.nerLabel))
     val testLabels = testDocs.flatMap(_.tokens.map(_.nerLabel))

     val d = trainLabels.head.domain
     MyModel.score(trainLabels.take(5))

     val nIter = 15
     val errors = (0 until nIter).map(i => {
       List(localTemplate, transTemplate).foreach(t => {
         t.freezeDomains
         var i = 0
         while (i < t.weights.length) {
           t.weights(i) = 0.0
           i += 1
         }
       })
       val learner2 = new VariableSettingsSampler[ChainNerLabel](MyModel, obj) with SampleRank with AROWUpdates
       learner2.processAll(trainLabels, 5)
       val ld = d.categories.filter(_.length > 2).map(_.substring(2))
       //val searcher = new BeamSearch with FullBeam
       //testDocs.foreach(d => d.sentences.foreach(s=>searcher.searchAndSetToMax(localTemplate, transTemplate, s.tokens.map(_.nerLabel))))
       val tee = new cc.factorie.app.chain.SegmentEvaluation[ChainNerLabel](ld)
       tee += testLabels.asInstanceOf[Seq[ChainNerLabel]]
       println(tee.f1)
       tee.f1
     })

     val s = errors.sum / nIter
     val s2 = errors.map(x => x*x).sum / nIter

     println("Test-set f1: "+s+"+-"+math.sqrt((s2-s*s)/nIter))


    
      
  }

}