package cc.factorie.app.nlp.ner
import cc.factorie._
import cc.factorie.app.nlp._
import java.io.File
import cc.factorie.util.{BinarySerializer, CubbieConversions}

/** A simple named entity recognizer, trained on CoNLL 2003 data.
    It does not have sufficient features to be state-of-the-art. */
class NER3 extends DocumentAnnotator {
  def this(filename: String) = { this(); deserialize(filename) }

  object FeaturesDomain extends CategoricalTensorDomain[String]
  class FeaturesVariable(val token:Token) extends BinaryFeatureVectorVariable[String] {
    def domain = FeaturesDomain
    override def skipNonCategories = true
  } 
  
  // The model
  val model1 = new TemplateModel with Parameters {
    // Bias term on each individual label
    val bias = this += new DotTemplateWithStatistics1[BilouConllNerLabel] {
      override def neighborDomain1 = BilouConllNerDomain
      val weights = Weights(new la.DenseTensor1(BilouConllNerDomain.size))
    }
    // Transition factors between two successive labels
    val markov = this += new DotTemplateWithStatistics2[BilouConllNerLabel, BilouConllNerLabel] {
      override def neighborDomain1 = BilouConllNerDomain
      override def neighborDomain2 = BilouConllNerDomain
      val weights = Weights(new la.DenseTensor2(BilouConllNerDomain.size, BilouConllNerDomain.size))
      def unroll1(label:BilouConllNerLabel) = if (label.token.sentenceHasPrev) Factor(label.token.prev.attr[BilouConllNerLabel], label) else Nil
      def unroll2(label:BilouConllNerLabel) = if (label.token.sentenceHasNext) Factor(label, label.token.next.attr[BilouConllNerLabel]) else Nil
    }
    // Factor between label and observed token
    val evidence = this += new DotTemplateWithStatistics2[BilouConllNerLabel, FeaturesVariable] {
      override def neighborDomain1 = BilouConllNerDomain
      override def neighborDomain2 = FeaturesDomain
      val weights = Weights(new la.DenseTensor2(BilouConllNerDomain.size, FeaturesDomain.dimensionSize))
      def unroll1(label:BilouConllNerLabel) = Factor(label, label.token.attr[FeaturesVariable])
      def unroll2(token:FeaturesVariable) = throw new Error("FeaturesVariable values shouldn't change")
    }
  }
  
  val model2 = new cc.factorie.app.chain.ChainModel[BilouConllNerLabel,FeaturesVariable,Token](BilouConllNerDomain, FeaturesDomain, l=>l.token.attr[FeaturesVariable], l=>l.token, t=>t.attr[BilouConllNerLabel])
  val model = model1
  
  
  // The training objective
  val objective = new HammingTemplate[BilouConllNerLabel]

  // Methods of DocumentAnnotator
  override def tokenAnnotationString(token:Token): String = token.attr[BilouConllNerLabel].categoryValue
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token])
  def postAttrs: Iterable[Class[_]] = List(classOf[BilouConllNerLabel])
  def process1(document:Document): Document = {
    if (document.tokenCount > 0) {
      val alreadyHadFeatures = document.hasAnnotation(classOf[FeaturesVariable])
      if (!alreadyHadFeatures) addFeatures(document)
      for (token <- document.tokens) if (token.attr[BilouConllNerLabel] eq null) token.attr += new BilouConllNerLabel(token, "O")
      for (sentence <- document.sentences if sentence.tokens.size > 0)
        BP.inferChainMax(sentence.tokens.map(_.attr[BilouConllNerLabel]).toSeq, model)
      if (!alreadyHadFeatures) { document.annotators.remove(classOf[FeaturesVariable]); for (token <- document.tokens) token.attr.remove[FeaturesVariable] }
    }
    document
  }
  
  // For words like Swedish & Swedes but not Sweden
  object Demonyms extends lexicon.PhraseLexicon { 
    for (line <- io.Source.fromInputStream(lexicon.WikipediaPerson.getClass.getResourceAsStream("iesl/demonyms.txt")).getLines) {
      val fields = line.trim.split(" ?\t ?") // TODO The currently checked in version has extra spaces in it; when this is fixed, use simply: ('\t')
      for (phrase <- fields.drop(1)) this += phrase
    }
  }
  
  // Feature creation
  def addFeatures(document:Document): Unit = {
    document.annotators(classOf[FeaturesVariable]) = this
    import cc.factorie.app.strings.simplifyDigits
    for (token <- document.tokens) {
      val features = new FeaturesVariable(token)
      token.attr += features
      val rawWord = token.string
      val word = simplifyDigits(rawWord).toLowerCase
      features += "W="+word
      features += "SHAPE="+cc.factorie.app.strings.stringShape(rawWord, 2)
      if (token.isPunctuation) features += "PUNCTUATION"
      if (lexicon.PersonFirst.containsLemmatizedWord(word)) features += "PERSON-FIRST"
      if (lexicon.Month.containsLemmatizedWord(word)) features += "MONTH"
      if (lexicon.PersonLast.containsLemmatizedWord(word)) features += "PERSON-LAST"
      if (lexicon.PersonHonorific.containsLemmatizedWord(word)) features += "PERSON-HONORIFIC"
      if (lexicon.Company.contains(token)) features += "COMPANY"
      if (lexicon.Country.contains(token)) features += "COUNTRY"
      if (lexicon.City.contains(token)) features += "CITY"
      if (lexicon.PlaceSuffix.contains(token)) features += "PLACE-SUFFIX"
      if (lexicon.USState.contains(token)) features += "USSTATE"
      if (lexicon.WikipediaPerson.contains(token)) features += "WIKI-PERSON"
      if (lexicon.WikipediaEvent.contains(token)) features += "WIKI-EVENT"
      if (lexicon.WikipediaLocation.contains(token)) features += "WIKI-LOCATION"
      if (lexicon.WikipediaOrganization.contains(token)) features += "WIKI-ORG"
      if (Demonyms.contains(token)) features += "DEMONYM"
    }
    //for (sentence <- document.sentences) cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions(sentence.tokens, (t:Token)=>t.attr[FeaturesVariable], List(0), List(0,0), List(0,0,-1), List(0,0,1), List(1), List(2), List(-1), List(-2))
    for (sentence <- document.sentences) cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions(sentence.tokens, (t:Token)=>t.attr[FeaturesVariable], List(0), List(1), List(2), List(-1), List(-2))

    for (token <- document.tokens) {
      val word = cc.factorie.app.strings.simplifyDigits(token.string).toLowerCase
      val features = token.attr[FeaturesVariable]
      if (word.length > 5) { features += "P="+cc.factorie.app.strings.prefix(word, 4); features += "S="+cc.factorie.app.strings.suffix(word, 4) }
    }

    // Add features from window of 4 words before and after
    document.tokens.foreach(t => t.attr[FeaturesVariable] ++= t.prevWindow(4).map(t2 => "PREVWINDOW="+simplifyDigits(t2.string).toLowerCase))
    document.tokens.foreach(t => t.attr[FeaturesVariable] ++= t.nextWindow(4).map(t2 => "NEXTWINDOW="+simplifyDigits(t2.string).toLowerCase))
    // Put features of first mention on later mentions
    document.tokens.foreach(t => {
      if (t.isCapitalized && t.string.length > 1 && !t.attr[FeaturesVariable].activeCategories.exists(f => f.matches(".*FIRSTMENTION.*"))) {
        //println("Looking for later mentions of "+t.word)
        var t2 = t
        while (t2.hasNext) {
          t2 = t2.next
          if (t2.string == t.string) { 
            //println("Adding FIRSTMENTION to "+t2.word); 
            t2.attr[FeaturesVariable] ++= t.attr[FeaturesVariable].activeCategories.map(f => "FIRSTMENTION="+f) // Only put the @ context features in, not the others.
          }
        }
      }
    })
    //for (section <- document.sections) cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions(section.tokens, (t:Token)=>t.attr[FeaturesVariable], Seq(0), Seq(-1), Seq(-2), Seq(1), Seq(2), Seq(0,0))
  }

  def tokenFeaturesString(tokens:Iterable[Token]): String = tokens.map(token => "%-20s  %s".format(token.string, token.attr[FeaturesVariable])).mkString("\n")
  
  def sampleOutputString(tokens:Iterable[Token]): String = {
    val sb = new StringBuffer
    for (token <- tokens) sb.append("%s %20s %10s %10s  %s\n".format(if (token.attr[BilouConllNerLabel].valueIsTarget) " " else "*", token.string, token.attr[BilouConllNerLabel].target.categoryValue, token.attr[BilouConllNerLabel].categoryValue, token.attr[FeaturesVariable]))
    sb.toString
  }

  // Parameter estimation
  def train(trainDocs:Iterable[Document], testDocs:Iterable[Document]): Unit = {
    def labels(docs:Iterable[Document]): Iterable[BilouConllNerLabel] = docs.flatMap(doc => doc.tokens.map(_.attr[BilouConllNerLabel]))
    trainDocs.foreach(addFeatures(_)); FeaturesDomain.freeze(); testDocs.foreach(addFeatures(_)) // Discovery features on training data only
    println(sampleOutputString(trainDocs.take(12).last.tokens.take(200)))
    val trainLabels = labels(trainDocs).toIndexedSeq
    val testLabels = labels(testDocs).toIndexedSeq
    //val trainer = new optimize.SampleRankTrainer(new IteratedConditionalModes(model, objective), new optimize.AdaGrad)
    val examples = trainDocs.flatMap(_.sentences.map(sentence => new optimize.LikelihoodExample(sentence.tokens.map(_.attr[BilouConllNerLabel]), model, InferByBPChainSum))).toSeq
    
//    // Make sure we are getting factors in order
//    for (sentence <- trainDocs.take(5).last.sentences) {
//      //val markovFactors = model1.markov.factors(sentence.tokens.map(_.attr[BilouConllNerLabel])).toSeq
//      val markovFactors = model1.factors(sentence.tokens.map(_.attr[BilouConllNerLabel])).filter({case f:Factor2[_,_] => f._1.isInstanceOf[BilouConllNerLabel] && f._2.isInstanceOf[BilouConllNerLabel]; case _  => false }).toSeq
//      println("Sentence length %d  markovFactors length %d".format(sentence.length, markovFactors.length))
//      //assert(markovFactors.length > 5, "length="+markovFactors.length)
//      assert(markovFactors.length < 2 || markovFactors.sliding(2).forall(fs => fs(0).asInstanceOf[Factor2[_,_]]._2 == fs(1).asInstanceOf[Factor2[_,_]]._1))
//    }
    
    //val trainer = new optimize.OnlineTrainer(model.parameters, new optimize.AdaGradRDA(l1=0.1/examples.length)) // L2RegularizedConcentrate or AdaMIRA (gives smaller steps)
    val trainer = new optimize.OnlineTrainer(model.parameters)
    for (iteration <- 1 until 3) { 
      //trainer.processContexts(trainLabels)
      trainer.processExamples(examples)
      trainDocs.foreach(process(_)); println("Train accuracy "+objective.accuracy(trainLabels))
      testDocs.foreach(process(_));  println("Test  accuracy "+objective.accuracy(testLabels))
      println(new app.chain.SegmentEvaluation[BilouConllNerLabel]("(B|U)-", "(I|L)-", BilouConllNerDomain, testLabels.toIndexedSeq))
    }
    val trainer2 = new optimize.BatchTrainer(model.parameters)
    while (!trainer2.isConverged) {
      trainer2.processExamples(examples)
      trainDocs.foreach(process(_)); println("Train accuracy "+objective.accuracy(trainLabels))
      testDocs.foreach(process(_));  println("Test  accuracy "+objective.accuracy(testLabels))
      println(new app.chain.SegmentEvaluation[BilouConllNerLabel]("(B|U)-", "(I|L)-", BilouConllNerDomain, testLabels.toIndexedSeq))
    }
    new java.io.PrintStream(new File("ner3-test-output")).print(sampleOutputString(testDocs.flatMap(_.tokens)))
  }
  def train(trainFilename:String, testFilename:String): Unit = {
    // TODO Make fixLabels no longer necessary by having LoadConll2003 directly create the right label type.
    def fixLabels(docs:Iterable[Document]): Iterable[Document] = { for (doc <- docs; token <- doc.tokens) token.attr += new BilouConllNerLabel(token, token.attr[NerLabel].categoryValue); docs }
    val trainDocs = fixLabels(LoadConll2003(BILOU=true).fromFilename(trainFilename))
    val testDocs = fixLabels(LoadConll2003(BILOU=true).fromFilename(testFilename))
    train(trainDocs, testDocs)
  }
  
  // Serialization
  def serialize(filename: String) {
    import CubbieConversions._
    val file = new File(filename); if (file.getParentFile eq null) file.getParentFile.mkdirs()
    BinarySerializer.serialize(FeaturesDomain.dimensionDomain, model, file)
  }
  def deserialize(filename: String) {
    import CubbieConversions._
    val file = new File(filename)
    assert(file.exists(), "Trying to load non-existent file: '" +file)
    BinarySerializer.deserialize(FeaturesDomain.dimensionDomain, model, file)
  }
}

object NER3 {
  def main(args:Array[String]): Unit = {
    val ner = new NER3
    ner.train(args(0), args(1))
    ner.serialize(args(2))
  }
}
