package cc.factorie.app.nlp.ner
import cc.factorie._
import cc.factorie.app.nlp._
import java.io.File
import cc.factorie.util.{LogUniformDoubleSampler, CmdOptions, BinarySerializer, CubbieConversions}
import scala.concurrent.Await

/** A simple named entity recognizer, trained on CoNLL 2003 data.
    It does not have sufficient features to be state-of-the-art. */
class NER3 extends DocumentAnnotator {
  def this(url:java.net.URL) = { this(); deserialize(url.openConnection.getInputStream) }

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
      for (sentence <- document.sentences if sentence.length > 0)
        BP.inferChainMax(sentence.tokens.map(_.attr[BilouConllNerLabel]).toSeq, model)
      if (!alreadyHadFeatures) { document.annotators.remove(classOf[FeaturesVariable]); for (token <- document.tokens) token.attr.remove[FeaturesVariable] }
    }
    document
  }
  
  // For words like Swedish & Swedes but not Sweden
  object Demonyms extends lexicon.PhraseLexicon("iesl/demonyms") { 
    for (line <- io.Source.fromInputStream(lexicon.ClasspathResourceLexicons.getClass.getResourceAsStream("iesl/demonyms.txt")).getLines) {
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
      if (lexicon.iesl.PersonFirst.containsLemmatizedWord(word)) features += "PERSON-FIRST"
      if (lexicon.iesl.Month.containsLemmatizedWord(word)) features += "MONTH"
      if (lexicon.iesl.PersonLast.containsLemmatizedWord(word)) features += "PERSON-LAST"
      if (lexicon.iesl.PersonHonorific.containsLemmatizedWord(word)) features += "PERSON-HONORIFIC"
      if (lexicon.iesl.Company.contains(token)) features += "COMPANY"
      if (lexicon.iesl.Country.contains(token)) features += "COUNTRY"
      if (lexicon.iesl.City.contains(token)) features += "CITY"
      if (lexicon.iesl.PlaceSuffix.contains(token)) features += "PLACE-SUFFIX"
      if (lexicon.iesl.USState.contains(token)) features += "USSTATE"
      if (lexicon.wikipedia.Person.contains(token)) features += "WIKI-PERSON"
      if (lexicon.wikipedia.Event.contains(token)) features += "WIKI-EVENT"
      if (lexicon.wikipedia.Location.contains(token)) features += "WIKI-LOCATION"
      if (lexicon.wikipedia.Organization.contains(token)) features += "WIKI-ORG"
      if (Demonyms.contains(token)) features += "DEMONYM"
    }
    //for (sentence <- document.sentences) cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions(sentence.tokens, (t:Token)=>t.attr[FeaturesVariable], List(0), List(0,0), List(0,0,-1), List(0,0,1), List(1), List(2), List(-1), List(-2))
    //for (sentence <- document.sentences) cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions(sentence.tokens, (t:Token)=>t.attr[FeaturesVariable], List(0), List(1), List(2), List(-1), List(-2))
    for (sentence <- document.sentences) cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions(sentence.tokens, (t:Token)=>t.attr[FeaturesVariable], List(0), List(1), List(2), List(-1), List(-2), List(0,-1), List(0,1))

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
  def train(trainDocs:Iterable[Document], testDocs:Iterable[Document], l1: Double, l2: Double): Double = {
    def labels(docs:Iterable[Document]): Iterable[BilouConllNerLabel] = docs.flatMap(doc => doc.tokens.map(_.attr[BilouConllNerLabel]))
    trainDocs.foreach(addFeatures(_)); FeaturesDomain.freeze(); testDocs.foreach(addFeatures(_)) // Discovery features on training data only
    println(sampleOutputString(trainDocs.take(12).last.tokens.take(200)))
    val trainLabels = labels(trainDocs).toIndexedSeq
    val testLabels = labels(testDocs).toIndexedSeq
    model.limitDiscreteValuesAsIn(trainLabels)
    val examples = trainDocs.flatMap(_.sentences.filter(_.length > 1).map(sentence => new optimize.LikelihoodExample(sentence.tokens.map(_.attr[BilouConllNerLabel]), model, InferByBPChainSum))).toSeq
    val trainer = new optimize.OnlineTrainer(model.parameters, new optimize.AdaGradRDA(l1=l1/examples.length, l2=l2/examples.length)) // L2RegularizedConcentrate or AdaMIRA (gives smaller steps)
    //val trainer = new optimize.OnlineTrainer(model.parameters)
    for (iteration <- 1 until 4) { 
      trainer.processExamples(examples)
      trainDocs.foreach(process(_)); println("Train accuracy "+objective.accuracy(trainLabels))
      testDocs.foreach(process(_));  println("Test  accuracy "+objective.accuracy(testLabels))
      println(new app.chain.SegmentEvaluation[BilouConllNerLabel]("(B|U)-", "(I|L)-", BilouConllNerDomain, testLabels.toIndexedSeq))
      println(model.parameters.tensors.sumInts(t => t.toSeq.count(x => x == 0)).toFloat/model.parameters.tensors.sumInts(_.length)+" sparsity")
    }
    return new app.chain.SegmentEvaluation[BilouConllNerLabel]("(B|U)-", "(I|L)-", BilouConllNerDomain, testLabels.toIndexedSeq).f1
    val trainer2 = new optimize.BatchTrainer(model.parameters, new optimize.LBFGS with optimize.L2Regularization { variance = 10.0 })
    while (!trainer2.isConverged) {
      trainer2.processExamples(examples)
      trainDocs.foreach(process(_)); println("Train accuracy "+objective.accuracy(trainLabels))
      testDocs.foreach(process(_));  println("Test  accuracy "+objective.accuracy(testLabels))
      println(new app.chain.SegmentEvaluation[BilouConllNerLabel]("(B|U)-", "(I|L)-", BilouConllNerDomain, testLabels.toIndexedSeq))
    }
    new java.io.PrintStream(new File("ner3-test-output")).print(sampleOutputString(testDocs.flatMap(_.tokens)))
    new app.chain.SegmentEvaluation[BilouConllNerLabel]("(B|U)-", "(I|L)-", BilouConllNerDomain, testLabels.toIndexedSeq).f1
  }
  def train(trainFilename:String, testFilename:String, l1: Double=0.001, l2: Double=0.001): Double = {
    // TODO Make fixLabels no longer necessary by having LoadConll2003 directly create the right label type.
    def fixLabels(docs:Iterable[Document]): Iterable[Document] = { for (doc <- docs; token <- doc.tokens) token.attr += new BilouConllNerLabel(token, token.attr[NerLabel].categoryValue); docs }
    val trainDocs = fixLabels(LoadConll2003(BILOU=true).fromFilename(trainFilename))
    val testDocs = fixLabels(LoadConll2003(BILOU=true).fromFilename(testFilename))
    train(trainDocs, testDocs, l1, l2)
  }
  
  // Serialization
  def serialize(filename: String): Unit = {
    val file = new File(filename); if (file.getParentFile eq null) file.getParentFile.mkdirs()
    serialize(new java.io.FileOutputStream(file))
  }
  def deserialize(file: File): Unit = {
    require(file.exists(), "Trying to load non-existent file: '" +file)
    deserialize(new java.io.FileInputStream(file))
  }
  def serialize(stream: java.io.OutputStream): Unit = {
    import CubbieConversions._
    val dstream = new java.io.DataOutputStream(stream)
    BinarySerializer.serialize(FeaturesDomain.dimensionDomain, dstream)
    BinarySerializer.serialize(model, dstream)
    dstream.close()  // TODO Are we really supposed to close here, or is that the responsibility of the caller
  }
  def deserialize(stream: java.io.InputStream): Unit = {
    import CubbieConversions._
    val dstream = new java.io.DataInputStream(stream)
    BinarySerializer.deserialize(FeaturesDomain.dimensionDomain, dstream)
    BinarySerializer.deserialize(model, dstream)
    model.parameters.densify()
    dstream.close()  // TODO Are we really supposed to close here, or is that the responsibility of the caller
  }
}

object NER3 extends cc.factorie.util.HyperparameterMain {
  class Opts extends CmdOptions {
    val train = new CmdOption("train", "eng.train", "STRING", "Training file")
    val test = new CmdOption("test", "eng.testa", "STRING", "Testing file")
    val l1 = new CmdOption("l1", 0.001, "DOUBLE", "L1 regularization")
    val l2 = new CmdOption("l2", 0.001, "DOUBLE", "L2 regularization")
    val serialize = new CmdOption("serialize", false, "BOOLEAN", "Whether to serialize the model")
    val modelFile = new CmdOption("model", "", "STRING", "File to save the model in")
  }
  def evaluateParameters(args:Array[String]): Double = {
    object opts extends Opts
    opts.parse(args)
    val ner = new NER3
    val res = ner.train(opts.train.value, opts.test.value, opts.l1.value, opts.l2.value)
    if (opts.serialize.value) {
      ner.model.parameters.sparsify()
      ner.serialize(args(2))
    }
    res
  }
}

object NER3Validator {
  def main(args: Array[String]) {
    val opts = new NER3.Opts
    opts.parse(args)
    val l1 = cc.factorie.util.HyperParameter(opts.l1, new LogUniformDoubleSampler(1e-6, 10))
    val l2 = cc.factorie.util.HyperParameter(opts.l2, new LogUniformDoubleSampler(1e-6, 10))
    opts.serialize.setValue(false)
    val qs = new cc.factorie.util.QSubExecutor(10, "cc.factorie.app.nlp.ner.NER3", "try-log/")
    val optimizer = new cc.factorie.util.HyperParameterSearcher(opts, Seq(l1, l2), qs.execute, 50, 40, 60)
    val result = optimizer.optimize()
    println("Got results: " + result.mkString(" "))
    println("Best l1: " + opts.l1.value + " best l2: " + opts.l2.value)
    opts.serialize.setValue(true)
    println("Running best configuration...")
    import scala.concurrent.duration._
    Await.result(qs.execute(opts.values.map(_.unParse).toArray), 1 hours)
    println("Done.")
  }
}