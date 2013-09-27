package cc.factorie.app.nlp.ner
import cc.factorie._
import model._
import variable._
import cc.factorie.app.nlp._
import java.io.File
import cc.factorie.util.{BinarySerializer, CubbieConversions}
import cc.factorie.optimize.{Trainer, LikelihoodExample}
import cc.factorie.app.nlp.segment.PlainNormalizedTokenString
import cc.factorie.infer.{InferByBPChain, DiscreteProposalMaximizer, MaximizeByBPChain}
import cc.factorie.variable.{BinaryFeatureVectorVariable, CategoricalVectorDomain, DiscreteVar}
import cc.factorie.model.{DotTemplateWithStatistics2, TemplateModel, DotTemplate2}

/** A simple named entity recognizer, trained on Ontonotes data.
    It does not have sufficient features to be state-of-the-art. */
class NER2 extends DocumentAnnotator {
  def this(url:java.net.URL) = {
    this()
    println("NER2 loading from "+url)
    deserialize(url.openConnection.getInputStream)
  }

  object FeaturesDomain extends CategoricalVectorDomain[String]
  class FeaturesVariable(val token:Token) extends BinaryFeatureVectorVariable[String] {
    def domain = FeaturesDomain
    override def skipNonCategories = true
  }
  
  class Model1 extends TemplateModel with cc.factorie.model.Parameters {
    val evidence = this += new DotTemplateWithStatistics2[BilouOntonotesNerLabel, FeaturesVariable] {
      val weights = Weights(new la.DenseTensor2(BilouOntonotesNerDomain.size, FeaturesDomain.dimensionSize))
      def unroll1(label:BilouOntonotesNerLabel) = Factor(label, label.token.attr[FeaturesVariable])
      def unroll2(token:FeaturesVariable) = throw new Error("FeaturesVariable values shouldn't be inferred.")
    }
  }
  object model1 extends Model1
  
  // The forward model
  class Model2 extends TemplateModel with Parameters {
    // Bias term on each individual label
    val bias = this += new DotTemplateWithStatistics1[BilouOntonotesNerLabel] {
      val weights = Weights(new la.DenseTensor1(BilouOntonotesNerDomain.size))
    }
    // Transition factors between two successive labels
//    val markov3 = this += new DotTemplateWithStatistics3[BilouOntonotesNerLabel, BilouOntonotesNerLabel,BilouOntonotesNerLabel] {
//      val weights = Weights(new la.DenseTensor3(BilouOntonotesNerDomain.size, BilouOntonotesNerDomain.size, BilouOntonotesNerDomain.size))
//      //val docStartLabel = new BilouOntonotesNerLabel(null, "O")  // TODO consider this, if we want to have this factor from the beginning of the document
//      def unroll1(label:BilouOntonotesNerLabel) = Nil //if (label.token.hasNext(2) Factor(label, label.token.next.attr[BilouOntonotesNerLabel], label.token.next.next.attr[BilouOntonotesNerLabel]) else Nil // Make this feedforward
//      def unroll2(label:BilouOntonotesNerLabel) = Nil //if (label.token.hasNext && label.token.hasPrev) Factor(label.token.prev.attr[BilouOntonotesNerLabel], label, label.token.next.attr[BilouOntonotesNerLabel]) else Nil // Make this feedforward
//      def unroll3(label:BilouOntonotesNerLabel) = if (label.token.hasPrev(2)) Factor(label.token.prev.prev.attr[BilouOntonotesNerLabel], label.token.prev.attr[BilouOntonotesNerLabel], label) else Nil
//    }
    val markovPrev1 = this += new DotTemplateWithStatistics2[BilouOntonotesNerLabel,BilouOntonotesNerLabel] {
      val weights = Weights(new la.DenseTensor2(BilouOntonotesNerDomain.size, BilouOntonotesNerDomain.size))
      //val docStartLabel = new BilouOntonotesNerLabel(null, "O")  // TODO consider this, if we want to have this factor from the beginning of the document
      def unroll1(label:BilouOntonotesNerLabel) = Nil //if (label.token.hasNext(2) Factor(label, label.token.next.attr[BilouOntonotesNerLabel], label.token.next.next.attr[BilouOntonotesNerLabel]) else Nil // Make this feedforward
      def unroll2(label:BilouOntonotesNerLabel) = if (label.token.hasPrev) Factor(label.token.prev.attr[BilouOntonotesNerLabel], label) else Nil
    }
    val markovPrev2 = this += new DotTemplateWithStatistics2[BilouOntonotesNerLabel,BilouOntonotesNerLabel] {
      val weights = Weights(new la.DenseTensor2(BilouOntonotesNerDomain.size, BilouOntonotesNerDomain.size))
      //val docStartLabel = new BilouOntonotesNerLabel(null, "O")  // TODO consider this, if we want to have this factor from the beginning of the document
      def unroll1(label:BilouOntonotesNerLabel) = Nil //if (label.token.hasNext(2) Factor(label, label.token.next.attr[BilouOntonotesNerLabel], label.token.next.next.attr[BilouOntonotesNerLabel]) else Nil // Make this feedforward
      def unroll2(label:BilouOntonotesNerLabel) = if (label.token.hasPrev(2)) Factor(label.token.prev.prev.attr[BilouOntonotesNerLabel], label) else Nil
    }
    val markovNext1 = this += new DotTemplateWithStatistics2[BilouOntonotesNerLabel,BilouOntonotesNerLabel] {
      val weights = Weights(new la.DenseTensor2(BilouOntonotesNerDomain.size, BilouOntonotesNerDomain.size))
      //val docStartLabel = new BilouOntonotesNerLabel(null, "O")  // TODO consider this, if we want to have this factor from the beginning of the document
      def unroll1(label:BilouOntonotesNerLabel) = if (label.token.hasNext) Factor(label, label.token.next.attr[BilouOntonotesNerLabel]) else Nil
      def unroll2(label:BilouOntonotesNerLabel) = Nil
    }
    val markovNext2 = this += new DotTemplateWithStatistics2[BilouOntonotesNerLabel,BilouOntonotesNerLabel] {
      val weights = Weights(new la.DenseTensor2(BilouOntonotesNerDomain.size, BilouOntonotesNerDomain.size))
      //val docStartLabel = new BilouOntonotesNerLabel(null, "O")  // TODO consider this, if we want to have this factor from the beginning of the document
      def unroll1(label:BilouOntonotesNerLabel) = if (label.token.hasNext(2)) Factor(label, label.token.next.next.attr[BilouOntonotesNerLabel]) else Nil
      def unroll2(label:BilouOntonotesNerLabel) = Nil
    }
    // Factor between label and observed token
    val evidence = this += new DotTemplateWithStatistics2[BilouOntonotesNerLabel, FeaturesVariable] {
      val weights = Weights(new la.DenseTensor2(BilouOntonotesNerDomain.size, FeaturesDomain.dimensionSize))
      def unroll1(label:BilouOntonotesNerLabel) = Factor(label, label.token.attr[FeaturesVariable])
      def unroll2(token:FeaturesVariable) = throw new Error("FeaturesVariable values shouldn't be inferred.")
    }
    // Factor between this label and the previous label with a Token having the same spelling, with the statistics comparing the label values with the BILOU prefixes removed
    val history = this += new DotTemplate2[BilouOntonotesNerLabel,BilouOntonotesNerLabel] {
      val weights = Weights(new la.DenseTensor2(OntonotesNerDomain.size, OntonotesNerDomain.size))
      override def statistics(v1:BilouOntonotesNerLabel#Value, v2:BilouOntonotesNerLabel#Value): la.Tensor = 
        OntonotesNerDomain(BilouOntonotesNerDomain.bilouSuffixIntValue(v1.intValue)) outer  OntonotesNerDomain(BilouOntonotesNerDomain.bilouSuffixIntValue(v2.intValue))
      def unroll1(label:BilouOntonotesNerLabel) = Nil
      def unroll2(label:BilouOntonotesNerLabel) = predictionHistory.mostFrequentLabel(label.token) match { case l:BilouOntonotesNerLabel => Factor(l, label); case _ => Nil }
    }
  }
  object model2 extends Model2
  
  class Model3 extends TemplateModel with Parameters {
    // Bias term on each individual label
    val bias = this += new DotTemplateWithStatistics1[BilouOntonotesNerLabel] {
      val weights = Weights(new la.DenseTensor1(BilouOntonotesNerDomain.size))
    }
    // Factor on transitions
    val markov = this += new DotTemplateWithStatistics2[BilouOntonotesNerLabel,BilouOntonotesNerLabel] {
      val weights = Weights(new la.DenseTensor2(BilouOntonotesNerDomain.size, BilouOntonotesNerDomain.size))
      def unroll1(label:BilouOntonotesNerLabel) = if (label.token.hasNext) Factor(label, label.token.next.attr[BilouOntonotesNerLabel]) else Nil
      def unroll2(label:BilouOntonotesNerLabel) = if (label.token.hasPrev) Factor(label.token.prev.attr[BilouOntonotesNerLabel], label) else Nil
    }
    // Factor between label and observed token
    val evidence = this += new DotTemplateWithStatistics2[BilouOntonotesNerLabel, FeaturesVariable] {
      val weights = Weights(new la.DenseTensor2(BilouOntonotesNerDomain.size, FeaturesDomain.dimensionSize))
      def unroll1(label:BilouOntonotesNerLabel) = Factor(label, label.token.attr[FeaturesVariable])
      def unroll2(token:FeaturesVariable) = throw new Error("FeaturesVariable values shouldn't be inferred.")
    }
  }
  object model3 extends Model3
  var mainModel: Model = model3

  // The training objective
  val objective = HammingObjective

  // Methods of DocumentAnnotator
  override def tokenAnnotationString(token:Token): String = token.attr[BilouOntonotesNerLabel].categoryValue
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token], classOf[segment.PlainNormalizedTokenString])
  def postAttrs: Iterable[Class[_]] = List(classOf[BilouOntonotesNerLabel])
  def process(document:Document): Document = {
    if (document.tokenCount > 0) {
      val alreadyHadFeatures = document.hasAnnotation(classOf[FeaturesVariable])
      if (!alreadyHadFeatures) addFeatures(document)
      for (token <- document.tokens) if (token.attr[BilouOntonotesNerLabel] eq null) token.attr += new BilouOntonotesNerLabel(token, "O")
      mainModel match {
        case model:Model1 => indepedentPredictDocument(document)
        case model:Model2 => forwardPredictDocument(document)
        case model:Model3 => bpPredictDocument(document)
      }
      if (!alreadyHadFeatures) { document.annotators.remove(classOf[FeaturesVariable]); for (token <- document.tokens) token.attr.remove[FeaturesVariable] }
    }
    document
  }
  
  // Prediction history
  val predictionHistory = new HashedTokenQueue(200)

  // Predict using model2, assuming that model1 has already been run
  def forwardPredictToken(token:Token): Unit = {
    // This assumes that model1 has already been run on all tokens, so we get reasonable predictions on forward labels.
    val label = token.attr[BilouOntonotesNerLabel]
    variable.MaximizeDiscrete(label, model2)
    predictionHistory += token
  }
  // Predict using model1 only
  def indepedentPredictDocument(document:Document): Unit = {
    for (token <- document.tokens) variable.MaximizeDiscrete(token.attr[BilouOntonotesNerLabel], model1)
  }  
  // Predict using model1 and then model2
  def forwardPredictDocument(document:Document): Unit = {
    //predictionHistory.clear()
    indepedentPredictDocument(document)
    for (token <- document.tokens) forwardPredictToken(token)
    //predictionHistory.clear()
  }
  def bpPredictDocument(document:Document): Unit = {
    for (sentence <- document.sentences) MaximizeByBPChain(sentence.tokens.toIndexedSeq.map(_.attr[BilouOntonotesNerLabel]), model3)
  }
 
  
  // Feature creation
  def addFeatures(document:Document): Unit = {
    val cow = cc.factorie.app.nlp.lexicon.iesl
    document.annotators(classOf[FeaturesVariable]) = this.getClass
    import cc.factorie.app.strings.simplifyDigits
    for (section <- document.sections; token <- section.tokens) {
      val features = new FeaturesVariable(token)
      token.attr += features
      val rawWord = token.string
      val rawWordLength = rawWord.length
      val word = simplifyDigits(rawWord).toLowerCase
      val shape = cc.factorie.app.strings.stringShape(rawWord, 2)
      features += "W="+word
      features += "SHAPE="+shape
      features += "WS="+word+"&"+shape // word conjoined with shape
      if (word.length > 5) { features += "P="+cc.factorie.app.strings.prefix(word, 4); features += "S="+cc.factorie.app.strings.suffix(word, 4) }
      if (token.isPunctuation) features += "PUNCTUATION"
      if (lexicon.NumberWords.containsLemmatizedWord(word)) features += "#WORD"
      if (lexicon.iesl.Money.containsLemmatizedWord(word)) features += "MONEY"
      if (lexicon.iesl.PersonFirst.containsLemmatizedWord(word)) features += "PERSON-FIRST"
      if (lexicon.iesl.Month.containsLemmatizedWord(word)) features += "MONTH"
      if (lexicon.iesl.PersonLast.containsLemmatizedWord(word)) features += "PERSON-LAST"
      if (lexicon.iesl.PersonHonorific.containsLemmatizedWord(word)) features += "PERSON-HONORIFIC"
      if (lexicon.iesl.Company.contains(token)) features += "COMPANY"
      if (lexicon.iesl.Country.contains(token)) features += "COUNTRY"
      if (lexicon.iesl.City.contains(token)) features += "CITY"
      if (lexicon.iesl.PlaceSuffix.contains(token)) features += "PLACE-SUFFIX"
      if (lexicon.iesl.USState.contains(token)) features += "USSTATE"
      //features ++= token.prevWindow(4).map(t2 => "PREVWINDOW="+simplifyDigits(t2.string).toLowerCase)
      //features ++= token.nextWindow(4).map(t2 => "NEXTWINDOW="+simplifyDigits(t2.string).toLowerCase)
    }
    for (section <- document.sections)
      cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions(section.tokens, (t:Token)=>t.attr[FeaturesVariable], Seq(0), Seq(-1), Seq(-2), Seq(1), Seq(2))
  }
  def tokenFeaturesString(tokens:Iterable[Token]): String = tokens.map(token => "%-20s  %s".format(token.string, token.attr[FeaturesVariable])).mkString("\n")
  
  def sampleOutputString(tokens:Iterable[Token]): String = {
    val sb = new StringBuffer
    //for (token <- tokens) sb.append("%20s %-20s  %10s %10s\n".format(token.string, token.lemmaString, token.attr[BilouOntonotesNerLabel].target.categoryValue, token.attr[BilouOntonotesNerLabel].categoryValue))
    for (token <- tokens) sb.append("%s %20s %10s %10s  %s\n".format(if (token.attr[BilouOntonotesNerLabel].valueIsTarget) " " else "*", token.string, token.attr[BilouOntonotesNerLabel].target.categoryValue, token.attr[BilouOntonotesNerLabel].categoryValue, token.attr[FeaturesVariable]))
    sb.toString
  }
  
  def segmentEvaluationString(labels:IndexedSeq[BilouOntonotesNerLabel]): String = {
    val se = new app.chain.SegmentEvaluation[BilouOntonotesNerLabel]("(B|U)-", "(I|L)-", BilouOntonotesNerDomain)
    se += labels
    se.toString
  }
  
  def trainPrep(trainDocs:Iterable[Document], testDocs:Iterable[Document]): Unit = {
    def labels(docs:Iterable[Document]): Iterable[BilouOntonotesNerLabel] = docs.flatMap(doc => doc.tokens.map(_.attr[BilouOntonotesNerLabel]))
    println("Adding training features")
    trainDocs.foreach(addFeatures(_)) // Initialize all features to get parameters size
    FeaturesDomain.freeze()
    println("Applying features to test data")
    testDocs.foreach(addFeatures(_))
    println(tokenFeaturesString(trainDocs.head.tokens.take(100)))
    println("Training with %d features.".format(FeaturesDomain.dimensionSize))
  }
  
  def trainModel1(trainDocs:Iterable[Document], testDocs:Iterable[Document])(implicit random: scala.util.Random): Unit = {
    // This depends on calling trainPrep alrady
    def labels(docs:Iterable[Document]): Iterable[BilouOntonotesNerLabel] = docs.flatMap(doc => doc.tokens.map(_.attr[BilouOntonotesNerLabel]))
    def predict(labels:Iterable[BilouOntonotesNerLabel]): Unit = for (label <- labels) variable.MaximizeDiscrete(label, model1)
    val examples = labels(trainDocs).map(label => new optimize.DiscreteLikelihoodExample(label, model1))
    def evaluate() {
      (trainDocs ++ testDocs).foreach(indepedentPredictDocument(_))
      println("Some model1 training data"); println(sampleOutputString(trainDocs.head.tokens.drop(200).take(200)))
      println("Some model1 testing data"); println(sampleOutputString(testDocs.head.tokens.drop(200).take(200)))
      println("Train accuracy "+objective.accuracy(labels(trainDocs)))
      println("Test  accuracy "+objective.accuracy(labels(testDocs)))
    }
    Trainer.onlineTrain(model1.parameters, examples.toSeq, maxIterations=1, evaluate=evaluate)
  }
  
  // Parameter estimation
  def trainModel2(trainDocs:Iterable[Document], testDocs:Iterable[Document])(implicit random: scala.util.Random): Unit = {
    def labels(docs:Iterable[Document]): Iterable[BilouOntonotesNerLabel] = docs.flatMap(doc => doc.tokens.map(_.attr[BilouOntonotesNerLabel]))
    trainPrep(trainDocs, testDocs)
    // Model2 depends on Model1, so train Model1 also
    trainModel1(trainDocs, testDocs)
    val predictor = new DiscreteProposalMaximizer(model2, objective) {
      override def process1(context:DiscreteVar): DiffList = {
        val result = super.process1(context)
        val label = context.asInstanceOf[BilouOntonotesNerLabel]
        predictionHistory += label.token
        result
      }
    }
    val learner = new optimize.SampleRankTrainer(predictor, new optimize.AdaGrad)
    for (iteration <- 1 until 4) {
      trainDocs.foreach(indepedentPredictDocument(_))
      learner.processContexts(labels(trainDocs))
      trainDocs.foreach(process(_)); println("Train accuracy "+objective.accuracy(labels(trainDocs)))
      testDocs.foreach(process(_));  println("Test  accuracy "+objective.accuracy(labels(testDocs)))
      println("Some training data"); println(sampleOutputString(trainDocs.head.tokens.drop(iteration*100).take(100)))
      println("Some testing data"); println(sampleOutputString(testDocs.head.tokens.drop(iteration*100).take(100)))
      println("Train accuracy "+objective.accuracy(labels(trainDocs)))
      println(segmentEvaluationString(labels(trainDocs).toIndexedSeq))
      println("Test  accuracy "+objective.accuracy(labels(testDocs)))
      println(segmentEvaluationString(labels(testDocs).toIndexedSeq))
    }
    new java.io.PrintStream(new File("ner2-test-output")).print(sampleOutputString(testDocs.head.tokens))
  }
  
  def trainModel3(trainDocs:Iterable[Document], testDocs:Iterable[Document])(implicit random: scala.util.Random): Unit = {
    def labels(docs:Iterable[Document]): Iterable[BilouOntonotesNerLabel] = docs.flatMap(doc => doc.tokens.map(_.attr[BilouOntonotesNerLabel]))
    trainPrep(trainDocs, testDocs)
    val labelChains = for (document <- trainDocs; sentence <- document.sentences) yield sentence.tokens.map(_.attr[BilouOntonotesNerLabel])
    val examples = labelChains.par.map(v => new LikelihoodExample(v, model3, InferByBPChain)).seq.toSeq
    def evaluate() {
      trainDocs.par.foreach(process(_)); println("Train accuracy "+objective.accuracy(labels(trainDocs)))
      testDocs.par.foreach(process(_));  println("Test  accuracy "+objective.accuracy(labels(testDocs)))
      println("Some training data"); println(sampleOutputString(trainDocs.head.tokens.drop(100).take(100)))
      println("Some testing data"); println(sampleOutputString(testDocs.head.tokens.drop(100).take(100)))
      println("Train accuracy "+objective.accuracy(labels(trainDocs)))
      println(segmentEvaluationString(labels(trainDocs).toIndexedSeq))
      println("Test  accuracy "+objective.accuracy(labels(testDocs)))
      println(segmentEvaluationString(labels(testDocs).toIndexedSeq))
    }
    Trainer.onlineTrain(model3.parameters, examples, evaluate=evaluate)
    new java.io.PrintStream(new File("ner2-test-output")).print(sampleOutputString(testDocs.head.tokens))
  }
  
  def train(trainFilename:String, testFilename:String)(implicit random: scala.util.Random): Unit = {
    val trainDocs = cc.factorie.app.nlp.load.LoadOntonotes5.fromFilename(trainFilename, nerBilou=true)
    val testDocs = cc.factorie.app.nlp.load.LoadOntonotes5.fromFilename(testFilename, nerBilou=true)
    mainModel match {
      case model:Model2 => trainModel2(trainDocs, testDocs)
      case model:Model3 => trainModel3(trainDocs, testDocs)
    }
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
    val sparseEvidenceWeights = new la.DenseLayeredTensor2(BilouOntonotesNerDomain.size, FeaturesDomain.dimensionDomain.size, new la.SparseIndexedTensor1(_))
    model3.evidence.weights.value.foreachElement((i, v) => if (v != 0.0) sparseEvidenceWeights += (i, v))
    model3.evidence.weights.set(sparseEvidenceWeights)
    val dstream = new java.io.DataOutputStream(stream)
    BinarySerializer.serialize(FeaturesDomain.dimensionDomain, dstream)
    BinarySerializer.serialize(model3, dstream)
    dstream.close()  // TODO Are we really supposed to close here, or is that the responsibility of the caller
  }
  def deserialize(stream: java.io.InputStream): Unit = {
    import CubbieConversions._
    val dstream = new java.io.DataInputStream(stream)
    BinarySerializer.deserialize(FeaturesDomain.dimensionDomain, dstream)
    model3.evidence.weights.set(new la.DenseLayeredTensor2(BilouOntonotesNerDomain.size, FeaturesDomain.dimensionDomain.size, new la.SparseIndexedTensor1(_)))
    BinarySerializer.deserialize(model3, dstream)
    //model3.parameters.densify()
    println("NER2 model parameters oneNorm "+model3.parameters.oneNorm)
    dstream.close()  // TODO Are we really supposed to close here, or is that the responsibility of the caller
  }  
  
  /** A queue of tokens, FILO, that will dequeue to maintain size less than maxSize,
      and which also has efficient acess to its elements keyed by Token.string. */
  class HashedTokenQueue(val maxSize:Int) extends scala.collection.mutable.Queue[Token] {
    private val hash = new scala.collection.mutable.HashMap[String,scala.collection.mutable.Queue[Token]]
    var debugPrintCount = 0
    /** Return a collection of Tokens with string value equal to the argument's string. */
    def filterByString(string:String): Seq[Token] = if (java.lang.Character.isUpperCase(string(0))) {
      //hash.getOrElse(string, Nil) else Nil
      val tokens = if (hash.contains(string)) hash(string) else return Nil
      tokens
    } else Nil
    def filterByToken(token:Token): Seq[Token] = {
      val tokens = filterByString(token.string)
//      if ((debugPrintCount % 1 == 0) && tokens.length > 0) println("HashedTokenQueue %20s %20s  %-20s  true=%-10s  pred=%-10s  freq=%-5s  %s".format(token.getPrev.map(_.string).getOrElse(null), token.string, token.getNext.map(_.string).getOrElse(null), token.attr[BilouOntonotesNerLabel].target.categoryValue, token.attr[BilouOntonotesNerLabel].categoryValue, mostFrequentLabel(tokens).baseCategoryValue, tokens.map(_.attr[BilouOntonotesNerLabel].categoryValue).mkString(" ")))
//      debugPrintCount += 1
      tokens
    }
    // A label having the most frequent value of all labels associated with all Tokens having the same string as the given Token, or null if there is no Token with matching string
    def mostFrequentLabel(token:Token): BilouOntonotesNerLabel = filterByToken(token) match {
      case Nil => null
      case tokens: Seq[Token] => tokens.groupBy(_.attr[BilouOntonotesNerLabel].baseCategoryValue).maxBy(_._2.size)._2.head.attr[BilouOntonotesNerLabel]
    }
    private def mostFrequentLabel(tokens:Seq[Token]): BilouOntonotesNerLabel = tokens.groupBy(_.attr[BilouOntonotesNerLabel].baseCategoryValue).maxBy(_._2.size)._2.head.attr[BilouOntonotesNerLabel]

    // Add a Token to the Queue and also to the internal hash
    override def +=(token:Token): this.type = {
      val str = token.string
      if (java.lang.Character.isUpperCase(str(0)) && !lexicon.StopWords.containsWord(str.toLowerCase)) { // Only add capitalized, non-stopword Tokens
        super.+=(token)
        hash.getOrElseUpdate(token.string, new scala.collection.mutable.Queue[Token]) += token
        if (debugPrintCount % 1000 == 0) println("HashedTokenQueue %20s %20s  %-20s  %s true=%-10s  pred=%-10s  freq=%-5s  %s".format(token.getPrev.map(_.string).getOrElse(null), token.string, token.getNext.map(_.string).getOrElse(null), if (token.attr[BilouOntonotesNerLabel].valueIsTarget) " " else "*", token.attr[BilouOntonotesNerLabel].target.categoryValue, token.attr[BilouOntonotesNerLabel].categoryValue, mostFrequentLabel(hash(token.string)).baseCategoryValue, hash(token.string).map(_.attr[BilouOntonotesNerLabel].categoryValue).mkString(" ")))
        debugPrintCount += 1
        if (length > maxSize) dequeue()
      }
      this
    }
    override def dequeue(): Token = {
      val token = super.dequeue()
      val q2 = hash(token.string)
      val t2 = q2.dequeue(); assert(t2 eq token)
      if (q2.size == 0) hash -= token.string
      token
    }
    override def clear(): Unit = {
      super.clear()
      hash.clear()
    }
    override def toString(): String = {
      (for (token <- this) yield
          "%s = %s".format(token.string, filterByToken(token).map(token => OntonotesNerDomain(BilouOntonotesNerDomain.bilouSuffixIntValue(token.attr[BilouOntonotesNerLabel].intValue)).category).mkString(" "))
      ).mkString("\n")
    }
  }

}

/** The default NER1 with parameters loaded from resources in the classpath. */
object NER2WSJ extends NER2(cc.factorie.util.ClasspathURL[NER2]("-WSJ.factorie"))
object NER2Ontonotes extends NER2(cc.factorie.util.ClasspathURL[NER2]("-Ontonotes.factorie"))

object NER2Trainer {
  def main(args:Array[String]): Unit = {
    implicit val random = new scala.util.Random(0)
    if (args.length != 3) throw new Error("Usage: trainfile testfile savemodelfile")
    val ner = new NER2
    ner.train(args(0), args(1))
    ner.serialize(args(2))
  }
}

