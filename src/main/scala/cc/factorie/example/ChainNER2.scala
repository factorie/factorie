package cc.factorie.example

object ChainNER2 {
  import scala.io.Source
  import cc.factorie._ 
  import cc.factorie.er._
  import cc.factorie.application.LabeledTokenSeqs._

  // Variable classes Token, Label and LabeledTokenSeq are already defined in cc.factorie.application.LabeledTokenSeqs
  // Use them to define model:
  val model = new Model(
    Foreach[Label] { label => Score(label) }                          % "LabelPrior",
    Foreach[Label] { label => Score(label.prev, label, label.token) } % "LabelLabelToken",
    //Foreach[Label] { label => Score(label, label.next, label.token) } % "LabelTokenLabel"
    //For[Label] { label => Score(label, label.token) }                 % "LabelToken",
    //For[Label] { label => Score(label, label.next) }                  % "LabelMarkov",
  )

  def main(args: Array[String]) : Unit = {
    if (args.length != 2) throw new Error("Usage: ChainNER1 trainfile testfile")
    // Read training and testing data.  The function 'featureExtractor' function is defined below
    val trainSentences = LabeledTokenSeq.fromOWPL(Source.fromFile(args(0)), featureExtractor, "-DOCSTART-".r)
    val testSentences =  LabeledTokenSeq.fromOWPL(Source.fromFile(args(1)), featureExtractor, "-DOCSTART-".r)
    (trainSentences ++ testSentences).foreach(s => s.addNeighboringFeatureConjunctions(List(0), List(-1,0), List(1)))
    println("Training on "+trainSentences.size+" sentences, "+trainSentences.foldLeft(0)(_+_.size)+" tokens.")
    println("Testing  on "+testSentences.size+" sentences, "+testSentences.foldLeft(0)(_+_.size)+" tokens.")
    println("Labels: "+Domain[Label].toList)

    // Get the variables to be inferred; prune the data so that it can finish in just ~2 minutes
    val trainLabels = trainSentences.flatMap(_.labels)//.take(20000)
    val testLabels = testSentences.flatMap(_.labels)//.take(10000)

    trainLabels.take(20).foreach(printLabel(_))
    println("Domain size = "+Domain[Token].size)
    
    // Train and test!
    (trainLabels ++ testLabels).foreach(_.setRandomly)
    val learner = new GibbsSampler1[Label](model,Global.defaultObjective) with SampleRank 
    //with PerceptronUpdates 
    with ConfidenceWeightedUpdates 
    {
      temperature = 0.01
      override def preProcessHook(label:Label) = if (label.valueIsTruth && !label.token.isCapitalized && Global.random.nextDouble > 0.5) null else label
      override def postIterationHook(): Boolean = { println(LabeledTokenSeq.segmentEvaluation(trainLabels)); true }
    }
    //with FactorQueue[Variable with IterableSettings] { def process0(x:AnyRef):DiffList = x match { case l:Label => process(l); case _ => null} }
    // Train for 5 iterations through all Labels
    val startTime = System.currentTimeMillis
    learner.process(trainLabels, 6)
    println("Finished training in "+(System.currentTimeMillis-startTime)/60000.0+" minutes.")
    // Predict, also by sampling, visiting each variable 3 times.
    //new SamplingMaximizer[Label](model).infer(testLabels, 5)
    new GibbsSampler(model) {temperature = 0.1}.process(testLabels, 1)
    new GibbsSampler(model) {temperature = 0.01}.process(testLabels, 3)
    println("TRAIN\n"+LabeledTokenSeq.segmentEvaluation(trainLabels))
    println("TEST\n"+LabeledTokenSeq.segmentEvaluation(testLabels))
  }

  def printLabel(label:Label) : Unit = {
    println("%-16s TRUE=%-8s PRED=%-8s %s".format(label.token.word, label.trueValue, label.value, label.token.toString))
  }
 
  def printDiagnostic(labels:Seq[Label]) : Unit = {
    for (label <- labels; if (label.index != label.domain.index("O"))) {
      if (!label.hasPrev || label.value != label.prev.value) 
        print("%-7s %-7s ".format((if (label.value != label.trueValue) label.trueValue else " "), label.value))
      print(label.token.word+" ")
      if (!label.hasNext || label.value != label.next.value) println()
    }
  }
 

    // Feature extraction
  def featureExtractor(initialFeatures:Seq[String]) : Seq[String] = {
    import scala.collection.mutable.ArrayBuffer
    val f = new ArrayBuffer[String]
    val word = initialFeatures(0)
    if (word.matches("(19|20)\\d\\d")) f += "W=<YEAR>" 
    else if (word.matches("\\d")) f += "W=<#>" 
    else if (word.matches("\\d\\d")) f += "W=<##>" 
    else if (word.matches("\\d+")) f += "W=<NUM>" 
    else f += "W="+word
    f ++= initialFeatures.drop(1)
    if (word.length > 3) f += "PRE="+word.substring(0,3)
    if (Capitalized.findFirstMatchIn(word) != None) f += "CAPITALIZED"
    if (Numeric.findFirstMatchIn(word) != None) f += "NUMERIC"
    if (Punctuation.findFirstMatchIn(word) != None) f += "PUNCTUATION"
    f
  }
  val Capitalized = "^[A-Z].*".r
  val Numeric = "^[0-9]+$".r
  val Punctuation = "[,\\.;:?!()]+".r

}


