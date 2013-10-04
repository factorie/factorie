package cc.factorie.app.nlp.parse

import cc.factorie.app.nlp._
import cc.factorie._
import cc.factorie.app.nlp.pos.PennPosLabel
import scala.collection.mutable.{HashSet, HashMap, ArrayBuffer}
import scala.util.parsing.json.JSON
import scala.annotation.tailrec
import java.io.{File,InputStream,FileInputStream}
import cc.factorie.util.BinarySerializer
import scala._
import cc.factorie.optimize._
import scala.concurrent.Await
import scala.Some
import java.util.concurrent.Executors
import cc.factorie.variable.{LabeledCategoricalVariable, BinaryFeatureVectorVariable, CategoricalVectorDomain, CategoricalDomain}
import cc.factorie.app.classify.{OnlineLinearMultiClassTrainer, SVMMultiClassTrainer, LinearMultiClassTrainer, LinearMultiClassClassifier}

/** Default transition-based dependency parser. */
class DepParser1 extends DocumentAnnotator {
  def this(stream:InputStream) = { this(); deserialize(stream) }
  def this(file: File) = this(new FileInputStream(file))
  def this(url:java.net.URL) = {
    this()
    val stream = url.openConnection.getInputStream
    if (stream.available <= 0) throw new Error("Could not open "+url)
    println("DepParser1 loading from "+url)
    deserialize(stream)
  }

  case class ParseDecision(action: String) {
    val Array(lrnS, srpS, label) = action.split(" ")
    val leftOrRightOrNo = lrnS.toInt
    val shiftOrReduceOrPass = srpS.toInt
  }
  object labelDomain extends CategoricalDomain[String]
  val defaultCategory = "-1 -1 N"
  labelDomain += defaultCategory
  class ParseDecisionVariable(targetDecision: ParseDecision, val state: ParseState) extends LabeledCategoricalVariable(targetDecision.action) {
    def domain = labelDomain
    val features = new NonProjDependencyParserFeatures(this)
    features ++= featureGenerators.map(_.apply(state))
  }
  object featuresDomain extends CategoricalVectorDomain[String]
  class NonProjDependencyParserFeatures(val decisionVariable: ParseDecisionVariable) extends BinaryFeatureVectorVariable[String] {
    def domain = featuresDomain
    override def skipNonCategories = domain.dimensionDomain.frozen
  }
  
  // Serialization
  def serialize(file: File): Unit = {
    if (file.getParentFile ne null) file.getParentFile.mkdirs()
    serialize(new java.io.FileOutputStream(file))
  }
  def deserialize(file: File): Unit = {
    require(file.exists(), "Trying to load non-existent file: '" +file)
    deserialize(new java.io.FileInputStream(file))
  }
  def serialize(stream: java.io.OutputStream): Unit = {
    import cc.factorie.util.CubbieConversions._
    // Sparsify the evidence weights
    import scala.language.reflectiveCalls
    val sparseEvidenceWeights = new la.DenseLayeredTensor2(featuresDomain.dimensionDomain.size, labelDomain.size, new la.SparseIndexedTensor1(_))
    model.weights.value.foreachElement((i, v) => if (v != 0.0) sparseEvidenceWeights += (i, v))
    model.weights.set(sparseEvidenceWeights)
    val dstream = new java.io.DataOutputStream(stream)
    BinarySerializer.serialize(featuresDomain.dimensionDomain, dstream)
    BinarySerializer.serialize(labelDomain, dstream)
    BinarySerializer.serialize(model, dstream)
    dstream.close()  // TODO Are we really supposed to close here, or is that the responsibility of the caller?
  }
  def deserialize(stream: java.io.InputStream): Unit = {
    import cc.factorie.util.CubbieConversions._
    // Get ready to read sparse evidence weights
    val dstream = new java.io.DataInputStream(stream)
    BinarySerializer.deserialize(featuresDomain.dimensionDomain, dstream)
    BinarySerializer.deserialize(labelDomain, dstream)
    import scala.language.reflectiveCalls
    model.weights.set(new la.DenseLayeredTensor2(featuresDomain.dimensionDomain.size, labelDomain.size, new la.SparseIndexedTensor1(_)))
    BinarySerializer.deserialize(model, dstream)
    println("DepParser1 model parameters oneNorm "+model.parameters.oneNorm)
    dstream.close()  // TODO Are we really supposed to close here, or is that the responsibility of the caller?
  }
    
    
    
  def classify(v: ParseDecisionVariable) = new ParseDecision(labelDomain.category(model.classification(v.features.value).bestLabelIndex))
  lazy val model = new LinearMultiClassClassifier(labelDomain.size, featuresDomain.dimensionSize)


  def trainFromVariables(vs: Iterable[ParseDecisionVariable], trainer: LinearMultiClassTrainer, evaluate: (LinearMultiClassClassifier) => Unit) {
    trainer.baseTrain(model, vs.map(_.targetIntValue).toSeq, vs.map(_.features.value).toSeq, vs.map(v => 1.0).toSeq, evaluate)
  }
  
  
  def train(trainSentences:Iterable[Sentence], testSentences:Iterable[Sentence], numBootstrappingIterations:Int = 2, l1Factor:Double = 0.00001, l2Factor:Double = 0.00001, nThreads: Int = 1)(implicit random: scala.util.Random): Unit = {
    featuresDomain.dimensionDomain.gatherCounts = true
    var trainingVars: Iterable[ParseDecisionVariable] = generateDecisions(trainSentences, 0, nThreads)
    println("Before pruning # features " + featuresDomain.dimensionDomain.size)
    println("DepParser1.train first 20 feature counts: "+featuresDomain.dimensionDomain.counts.toSeq.take(20))
    featuresDomain.dimensionDomain.trimBelowCount(5) // Every feature is actually counted twice, so this removes features that were seen 2 times or less
    featuresDomain.freeze()
    println("After pruning # features " + featuresDomain.dimensionDomain.size)
    trainingVars = generateDecisions(trainSentences, 0, nThreads)
    featuresDomain.freeze()
    val numTrainSentences = trainSentences.size
    val optimizer = new AdaGradRDA(1.0, 0.1, l1Factor / numTrainSentences, l2Factor / numTrainSentences)
    trainDecisions(trainingVars, optimizer, trainSentences, testSentences)
    trainingVars = null // Allow them to be GC'ed
    for (i <- 0 until numBootstrappingIterations) {
      println("Boosting iteration " + (i+1))
      trainDecisions(generateDecisions(trainSentences, ParserConstants.BOOSTING, nThreads), optimizer, trainSentences, testSentences)
    }
  }
  
  def trainDecisions(trainDecisions:Iterable[ParseDecisionVariable], optimizer:optimize.GradientOptimizer, trainSentences:Iterable[Sentence], testSentences:Iterable[Sentence])(implicit random: scala.util.Random): Unit = {
    def evaluate(c: LinearMultiClassClassifier) {
      // println(model.weights.value.toSeq.count(x => x == 0).toFloat/model.weights.value.length +" sparsity")
      println(" TRAIN "+testString(trainSentences))
      println(" TEST  "+testString(testSentences))
    }
    new OnlineLinearMultiClassTrainer(optimizer=optimizer, maxIterations=2).baseTrain(model, trainDecisions.map(_.targetIntValue).toSeq, trainDecisions.map(_.features.value).toSeq, trainDecisions.map(v => 1.0).toSeq, evaluate=evaluate)
  }
  
  def testString(testSentences:Iterable[Sentence]): String = {
    val t0 = System.currentTimeMillis()
    testSentences.foreach(process)
    val totalTime = System.currentTimeMillis() - t0
    val totalTokens = testSentences.map(_.tokens.length).sum
    val pred = testSentences.map(_.attr[ParseTree])
    "LAS="+ParserEval.calcLas(pred)+" UAS="+ParserEval.calcUas(pred)+s"  ${totalTokens*1000.0/totalTime} tokens/sec"
  }


  lazy val testFeatureSpec = io.Source.fromURL(this.getClass.getResource("/parser-features.json")).getLines().mkString("\n")
  lazy val featureGenerators: Seq[DependencyFeatures.DependencyFeatureGenerator] = DependencyFeatures.fromJSON(testFeatureSpec)

  object ParserConstants {
    val SHIFT  = 0
    val REDUCE = 1
    val PASS   = 2

    val LEFT  = 0
    val RIGHT = 1
    val NO    = 2

    val ROOT_ID = 0

    val TRAINING   = 0
    val PREDICTING = 1
    val BOOSTING   = 2
  }

  def generateDecisions(ss: Iterable[Sentence], mode: Int, nThreads: Int): Iterable[ParseDecisionVariable] = {
    val decs = cc.factorie.util.Threading.parMap(ss, nThreads)(s => {
      val oracle: NonProjectiveOracle = {
        if (mode == ParserConstants.TRAINING) new NonprojectiveGoldOracle(s)
        else new NonprojectiveBoostingOracle(s, classify)
      }
      new NonProjectiveShiftReduce(oracle.predict).parse(s)
      oracle.instances.toSeq
    })
    decs.flatten
  }
  def boosting(ss: Iterable[Sentence], nThreads: Int, trainer: LinearMultiClassTrainer, evaluate: LinearMultiClassClassifier => Unit) =
    trainFromVariables(generateDecisions(ss, ParserConstants.BOOSTING, nThreads), trainer, evaluate)

  // For DocumentAnnotator trait
  def process(doc: Document) = { doc.sentences.foreach(process(_)); doc }
  def prereqAttrs = Seq(classOf[Sentence], classOf[PennPosLabel], classOf[lemma.WordNetTokenLemma]) // Sentence also includes Token
  def postAttrs = Seq(classOf[ParseTree])
  override def tokenAnnotationString(token:Token): String = {
    val sentence = token.sentence
    val pt = if (sentence ne null) sentence.attr[ParseTree] else null
    if (pt eq null) "_\t_"
    else (pt.parentIndex(token.positionInSentence)+1).toString+"\t"+pt.label(token.positionInSentence).categoryValue
  }
  //override def tokenAnnotationString(token:Token): String = { val parse = token.parseParent; if (parse ne null) parse.positionInSentence+"\t"+token.parseLabel.categoryValue else "_\t_" }

  def process(s: Sentence): Sentence = {
    val parse = s.attr.getOrElseUpdate(new ParseTree(s))
    new NonProjectiveShiftReduce(predict = classify).parse(s).zipWithIndex.map(dt => {
      parse.setParent(dt._2, dt._1._1)
      parse.label(dt._2).set(ParseTreeLabelDomain.index(dt._1._2))(null)
    })
    s
  }

  class NonProjectiveShiftReduce(val predict: ParseDecisionVariable => ParseDecision) {
    import ParserConstants._
    def parse(s: Sentence) = {
      val state = new ParseState(0, 1, collection.mutable.HashSet[Int](), s)
      while(state.input < state.sentenceTokens.length) {
        if (state.stack < 0)
          noShift(state)
        else {
          val label = predict(new ParseDecisionVariable(ParseDecision(defaultCategory), state))
          if (label.leftOrRightOrNo == LEFT) {
            if (state.stack == ROOT_ID) noShift(state)
            else if (state.inputToken(0).isDescendentOf(state.stackToken(0))) noPass(state)
            else if (label.shiftOrReduceOrPass == REDUCE) leftReduce(label.label, state)
            else leftPass(label.label, state)
          }
          else if (label.leftOrRightOrNo == RIGHT) {
              if (state.stackToken(0).isDescendentOf(state.inputToken(0))) noPass(state)
              else if (label.shiftOrReduceOrPass == SHIFT) rightShift(label.label, state)
              else rightPass(label.label, state)
          }
          else {
              if (label.shiftOrReduceOrPass == SHIFT) noShift(state)
              else if (label.shiftOrReduceOrPass == REDUCE && state.stackToken(0).hasHead) noReduce(state)
              else noPass(state)
          }
        }
      }
      state.sentenceTokens.drop(1).map(dt => if (dt.hasHead) (dt.head.depToken.thisIdx-1, dt.head.label) else (-1,""))
    }

    private def passAux(state: ParseState): Unit = {
      var i = state.stack - 1
      while (i >= 0) {
        if (!state.reducedIds.contains(i)) {
            state.stack = i
            return
        }
        i -= 1
      }
      state.stack = i
    }

    private def leftArc(label: String, state: ParseState)  { state.stackToken(0).setHead(state.inputToken(0), label) }
    private def rightArc(label: String, state: ParseState) { state.inputToken(0).setHead(state.stackToken(0), label) }

    private def shift(state: ParseState)  { state.stack = state.input; state.input += 1 }
    private def reduce(state: ParseState) { state.reducedIds.add(state.stack); passAux(state) }
    private def pass(state: ParseState)   { passAux(state: ParseState) }

    private def noShift(state: ParseState)  { shift(state) }
    private def noReduce(state: ParseState) { reduce(state) }
    private def noPass(state: ParseState)   { pass(state) }
    private def leftReduce(label: String, state: ParseState) { leftArc(label, state);  reduce(state) }
    private def leftPass(label: String, state: ParseState)   { leftArc(label, state);  pass(state)   }
    private def rightShift(label: String, state: ParseState) { rightArc(label, state); shift(state)  }
    private def rightPass(label: String, state: ParseState)  { rightArc(label, state); pass(state)   }
  }

  trait NonProjectiveOracle {
    import ParserConstants._
    val sentence: Sentence
    def predict(state: ParseDecisionVariable): ParseDecision

    var instances = new ArrayBuffer[ParseDecisionVariable] { override val initialSize = 100 }
    def getSimpleDepArcs = sentence.parse.targetParents.map(_ + 1).zip(sentence.parse.labels.map(_.target.value.category))
    def getDepArcs = { Seq((-1, "<ROOT-ROOT>")) ++ getSimpleDepArcs.map { case (i: Int, l: String) => (i, l) } }
    val goldHeads = getDepArcs

    def getGoldDecision(state: ParseState): ParseDecision = {
      val shiftOrReduceOrPass =
        getGoldLRN(state) match {
          case LEFT  => if (shouldGoldReduce(hasHead=true, state=state)) REDUCE else PASS
          case RIGHT => if (shouldGoldShift(state=state)) SHIFT else PASS
          case _ => {
            if (shouldGoldShift(state=state)) SHIFT
            else if (shouldGoldReduce(hasHead=false, state=state)) REDUCE
            else PASS
          }
        }
      new ParseDecision(getGoldLRN(state) + " " + shiftOrReduceOrPass + " " + getGoldLabel(state))
    }

    def getGoldLabel(state: ParseState): String = {
      if (goldHeads(state.stack)._1 == state.input) goldHeads(state.stack)._2
      else if (goldHeads(state.input)._1 == state.stack) goldHeads(state.input)._2
      else "N"
    }

    def getGoldLRN(state: ParseState): Int = {
      if (goldHeads(state.stack)._1 == state.input) LEFT
      else if (goldHeads(state.input)._1 == state.stack) RIGHT
      else NO
    }

    def shouldGoldShift(state: ParseState): Boolean = {
      if (goldHeads(state.input)._1 < state.stack) return false
      else
        for (i <- (state.stack - 1) until 0 by -1) if (!state.reducedIds.contains(i)) {
          if (goldHeads(i)._1 == state.input)
            return false
        }
      true
    }

    def shouldGoldReduce(hasHead: Boolean, state: ParseState): Boolean = {
      if (!hasHead && !state.stackToken(0).hasHead)
        return false
      for (i <- (state.input + 1) until state.sentenceTokens.length)
        if (goldHeads(i)._1 == state.stack)
          return false

      true
    }
  }

  class NonprojectiveGoldOracle(val sentence: Sentence) extends NonProjectiveOracle {
    def predict(decisionVariable: ParseDecisionVariable): ParseDecision = {
      val decision = getGoldDecision(decisionVariable.state)
      instances += new ParseDecisionVariable(decision, decisionVariable.state)
      decision
    }
  }

  class NonprojectiveBoostingOracle(val sentence: Sentence, basePredict: ParseDecisionVariable => ParseDecision) extends NonProjectiveOracle {
    def predict(decisionVariable: ParseDecisionVariable): ParseDecision = {
      val label = new ParseDecisionVariable(getGoldDecision(decisionVariable.state), decisionVariable.state)
      instances += label
      basePredict(label)
    }
  }

  object DependencyFeatures {
    val locationAbbrevs = collection.mutable.HashMap(
      "S_LAMBDA" -> "l",
      "S_STACK"  -> "s",
      "S_BETA"   -> "b",
      "R_H"      -> "h",     // head
      "R_LMD"    -> "lmd",   // left-most dependent
      "R_RMD"    -> "rmd"    // right-most dependent
    )
    val formAbbrevs = collection.mutable.HashMap(
      "F_FORM"   -> "f",
      "F_LEMMA"  -> "m",
      "F_POS"    -> "p",
      "F_DEPREL" -> "d",
      "F_LNPL"   -> "lnpl", // left-nearest punctuation of lambda
      "F_RNPL"   -> "rnpl", // right-nearest punctuation of lambda
      "F_LNPB"   -> "lnpb", // left-nearest punctuation of beta
      "F_RNPB"   -> "rnpb"  // right-nearest punctuation of beta
    )
    val locationFns: HashMap[String, (Int) => (ParseState) => DepToken] = HashMap(
      "b"   -> ((offset: Int) => (state: ParseState) => state.inputToken(offset)),
      "l"   -> ((offset: Int) => (state: ParseState) => state.lambdaToken(offset)),
      "s"   -> ((offset: Int) => (state: ParseState) => state.stackToken(offset)),
      "l_h" -> ((_: Int) => (state: ParseState) => state.lambdaToken(0)),
      "l_lmd" -> ((offset: Int) => (state: ParseState) => state.lambdaToken(offset).leftmostDependent),
      "b_lmd" -> ((offset: Int) => (state: ParseState) =>  state.stackToken(offset).leftmostDependent),
      "l_lmd" -> ((offset: Int) => (state: ParseState) => state.lambdaToken(offset).rightmostDependent),
      "b_lmd" -> ((offset: Int) => (state: ParseState) =>  state.stackToken(offset).rightmostDependent)
    )

    val formFns = HashMap(
      "f"   -> ((t: DepToken) => t.form),
      "m"   -> ((t: DepToken) => t.lemma),
      "p"   -> ((t: DepToken) => t.pos),
      "d"   -> ((t: DepToken) => if (!t.hasHead) "null" else t.head.label),
      "b0"  -> ((t: DepToken) => if (t.pos == -2) "null" else (t.state.lambdaToken(0) eq t.state.sentenceTokens(1)).toString),
      "b1"  -> ((t: DepToken) => (t.state.stackToken(0) eq t.state.sentenceTokens.last).toString),
      "b2"  -> ((t: DepToken) => (t.state.input - t.state.stack == 1).toString)
    )

    def generators(locationOffsetAndForm: String): (ParseState => String) = {
      val LocationOffsetAndForm = """([a-z_]*)[+]?([-0-9]*):([a-z]*[0-9]?)""".r
      locationOffsetAndForm match {
        case LocationOffsetAndForm(location, offset, form) => {
          val locationFn = locationFns(location)(if (offset == "") 0 else offset.toInt)
          (state: ParseState) => location + offset + ":" + formFns(form)(locationFn(state))
        }
        case _ => throw new Error("Couldn't parse location and form from feature generator string.")
      }
    }

    abstract class DependencyFeatureGenerator extends (ParseState => String)
    class SingletonDependencyFeatureGenerator(f: String) extends DependencyFeatureGenerator {
      lazy val featureFn = generators(f)
      def apply(s: ParseState): String = featureFn(s)
    }
    class CompositeDependencyFeatureGenerator(gens: Seq[DependencyFeatureGenerator]) extends DependencyFeatureGenerator {
      def apply(s: ParseState) = gens.map(_.apply(s)).mkString("|")
    }
    private def stripJSONComments(s: String) = s.split("\n").map(_.split("#").head).mkString("\n")
    def fromJSON(source: String) = {
      val someJson = JSON.parseFull(stripJSONComments(source))
      val featureSpec = someJson match {
        case map: Some[Map[String, List[List[String]]] @unchecked] => map.get("features")
        case _ => throw new Error()
      }
      featureSpec.map(fs => {
        val fGens = fs.map(f => new SingletonDependencyFeatureGenerator(f))
        if (fGens.length > 1) new CompositeDependencyFeatureGenerator(fGens)
        else fGens.head
      })
    }
  }

  class DepToken(val form: String, val lemma: String, val pos: String, val thisIdx: Int, val state: ParseState) {
    var head: DepArc = null
    def hasHead: Boolean = head ne null

    def setHead(headToken: DepToken, label: String) {
      head = new DepArc(headToken, label)
      if (thisIdx < head.depToken.thisIdx) state.leftmostDeps(head.depToken.thisIdx) = thisIdx
      else state.rightmostDeps(head.depToken.thisIdx) = thisIdx
    }
    def leftmostDependent: DepToken = {
      val i = state.leftmostDeps(thisIdx)
      if (i == -1) state.nullToken
      else state.sentenceTokens(i)
    }
    def rightmostDependent: DepToken = {
      val i = state.rightmostDeps(thisIdx)
      if (i == -1) state.nullToken
      else state.sentenceTokens(i)
    }
    @tailrec final def isDescendentOf(that: DepToken): Boolean = {
      if (!hasHead) false
      else if (this.head.depToken == that) true
      else this.head.depToken.isDescendentOf(that)
    }
  }

  case class DepArc(depToken: DepToken, label: String)

  class ParseState(var stack: Int, var input: Int, val reducedIds: HashSet[Int], sentence: Sentence) {
    private def depToken(token: Token, idx: Int, state: ParseState) = new DepToken(form = token.string, lemma = token.lemmaString, pos = token.posLabel.categoryValue, thisIdx=idx, state=state)
    val rootToken = new DepToken(form = "<ROOT>-f",  lemma = "<ROOT>-m", pos = "<ROOT>-p", thisIdx = 0, state=this)
    val nullToken = new DepToken(form = "<NULL>-f",  lemma = "<NULL>-m", pos = "<NULL>-p", thisIdx = -1, state=this)
    val sentenceTokens = (Seq(rootToken) ++ sentence.tokens.zipWithIndex.map(t => depToken(t._1, t._2+1, this))).toArray

    val leftmostDeps = Array.fill[Int](sentenceTokens.size)(-1)
    val rightmostDeps = Array.fill[Int](sentenceTokens.size)(-1)

    def inputToken(offset: Int): DepToken = {
      val i = input + offset
      if (i < 0 || sentenceTokens.size - 1 < i) nullToken
      else sentenceTokens(i)
    }

    def lambdaToken(offset: Int): DepToken = {
      val i = stack + offset
      if (i < 0 || sentenceTokens.size - 1 < i) nullToken
      else sentenceTokens(i)
    }

    def stackToken(offset: Int): DepToken = {
      if (offset == 0)
        return sentenceTokens(stack)

      var off = math.abs(offset)
      var dir = if (offset < 0) -1 else 1
      var i = stack + dir
      while (0 < i && i < input) {
        if (!reducedIds.contains(i)) {
          off -= 1
          if (off == 0)
            return sentenceTokens(i)
        }
        i += dir
      }
      nullToken
    }
  }
}

object DepParser1 extends DepParser1(cc.factorie.util.ClasspathURL[DepParser1](".factorie"))

object DepParser1Ontonotes extends DepParser1(cc.factorie.util.ClasspathURL[DepParser1]("-Ontonotes.factorie"))



class DepParser1Args extends cc.factorie.util.DefaultCmdOptions with SharedNLPCmdOptions{
  val trainFiles =  new CmdOption("train", Nil.asInstanceOf[List[String]], "FILENAME...", "")
  val testFiles =  new CmdOption("test", Nil.asInstanceOf[List[String]], "FILENAME...", "")
  val devFiles =   new CmdOption("dev", Nil.asInstanceOf[List[String]], "FILENAME...", "")
  val ontonotes = new CmdOption("onto", true, "BOOLEAN", "")
  val cutoff    = new CmdOption("cutoff", "0", "", "")
  val loadModel = new CmdOption("load", "", "", "")
  val nThreads =  new CmdOption("nThreads", 1, "INT", "How many threads to use during training.")
  val useSVM =    new CmdOption("use-svm", true, "BOOL", "Whether to use SVMs to train")
  val modelDir =  new CmdOption("model", "model", "FILENAME", "File in which to save the trained model.")
  val bootstrapping = new CmdOption("bootstrap", "0", "INT", "The number of bootstrapping iterations to do. 0 means no bootstrapping.")
  val saveModel = new CmdOption("save-model",true,"BOOLEAN","whether to write out a model file or not")
  val l1 = new CmdOption("l1", 0.000001,"FLOAT","l1 regularization weight")
  val l2 = new CmdOption("l2", 0.00001,"FLOAT","l2 regularization weight")
  val rate = new CmdOption("rate", 10.0,"FLOAT","base learning rate")
  val delta = new CmdOption("delta", 100.0,"FLOAT","learning rate decay")
}

object DepParser1Trainer extends cc.factorie.util.HyperparameterMain {
  def evaluateParameters(args: Array[String]) = {
    val opts = new DepParser1Args
    implicit val random = new scala.util.Random(0)
    opts.parse(args)

    // Load the sentences
    def loadSentences(o: opts.CmdOption[List[String]]): Seq[Sentence] = {
      if (o.wasInvoked) o.value.toIndexedSeq.flatMap(filename => (if (opts.ontonotes.value) load.LoadOntonotes5.fromFilename(filename) else load.LoadConll2008.fromFilename(filename)).head.sentences.toSeq)
      else Seq.empty[Sentence]
    }

    val sentencesFull = loadSentences(opts.trainFiles)
    val devSentencesFull = loadSentences(opts.devFiles)
    val testSentencesFull = loadSentences(opts.testFiles)

    val trainPortionToTake = if(opts.trainPortion.wasInvoked) opts.trainPortion.value.toDouble  else 1.0
    val testPortionToTake =  if(opts.testPortion.wasInvoked) opts.testPortion.value.toDouble  else 1.0
    val sentences = sentencesFull.take((trainPortionToTake*sentencesFull.length).floor.toInt)
    val testSentences = testSentencesFull.take((testPortionToTake*testSentencesFull.length).floor.toInt)
    val devSentences = devSentencesFull.take((testPortionToTake*devSentencesFull.length).floor.toInt)



    println("Total train sentences: " + sentences.size)
    println("Total test sentences: " + testSentences.size)

    def testSingle(c: DepParser1, ss: Seq[Sentence], extraText: String = ""): Unit = {
      if (ss.nonEmpty) {
        println(extraText + " " + c.testString(ss))
      }
    }

    def testAll(c: DepParser1, extraText: String = ""): Unit = {
      println("\n")
      testSingle(c, sentences,     "Train " + extraText)
      testSingle(c, devSentences,  "Dev "   + extraText)
      testSingle(c, testSentences, "Test "  + extraText)
    }

    // Load other parameters
    val numBootstrappingIterations = opts.bootstrapping.value.toInt
    val c = new DepParser1
    val l1 = 2*opts.l1.value / sentences.length
    val l2 = 2*opts.l2.value / sentences.length
    val optimizer = new AdaGradRDA(opts.rate.value, opts.delta.value, l1, l2)
    val trainer = if (opts.useSVM.value) new SVMMultiClassTrainer()
      else new OnlineLinearMultiClassTrainer(optimizer=optimizer, useParallel=true, miniBatch=50, nThreads=opts.nThreads.value, objective=LinearObjectives.hingeMultiClass, maxIterations=5)
    def evaluate(cls: LinearMultiClassClassifier) {
      println(cls.weights.value.toSeq.count(x => x == 0).toFloat/cls.weights.value.length +" sparsity")
      testAll(c, "iteration ")
    }
    c.featuresDomain.dimensionDomain.gatherCounts = true
    var trainingVs = c.generateDecisions(sentences, 0, opts.nThreads.value)
    println("Before pruning # features " + c.featuresDomain.dimensionDomain.size)
    c.featuresDomain.dimensionDomain.trimBelowCount(5) // Every feature is actually counted twice, so this removes features that were seen 2 times or less
    c.featuresDomain.freeze()
    c.featuresDomain.dimensionDomain.gatherCounts = false
    println("After pruning # features " + c.featuresDomain.dimensionDomain.size)
    trainingVs = c.generateDecisions(sentences, 0, opts.nThreads.value)
    c.trainFromVariables(trainingVs, trainer, evaluate)
    trainingVs = null // GC the old training labels
    for (i <- 0 until numBootstrappingIterations) {
      println("Boosting iteration " + i)
      c.boosting(sentences, nThreads=opts.nThreads.value, trainer=trainer, evaluate=evaluate)
    }
    testSentences.par.foreach(c.process)
    if (opts.saveModel.value) {
      val modelUrl: String = if (opts.modelDir.wasInvoked) opts.modelDir.value else opts.modelDir.defaultValue + System.currentTimeMillis().toString + ".factorie"
      c.serialize(new java.io.File(modelUrl))
      val d = new DepParser1
      d.deserialize(new java.io.File(modelUrl))
      testSingle(c, testSentences, "Post serialization accuracy ")
    }
    val testLAS = ParserEval.calcLas(testSentences.map(_.attr[ParseTree]))
    if(opts.targetAccuracy.wasInvoked) assert(testLAS > opts.targetAccuracy.value.toDouble, "Did not reach accuracy requirement")
    testLAS
  }
}

object DepParse12Optimizer {
  def main(args: Array[String]) {
    val opts = new DepParser1Args
    opts.parse(args)
    opts.saveModel.setValue(false)
    val l1 = cc.factorie.util.HyperParameter(opts.l1, new cc.factorie.util.LogUniformDoubleSampler(1e-10, 1e2))
    val l2 = cc.factorie.util.HyperParameter(opts.l2, new cc.factorie.util.LogUniformDoubleSampler(1e-10, 1e2))
    val rate = cc.factorie.util.HyperParameter(opts.rate, new cc.factorie.util.LogUniformDoubleSampler(1e-4, 1e4))
    val delta = cc.factorie.util.HyperParameter(opts.delta, new cc.factorie.util.LogUniformDoubleSampler(1e-4, 1e4))
    /*
    val ssh = new cc.factorie.util.SSHActorExecutor("apassos",
      Seq("avon1", "avon2"),
      "/home/apassos/canvas/factorie-test",
      "try-log/",
      "cc.factorie.app.nlp.parse.DepParser1",
      10, 5)
      */
    val qs = new cc.factorie.util.QSubExecutor(60, "cc.factorie.app.nlp.parse.DepParser1Trainer")
    val optimizer = new cc.factorie.util.HyperParameterSearcher(opts, Seq(l1, l2, rate, delta), qs.execute, 200, 180, 60)
    val result = optimizer.optimize()
    println("Got results: " + result.mkString(" "))
    println("Best l1: " + opts.l1.value + " best l2: " + opts.l2.value)
    opts.saveModel.setValue(true)
    println("Running best configuration...")
    import scala.concurrent.duration._
    Await.result(qs.execute(opts.values.flatMap(_.unParse).toArray), 1.hours)
    println("Done")
  }
}
