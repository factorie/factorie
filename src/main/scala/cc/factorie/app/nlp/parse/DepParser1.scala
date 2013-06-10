/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */
package cc.factorie.app.nlp.parse

import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.pos._
import cc.factorie.app.classify.{MultiClassModel, Classification, Classifier, LabelList}
import cc.factorie.app.nlp.lemma.TokenLemma
import collection.mutable.ArrayBuffer
import java.io.File
import cc.factorie.util.{BinarySerializer, CubbieConversions, ProtectedIntArrayBuffer, LocalDoubleAccumulator}
import cc.factorie.la.{DenseTensor1, Tensor1}
import optimize.{AdaGrad, MiniBatchExample, ParameterAveraging}


class DepParser1(val useLabels: Boolean = true) extends DocumentAnnotator {
  def this(url:java.net.URL) = { this(); deserialize(url.openConnection().getInputStream) }

  class ParserStack extends ProtectedIntArrayBuffer {
    def push(i: Int) = this._append(i)
    def pop() = { val r = this._apply(this._length-1); this._remove(this._length-1); r}
    def head = this._apply(this._length-1)
    def size = this._length
    def apply(i: Int) = this._apply(this._length-i-1)
    def nonEmpty = _length > 0
    def isEmpty = _length == 0
    def elements = _asSeq.reverse
  }

  // The shift/reduce actions predicted
  object ActionDomain extends CategoricalDomain[(Int, String)] {
    override def stringToCategory(s: String): (Int, String) = {
      val (i, str) = s.splitAt(s.indexOf(","))
      (Integer.parseInt(i.substring(1,2)), str.substring(1,str.size-1)) // assumes single digit actionIdx
    }
  }
  class Action(targetAction: (Int, String), val stack: ParserStack, val input: ParserStack, tree: ParseTree) extends LabeledCategoricalVariable(targetAction) {
    def domain = ActionDomain
    val features = new Features(this, stack, input, tree)
    val allowedActions = (1 to 4).filter(actionIdx => {
      actionIdx match {
        /*Left*/   case 1 => { stack.size > 1 && tree.parentIndex(stack.head) == ParseTree.noIndex && input.size > 0 }
        /*Right*/  case 2 => { input.size > 0 }
        /*Shift*/  case 3 => { input.size > 1 }
        /*Reduce*/ case 4 => { stack.size > 1 && tree.parentIndex(stack.head) != ParseTree.noIndex }
      }
    }).toSet
    val validTargetList = domain.filter(a => allowedActions.contains(a.category._1)).toSeq.map(_.intValue).iterator
  }

  // The features representing the input and stacks
  object FeaturesDomain extends CategoricalTensorDomain[String]
  val nullLemma = new TokenLemma(null, "null")
  class Features(val label: Action, stack: ParserStack, input: ParserStack, tree: ParseTree) extends BinaryFeatureVectorVariable[String] {
    def domain = FeaturesDomain
    override def skipNonCategories = featuresSkipNonCategories
    import cc.factorie.app.strings.simplifyDigits
    // assumes all locations > 0
    def formFeatures(s: String, seq: ParserStack, locations: Seq[Int], tree: ParseTree): Seq[String] =
      locations.filter(_ < seq.size).map(i => s + "sf-" + { if (seq(i) == ParseTree.noIndex || seq(i) == ParseTree.rootIndex) "null" else simplifyDigits(tree.sentence.tokens(seq(i)).string) } + i)
    def lemmaFeatures(s: String, seq: ParserStack, locations: Seq[Int], tree: ParseTree): Seq[String] =
      locations.filter(_ < seq.size).map(i => s + "lf-" + { if (seq(i) == ParseTree.noIndex || seq(i) == ParseTree.rootIndex) "null" else tree.sentence.tokens(seq(i)).attr.getOrElse[TokenLemma](nullLemma).value }+ i)
    def tagFeatures(s: String, seq: ParserStack, locations: Seq[Int], tree: ParseTree): Seq[String] =
      locations.filter(_ < seq.size).map(i => s + "it-" + { if (seq(i) == ParseTree.noIndex || seq(i) == ParseTree.rootIndex) "null" else tree.sentence.tokens(seq(i)).attr[pos.PTBPosLabel].categoryValue }+ i)
    def depRelFeatures(s: String, seq: ParserStack, locations: Seq[Int], tree: ParseTree): Seq[String] =
      locations.filter(_ < seq.size).map(i => s + "sd-" + { if (seq(i) == ParseTree.noIndex || seq(i) == ParseTree.rootIndex) "null" else tree.label(seq(i)).value } + i)
    def lChildDepRelFeatures(s: String, seq: ParserStack, locations: Seq[Int], tree: ParseTree): Seq[String] =
      locations.filter(_ < seq.size).flatMap(i => tree.leftChildren(seq(i)).map(t => s + "lcd-" + tree.label(t.positionInSentence).value.toString() + i))
    def rChildDepRelFeatures(s: String, seq: ParserStack, locations: Seq[Int], tree: ParseTree): Seq[String] =
      locations.filter(_ < seq.size).flatMap(i => tree.rightChildren(seq(i)).map(t => s + "rcd-" + tree.label(t.positionInSentence).value.toString() + i))
    // Initialize the Features value
    def initialize() {
      val sff = formFeatures("s", stack, Seq(0,1), tree)
      val iff = formFeatures("i", input, Seq(0,1), tree)
      // TODO We don't have have a good lemma annotator for new text
      val slf = lemmaFeatures("s", stack, Seq(0,1), tree)
      val ilf = lemmaFeatures("i", input, Seq(0,1), tree)
      val stf = tagFeatures("s", stack, Seq(0,1,2,3), tree)
      val itf = tagFeatures("i", input, Seq(0,1,2,3), tree)
      val sdr = depRelFeatures("s", stack, Seq(0), tree)
      val idr = depRelFeatures("i", input, Seq(0), tree)
      val slc = lChildDepRelFeatures("s", stack, Seq(0), tree)
      val ilc = lChildDepRelFeatures("i", input, Seq(0), tree)
      val src = rChildDepRelFeatures("s", stack, Seq(0), tree)
      val irc = rChildDepRelFeatures("i", input, Seq(0), tree)
      Seq(sff, iff, slf, ilf, stf, itf, sdr, idr, slc, ilc, src, irc).foreach(this ++= _)
      for ((stack,input) <- Seq((sff, iff), (slf, ilf), (sdr, idr));
           left <- stack; right <- input) {
        this += (left + "&" + right)
      }
    }
    initialize()
  }
  var featuresSkipNonCategories = true
  
  // The model scoring an Action in the context of Features
  val model = new MultiClassModel {
    val evidence = Weights(new la.DenseTensor2(ActionDomain.size, FeaturesDomain.dimensionSize))
  }
  
  // Action implementations
  def applyLeftArc(tree: ParseTree, stack: ParserStack, input: ParserStack, relation: String = ""): Unit = {
    val child: Int = stack.pop()
    val parent: Int = input.head
    tree.setParent(child, parent)
    tree.label(child).setCategory(relation)(null)
    assert(tree.label(child).intValue != -1, "Relation is: " + relation)
  }
  def applyRightArc(tree: ParseTree, stack: ParserStack, input: ParserStack, relation: String = ""): Unit = {
    val child: Int = input.pop()
    val parent: Int = stack.head
    tree.setParent(child, parent)
    tree.label(child).setCategory(relation)(null)
    assert(tree.label(child).intValue != -1)
    stack.push(child)
  }
  def applyShift(tree: ParseTree, stack: ParserStack, input: ParserStack, relation: String = ""): Unit =
    stack.push(input.pop())
  def applyReduce(tree: ParseTree, stack: ParserStack, input: ParserStack, relation: String = ""): Unit =
    stack.pop()
  def applyAction(tree: ParseTree, stack: ParserStack, input: ParserStack, actionIdx: Int, relation: String) {
    actionIdx match {
      case 1 => applyLeftArc(tree,stack,input,relation)
      case 2 => applyRightArc(tree,stack,input,relation)
      case 3 => applyShift(tree,stack,input,relation)
      case 4 => applyReduce(tree,stack,input,relation)
      case _ => throw new Error("Action category value is invalid.")
    }
  }

  def predict(stack: ParserStack, input: ParserStack, tree: ParseTree): (Action, (Int, String)) = {
    val v = new Action((4, ""), stack, input, tree)
    val weights = model.evidence.value
    val prediction = weights * v.features.tensor.asInstanceOf[Tensor1]
    (v, v.domain.categories(v.validTargetList.maxBy(prediction(_))))
  }

  def parse(s: Sentence): Seq[Action] = {
    val actionsPerformed = new ArrayBuffer[Action]
    //s.attr.remove[ParseTree]
    val tree = s.attr.getOrElseUpdate(new ParseTree(s))
    for (i <- 0 until s.length) tree._parents(i) = ParseTree.noIndex // need to clear the parse tree before parsing
    val stack = new ParserStack
    stack.push(ParseTree.rootIndex)
    val input = new ParserStack; for (i <- (0 until s.length).reverse) input.push(i)
    while(input.nonEmpty) {
      val (action,  (actionIdx, relation)) = predict(stack, input, tree)
      actionsPerformed.append(action)
      applyAction(tree, stack, input, actionIdx, relation)
      while (input.isEmpty && stack.size > 1) {
        if (tree.parentIndex(stack.head) == ParseTree.noIndex)
          input.push(stack.pop())
        else
          stack.pop()
      }
    }
    actionsPerformed
  }
  
  def freezeDomains(): Unit = {
    featuresSkipNonCategories = true
    FeaturesDomain.freeze()
    ActionDomain.freeze()
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
    BinarySerializer.serialize(ActionDomain, dstream)
    BinarySerializer.serialize(FeaturesDomain.dimensionDomain, dstream)
    BinarySerializer.serialize(model, dstream)
    dstream.close()  // TODO Are we really supposed to close here, or is that the responsibility of the caller
  }
  def deserialize(stream: java.io.InputStream): Unit = {
    import CubbieConversions._
    val dstream = new java.io.DataInputStream(stream)
    BinarySerializer.deserialize(ActionDomain, dstream)
    BinarySerializer.deserialize(FeaturesDomain.dimensionDomain, dstream)
    BinarySerializer.deserialize(model, dstream)
    model.parameters.densify()
    dstream.close()  // TODO Are we really supposed to close here, or is that the responsibility of the caller
  }
  
    
  // Training
  def generateTrainingLabels(ss: Seq[Sentence]): Seq[Seq[Action]] = ss.par.map(generateTrainingLabels(_)).seq
  def generateTrainingLabels(s: Sentence): Seq[Action] = {
    val origTree = s.attr[ParseTree]
    val tree = new ParseTree(s)
    val stack = new ParserStack; stack.push(ParseTree.rootIndex)
    val input = new ParserStack; for (i <- (0 until s.length).reverse) input.push(i)
    val actionLabels = new ArrayBuffer[Action]
    while (input.nonEmpty) {
      var done = false
      val inputIdx = input.head
      val inputIdxParent = origTree.parentIndex(inputIdx)
      val stackIdx = stack.head
      val stackIdxParent = {
        if (stackIdx == ParseTree.rootIndex) -3
        else origTree.parentIndex(stackIdx)
      }
      if (inputIdxParent == stackIdx) {
        val action = new Action((2, { if (useLabels) origTree.label(inputIdx).categoryValue else "" }), stack, input, tree) // RightArc
        assert(action.allowedActions.contains(action.targetCategory._1), action.targetCategory._1 + " stack " + stack + " input " + input)
        actionLabels.append(action)
        applyAction(tree, stack, input, action.categoryValue._1, action.categoryValue._2)
        done = true
      } else if (inputIdx == stackIdxParent) {
        val action = new Action((1, { if (useLabels) origTree.label(stackIdx).categoryValue else "" }), stack, input, tree) // LeftArc
        assert(action.allowedActions.contains(action.targetCategory._1), action.targetCategory._1 + " stack " + stack + " input " + input)
        actionLabels.append(action)
        applyAction(tree, stack, input, action.categoryValue._1, action.categoryValue._2)
        done = true
      } else {
        for (si <- stack.elements.drop(1);
             if (inputIdx != ParseTree.rootIndex &&
                (tree.parentIndex(stack.head) != ParseTree.noIndex) &&
                ((inputIdxParent == si) ||
                 (inputIdx == { if (si == ParseTree.rootIndex) -3 // -3 doesn't conflict with noIndex or rootIndex //ParseTree.noIndex
                                else  origTree.parentIndex(si) })))) {// is the -2 right here?
          val action = new Action((4, ""), stack, input, tree) // Reduce
          assert(action.allowedActions.contains(action.targetCategory._1), action.targetCategory._1 + " stack " + stack + " input " + input)
          actionLabels.append(action)
          applyAction(tree, stack, input, action.categoryValue._1, action.categoryValue._2)
          done = true
        }
      }
      if (!done) {
        val action = new Action((3, ""), stack, input, tree) // Shift
        actionLabels.append(action)
        applyAction(tree, stack, input, action.categoryValue._1, action.categoryValue._2)
      }
    }
    actionLabels
  }
  class Example(ignoredModel:Parameters, featureVector:la.Tensor1, targetLabel:Int) extends optimize.Example {
    // similar to GLMExample, but specialized to DepParser.model
    def accumulateExampleInto(gradient:la.WeightsMapAccumulator, value:util.DoubleAccumulator): Unit = {
      val weights = model.evidence.value
      val prediction = weights * featureVector
      val (obj, grad) = optimize.LinearObjectives.sparseLogMultiClass.valueAndGradient(prediction, targetLabel)
      if (value ne null) value.accumulate(obj)
      if (gradient ne null) gradient.accumulate(model.evidence, grad outer featureVector)
    }
  }

  case class TrainOptions(val l2: Double, val l1: Double, val lrate: Double, val optimizer: String)  
  def train(trainSentences:Seq[Sentence], testSentences:Seq[Sentence], devSentences:Seq[Sentence], name: String, nThreads: Int, options: TrainOptions, numIteration: Int = 10): Unit = {
    featuresSkipNonCategories = false
    println("Generating trainActions...")
    val trainActions = new LabelList[Action, Features]((action: Action) => action.features)
    val testActions = new LabelList[Action, Features]((action: Action) => action.features)
    for (s <- trainSentences) trainActions ++= generateTrainingLabels(s)
    for (s <- testSentences) testActions ++= generateTrainingLabels(s)
    println("%d actions.  %d input features".format(ActionDomain.size, FeaturesDomain.dimensionSize))
    println("%d parameters.  %d tensor size.".format(ActionDomain.size * FeaturesDomain.dimensionSize, model.evidence.value.length))
    println("Generating examples...")
    val examples = MiniBatchExample(10,trainActions.map(a => new Example(model, a.features.value.asInstanceOf[la.Tensor1], a.targetIntValue)))
    freezeDomains()
    println("Training...")
    val rng = new scala.util.Random(0)
    val l1 = 10*options.l1/examples.length
    val l2 = 10*options.l2/examples.length

    val opt = options.optimizer match {
      case "AdaGradRDA" =>  new cc.factorie.optimize.AdaGradRDA(0.1, options.lrate, l1, l2)
      case "AdaMira" =>  new cc.factorie.optimize.AdaMira(options.lrate)
      case "AdaGrad" =>  new AdaGrad(options.lrate, 0.1)
      case "L2RegularizedConstantRate" => new cc.factorie.optimize.L2RegularizedConstantRate(l2,options.lrate)
    }


    val trainer = new optimize.SynchronizedOptimizerOnlineTrainer(model.parameters, opt, maxIterations = numIteration, nThreads = nThreads)
    var iter = 0
    while(!trainer.isConverged) {
      iter += 1
      trainer.processExamples(rng.shuffle(examples).toSeq.asInstanceOf[Seq[Example]])
      // trainActions.foreach()
      trainActions.foreach(a => {
        a.set((model.evidence.value * a.features.tensor.asInstanceOf[Tensor1]).maxIndex)(null)
      })
      println("Train action accuracy = "+HammingObjective.accuracy(trainActions))
      //opt.setWeightsToAverage(model.weightsSet)
      val t0 = System.currentTimeMillis()

      println("Predicting train set..."); trainSentences.foreach { s => parse(s) } // Was par
      println("Predicting test set...");  testSentences.foreach { s => parse(s) } // Was par
      println("Processed in " + (trainSentences.toSeq.length+testSentences.toSeq.length)*1000.0/(System.currentTimeMillis()-t0) + " sentences per second")
      println("Training UAS = "+ ParserEval.calcUas(trainSentences.map(_.attr[ParseTree])))
      println(" Testing UAS = "+ ParserEval.calcUas(testSentences.map(_.attr[ParseTree])))
      println()
      println("Training LAS = "+ ParserEval.calcLas(trainSentences.map(_.attr[ParseTree])))
      println(" Testing LAS = "+ ParserEval.calcLas(testSentences.map(_.attr[ParseTree])))
      println()
      println("Saving model...")
      if (name != "") serialize(name + "-iter-"+iter)

      //opt.unSetWeightsToAverage(model.weightsSet)
    }
    println("Finished training.")
    //opt.setWeightsToAverage(model.weightsSet)
    //opt.finalizeWeights(model.weightsSet)
    // Print accuracy diagnostics
  }

  // DocumentAnnotator interface
  def process1(d: Document) = { for (sentence <- d.sentences) parse(sentence); d }
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Sentence], classOf[pos.PTBPosLabel]) // TODO Also TokenLemma?  But we don't have a lemmatizer that matches the training data 
  def postAttrs: Iterable[Class[_]] = List(classOf[ParseTree])
  override def tokenAnnotationString(token:Token): String = {
    val pt = token.sentence.attr[ParseTree]
    if (pt eq null) "_\t_"
    else (pt.parentIndex(token.sentencePosition)+1).toString+"\t"+(pt.targetParentIndex(token.sentencePosition)+1)+"\t"+pt.label(token.sentencePosition).categoryValue+"\t"+pt.label(token.sentencePosition).targetCategory
  }
}

object DepParser1 extends DepParser1(cc.factorie.util.ClasspathURL(classOf[DepParser1], ".factorie"))


// Driver for training
object DepParser1Trainer {
  def main(args: Array[String]): Unit = {
    object opts extends cc.factorie.util.DefaultCmdOptions {
      val trainFile = new CmdOption("train", "", "FILES", "CoNLL-2008 train file.")
      //val devFile =   new CmdOption("dev", "", "FILES", "CoNLL-2008 dev file")
      val testFile =  new CmdOption("test", "", "FILES", "CoNLL-2008 test file.")
      val unlabeled  = new CmdOption("unlabeled", false, "BOOLEAN", "Whether to ignore labels.")
      val model      = new CmdOption("model", "parser-model", "FILE", "File in which to save the trained model.")
      val outputDir  = new CmdOption("output", ".", "DIR", "Directory in which to save the parsed output (to be scored by eval.pl).")
      val warmModel  = new CmdOption("warm", "file:parser-model", "URL", "File from which to read a model for warm-start training.")
      val nThreads   = new CmdOption("nThreads", 10, "INT", "Number of threads to use.")
      val l2 = new CmdOption("l2",1.0,"FLOAT","l2 regularization param")
      val l1 = new CmdOption("l1",1.0,"FLOAT","l1 regularization param")
      val optimizerStr = new CmdOption("optimizer","AdaGradRDA","STRING","what optimizer to use")
      val lrate = new CmdOption("lrate",1.0,"FLOAT","Learning Rate Param")
    }


    opts.parse(args)

    val parser = new DepParser1(!opts.unlabeled.value)
    val trainOptions = parser.TrainOptions(opts.l2.value, opts.l1.value, opts.lrate.value ,opts.optimizerStr.value)

    if (opts.warmModel.wasInvoked) {
      print("Loading " + opts.warmModel.value + " as a warm-start model.....")
      parser.deserialize(util.ClasspathURL(opts.warmModel.value).openConnection.getInputStream)
      println("Finished loading warm-start model.")
    }

    val trainDoc = LoadOntonotes5.fromFilename(opts.trainFile.value).head
    val testDoc = LoadOntonotes5.fromFilename(opts.testFile.value).head
    
    // Train
    parser.train(trainDoc.sentences.toSeq, testDoc.sentences.toSeq, null, opts.model.value, math.min(opts.nThreads.value, Runtime.getRuntime.availableProcessors()),trainOptions)
    // Test
    parser.freezeDomains()
    
    // Print accuracy diagnostics
    println("Predicting train set..."); trainDoc.sentences.foreach { s => parser.parse(s) } // Was par
    println("Predicting test set...");  testDoc.sentences.foreach { s => parser.parse(s) } // Was par
    println("Training UAS = "+ ParserEval.calcUas(trainDoc.sentences.toSeq.map(_.attr[ParseTree])))
    println(" Testing UAS = "+ ParserEval.calcUas(testDoc.sentences.toSeq.map(_.attr[ParseTree])))
    println()
    println("Training LAS = "+ ParserEval.calcLas(trainDoc.sentences.toSeq.map(_.attr[ParseTree])))
    println(" Testing LAS = "+ ParserEval.calcLas(testDoc.sentences.toSeq.map(_.attr[ParseTree])))

    //parser.model.skipNonCategories = false
    // Write results
    println("Writing results...")
    //WriteConll2008.toFile(opts.outputDir.value + "/dep.train", trainDoc, opts.trainFile.value)
    //WriteConll2008.toFile(opts.outputDir.value + "/dep.test", testDoc, opts.testFile.value)
    var out = new java.io.PrintStream(new java.io.File(opts.outputDir.value + "/dep.train"))
    out.println(trainDoc.owplString(Seq((t:Token) => t.attr[PTBPosLabel].categoryValue, parser.tokenAnnotationString(_))))
    out.close()
    out = new java.io.PrintStream(new java.io.File(opts.outputDir.value + "/dep.test"))
    out.println(testDoc.owplString(Seq((t:Token) => t.attr[PTBPosLabel].categoryValue, parser.tokenAnnotationString(_))))
    out.close()    

    println("Done.")
  }
  
}


