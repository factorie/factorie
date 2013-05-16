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
import cc.factorie.app.classify.{Classification, Classifier, LabelList}
import cc.factorie.app.nlp.lemma.TokenLemma
import collection.mutable.ArrayBuffer
import java.io.File
import cc.factorie.util.{ProtectedIntArrayBuffer, LocalDoubleAccumulator}
import cc.factorie.la.Tensor1
import cc.factorie.optimize.ParameterAveraging


class DepParser1(val useLabels: Boolean = true) extends DocumentAnnotator {
  def this(filename:String) = { this(); deserialize(filename) }

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
    val validActionList = domain.filter(a => isAllowedCategory(a.category._1)).toSeq.map(_.intValue).iterator
    private def isAllowedCategory(actionIdx: Int): Boolean = {
      actionIdx match {
        /*Left*/   case 1 => { stack.size > 1 && tree.parentIndex(stack.head) == ParseTree.noIndex && input.size > 0 }
        /*Right*/  case 2 => { input.size > 0 }
        /*Shift*/  case 3 => { input.size > 1 }
        /*Reduce*/ case 4 => { stack.size > 1 && tree.parentIndex(stack.head) != ParseTree.noIndex }
      }
    }
  }

  // The features representing the input and stacks
  object FeaturesDomain extends CategoricalDimensionTensorDomain[(String, Int)] {
    override def stringToCategory(s: String): (String,Int) = {
      val (str, i) = s.splitAt(s.lastIndexOf(","))
      (str.substring(1), Integer.parseInt(i.substring(1,i.size-1)))
    }
  }
  val nullLemma = new TokenLemma(null, "null")
  class Features(val label: Action, stack: ParserStack, input: ParserStack, tree: ParseTree) extends BinaryFeatureVectorVariable[(String, Int)] {
    def domain = FeaturesDomain
    override def skipNonCategories = featuresSkipNonCategories
    import cc.factorie.app.strings.simplifyDigits
    // assumes all locations > 0
    def formFeatures(seq: ParserStack, locations: Seq[Int], tree: ParseTree): Seq[(String, Int)] =   locations.filter(_ < seq.size).map(i => ("sf-" + { if (seq(i) == ParseTree.noIndex || seq(i) == ParseTree.rootIndex) "null" else simplifyDigits(tree.sentence.tokens(seq(i)).string) }, i))
    def lemmaFeatures(seq: ParserStack, locations: Seq[Int], tree: ParseTree): Seq[(String, Int)] =  locations.filter(_ < seq.size).map(i => ("lf-" + { if (seq(i) == ParseTree.noIndex || seq(i) == ParseTree.rootIndex) "null" else tree.sentence.tokens(seq(i)).attr.getOrElse[TokenLemma](nullLemma).value }, i))
    def tagFeatures(seq: ParserStack, locations: Seq[Int], tree: ParseTree): Seq[(String, Int)] =    locations.filter(_ < seq.size).map(i => ("it-" + { if (seq(i) == ParseTree.noIndex || seq(i) == ParseTree.rootIndex) "null" else tree.sentence.tokens(seq(i)).attr[pos.PTBPosLabel].categoryValue }, i))
    def depRelFeatures(seq: ParserStack, locations: Seq[Int], tree: ParseTree): Seq[(String, Int)] = locations.filter(_ < seq.size).map(i => ("sd-" + { if (seq(i) == ParseTree.noIndex || seq(i) == ParseTree.rootIndex) "null" else tree.label(seq(i)).value }, i))
    def lChildDepRelFeatures(seq: ParserStack, locations: Seq[Int], tree: ParseTree): Seq[(String, Int)] = {
      locations.filter(_ < seq.size).flatMap(i => tree.leftChildren(seq(i)).map(t => ("lcd-" + t.parseLabel.value.toString(), i))) }
    def rChildDepRelFeatures(seq: ParserStack, locations: Seq[Int], tree: ParseTree): Seq[(String, Int)] = {
      locations.filter(_ < seq.size).flatMap(i => tree.rightChildren(seq(i)).map(t => ("rcd-" + t.parseLabel.value.toString(), i))) }
    // Initialize the Features value
    this ++= formFeatures(stack, Seq(0,1), tree)
    this ++= formFeatures(input, Seq(0,1), tree)
    // TODO We don't have have a good lemma annotator for new text
    this ++= lemmaFeatures(stack, Seq(0,1), tree) //
    this ++= lemmaFeatures(input, Seq(0,1), tree) //
    this ++= tagFeatures(stack, Seq(0,1,2,3), tree)
    this ++= tagFeatures(input, Seq(0,1,2,3), tree)
    this ++= depRelFeatures(stack, Seq(0), tree)
    this ++= depRelFeatures(input, Seq(0), tree)
    this ++= lChildDepRelFeatures(stack, Seq(0), tree)
    this ++= lChildDepRelFeatures(input, Seq(0), tree)
    this ++= rChildDepRelFeatures(stack, Seq(0), tree)
    this ++= rChildDepRelFeatures(input, Seq(0), tree)
  }
  var featuresSkipNonCategories = true
  
  // The model scoring an Action in the context of Features
  val model = new Model with Weights {
    object family extends DotFamilyWithStatistics2[Action, Features] {
      lazy val weightsTensor = new la.DenseTensor2(ActionDomain.size, FeaturesDomain.dimensionSize)
    }
    lazy val weights: la.Tensors = new la.Tensors(Seq((family, family.weightsTensor)))
    def factors(v:Var) = v match {
      case action:Action => new family.Factor(action, action.features)
      case _ => Nil
    }
    def factor(a:Action) = family.Factor(a, a.features)
  }
  
  // Action implementations
  def applyLeftArc(tree: ParseTree, stack: ParserStack, input: ParserStack, relation: String = ""): Unit = {
    val child: Int = stack.pop()
    val parent: Int = input.head
    tree.setParent(child, parent)
    tree.label(child).setCategory(relation)(null)
  }
  def applyRightArc(tree: ParseTree, stack: ParserStack, input: ParserStack, relation: String = ""): Unit = {
    val child: Int = input.pop()
    val parent: Int = stack.head
    tree.setParent(child, parent)
    tree.label(child).setCategory(relation)(null)
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
    val weights = model.family.weightsTensor
    val prediction = weights * v.features.tensor.asInstanceOf[Tensor1]
    (v, v.domain.categories(v.validActionList.maxBy(prediction(_))))
  }

  def parse(s: Sentence): Seq[Action] = {
    val actionsPerformed = new ArrayBuffer[Action]
    //s.attr.remove[ParseTree]
    val tree = s.attr.getOrElseUpdate(new ParseTree(s))
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
  def serialize(filename: String) {
    import CubbieConversions._
    val file = new File(filename); if (file.getParentFile != null && !file.getParentFile.exists) file.getParentFile.mkdirs()
    BinarySerializer.serialize(ActionDomain, FeaturesDomain.dimensionDomain, model, file)
  }
  def deserialize(filename: String) {
    import CubbieConversions._
    val file = new File(filename)
    assert(file.exists(), "Trying to load non-existent file: '" +file)
    BinarySerializer.deserialize(ActionDomain, FeaturesDomain.dimensionDomain, model, file)
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
        actionLabels.append(action)
        applyAction(tree, stack, input, action.categoryValue._1, action.categoryValue._2)
        done = true
      }
      else if (inputIdx == stackIdxParent) {
        val action = new Action((1, { if (useLabels) origTree.label(stackIdx).categoryValue else "" }), stack, input, tree) // LeftArc
        actionLabels.append(action)
        applyAction(tree, stack, input, action.categoryValue._1, action.categoryValue._2)
        done = true
      }
      while ((stack.size > 1) && (tree.parentIndex(stack.head) < 0) && origTree.children(stack.head).length == tree.children(stack.head).length) {
          val action = new Action((4, ""), stack, input, tree) // Reduce
          actionLabels.append(action)
          applyAction(tree, stack, input, action.categoryValue._1, action.categoryValue._2)
          done = true
      }
      if (!done) {
        val action = new Action((3, ""), stack, input, tree) // Shift
        actionLabels.append(action)
        applyAction(tree, stack, input, action.categoryValue._1, action.categoryValue._2)
      }
    }
    actionLabels
  }
  class Example(featureVector:la.Tensor1, targetLabel:Int) extends optimize.Example[Weights] {
    // similar to GLMExample, but specialized to DepParser.model
    def accumulateExampleInto(ignoredModel:Weights, gradient:la.TensorsAccumulator, value:util.DoubleAccumulator): Unit = {
      val weights = model.family.weightsTensor
      val prediction = weights * featureVector
      val (obj, grad) = optimize.ObjectiveFunctions.logMultiClassObjective(prediction, targetLabel)
      if (value ne null) value.accumulate(obj)
      if (gradient ne null) gradient.accumulateOuter(model.family, grad, featureVector)
    }
  }
  def train(trainSentences:Iterable[Sentence], testSentences:Iterable[Sentence], devSentences:Iterable[Sentence]): Unit = {
    featuresSkipNonCategories = false
    println("Generating trainActions...")
    val trainActions = new LabelList[Action, Features]((action: Action) => action.features)
    val testActions = new LabelList[Action, Features]((action: Action) => action.features)
    for (s <- trainSentences) trainActions ++= generateTrainingLabels(s)
    for (s <- testSentences) testActions ++= generateTrainingLabels(s)
    println("%d actions.  %d input features".format(ActionDomain.size, FeaturesDomain.dimensionSize))
    println("%d parameters.  %d tensor size.".format(ActionDomain.size * FeaturesDomain.dimensionSize, model.family.weightsTensor.length))
    println("Generating examples...")
    val examples = trainActions.map(a => new Example(a.features.value.asInstanceOf[la.Tensor1], a.targetIntValue))
    freezeDomains()
    println("Training...")
    val opt = new cc.factorie.optimize.AdaGrad(rate=1.0) with ParameterAveraging
    val trainer = new optimize.HogwildTrainer[Weights](model, opt, maxIterations = 10)
    for (iteration <- 0 until 10) {
      trainer.processExamples(examples)
      // trainActions.foreach()
      // testActions.foreach(classifier.classify(_)); println("Test action accuracy = "+HammingObjective.accuracy(testActions))
      opt.setWeightsToAverage(model.weights)
      println("Predicting train set..."); trainSentences.par.foreach { s => parse(s) } // Was par
      println("Predicting test set...");  testSentences.par.foreach { s => parse(s) } // Was par
      println("Training label accuracy = "+HammingObjective.accuracy(trainSentences.flatMap(s => s.parse.labels)))
      println(" Testing label accuracy = "+HammingObjective.accuracy(testSentences.flatMap(s => s.parse.labels)))
      println("Training arc accuracy = "+(trainSentences.map((s:Sentence) => s.parse.numParentsCorrect).sum.toDouble / trainSentences.map(_.tokens.length).sum))
      println(" Testing arc accuracy = "+(testSentences.map((s:Sentence) => s.parse.numParentsCorrect).sum.toDouble / testSentences.map(_.tokens.length).sum))
      println("Saving model...")
      parser.serialize(opts.model.value + "-iter-"+iteration)
      opt.unSetWeightsToAverage(model.weights)
    }
    println("Finished training.")
    opt.setWeightsToAverage(model.weights)
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


// Driver for training
object DepParser1 {

  def main(args: Array[String]): Unit = {
    object opts extends cc.factorie.util.DefaultCmdOptions {
      val trainFile = new CmdOption("train", "", "FILES", "CoNLL-2008 train file.")
      //val devFile =   new CmdOption("dev", "", "FILES", "CoNLL-2008 dev file")
      val testFile =  new CmdOption("test", "", "FILES", "CoNLL-2008 test file.")
      val unlabeled  = new CmdOption("unlabeled", false, "BOOLEAN", "Whether to ignore labels.")
      val model      = new CmdOption("model", "parser-model", "FILE", "File in which to save the trained model.")
      val outputDir  = new CmdOption("output", ".", "DIR", "Directory in which to save the parsed output (to be scored by eval.pl).")
      val warmModel  = new CmdOption("warm", "parser-model", "FILE", "File from which to read a model for warm-start training.")
    }
    opts.parse(args)
    
    val parser = new DepParser1(!opts.unlabeled.value)

    if (opts.warmModel.wasInvoked) {
      print("Loading " + opts.warmModel.value + " as a warm-start model.....")
      parser.deserialize(opts.warmModel.value)
      println("Finished loading warm-start model.")
    }

    val trainDoc = LoadOntonotes5.fromFilename(opts.trainFile.value).head
    val testDoc = LoadOntonotes5.fromFilename(opts.testFile.value).head
    
    // Train
    parser.train(trainDoc.sentences, testDoc.sentences, null)
    // Test
    parser.freezeDomains()
    
    // Print accuracy diagnostics
    println("Predicting train set..."); trainDoc.sentences.foreach { s => parser.parse(s) } // Was par
    println("Predicting test set...");  testDoc.sentences.foreach { s => parser.parse(s) } // Was par
    println("Training label accuracy = "+HammingObjective.accuracy(trainDoc.sentences.flatMap(s => s.parse.labels)))
    println(" Testing label accuracy = "+HammingObjective.accuracy(testDoc.sentences.flatMap(s => s.parse.labels)))
    println("Training arc accuracy = "+(trainDoc.sentences.map((s:Sentence) => s.parse.numParentsCorrect).sum.toDouble / trainDoc.tokens.length))
    println(" Testing arc accuracy = "+(testDoc.sentences.map((s:Sentence) => s.parse.numParentsCorrect).sum.toDouble / testDoc.tokens.length))

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