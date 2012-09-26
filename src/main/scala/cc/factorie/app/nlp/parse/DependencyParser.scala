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

/**
 * A Nivre-style, arc-eager, shift-reduce dependency parser.
 *
 * @author Brian Martin
 */

import cc.factorie._
import app.classify.{Classification, Classifier, LabelList}
//import bp.ParallelTrainer
import cc.factorie.app.nlp._
import collection.mutable.ArrayBuffer

object Actions {
  def applyLeftArc(tree: ParseTree, stack: ArrayBuffer[Int], input: ArrayBuffer[Int], relation: String = ""): Unit = {
    val child: Int = stack.remove(0)
    val parent: Int = input(0)
    tree.setParent(child, parent)
    tree.label(child).setCategory(relation)(null)
  }

  def applyRightArc(tree: ParseTree, stack: ArrayBuffer[Int], input: ArrayBuffer[Int], relation: String = ""): Unit = {
    val child: Int = input.remove(0)
    val parent: Int = stack(0)
    tree.setParent(child, parent)
    tree.label(child).setCategory(relation)(null)
    stack.prepend(child)
  }

  def applyShift(tree: ParseTree, stack: ArrayBuffer[Int], input: ArrayBuffer[Int], relation: String = ""): Unit =
    stack.prepend(input.remove(0))

  def applyReduce(tree: ParseTree, stack: ArrayBuffer[Int], input: ArrayBuffer[Int], relation: String = ""): Unit =
    stack.remove(0)
}

object ActionDomain extends CategoricalDomain[(Int, String)] {
  dimensionDomain.string2T = {
    (s: String) => {
      val (i, str) = s.splitAt(s.indexOf(","))
      (Integer.parseInt(i.substring(1,2)), str.substring(1,str.size-1)) // assumes single digit actionIdx
    }
  }
}
class ActionLabel(targetAction: (Int, String), stack: ArrayBuffer[Int], input: ArrayBuffer[Int], tree: ParseTree) extends LabelVariable(targetAction) {
  import ParseFeatureExtractors._

  def domain = ActionDomain

  val features = new ShiftReduceDependencyParserFeatures(this)
  features ++= (
    formFeatures(stack, Seq(0,1), tree) ++
    formFeatures(input, Seq(0,1), tree) ++
    lemmaFeatures(stack, Seq(0,1), tree) ++
    lemmaFeatures(input, Seq(0,1), tree) ++
    tagFeatures(stack, Seq(0,1,2,3), tree) ++
    tagFeatures(input, Seq(0,1,2,3), tree) ++
    depRelFeatures(stack, Seq(0), tree) ++
    depRelFeatures(input, Seq(0), tree) ++
    lChildDepRelFeatures(stack, Seq(0), tree) ++
    lChildDepRelFeatures(input, Seq(0), tree) ++
    rChildDepRelFeatures(stack, Seq(0), tree) ++
    rChildDepRelFeatures(input, Seq(0), tree)
  )

  override def settings = new SettingIterator {
    private var validActionList: Iterator[ActionLabel.this.Value] = domain.filter(a => isAllowedCategory(a.category._1)).toSeq.iterator
    def hasNext = validActionList.hasNext
    def next(difflist:DiffList) = { val d = new DiffList; set(validActionList.next)(d); d }
    def reset = validActionList = domain.filter(a => isAllowedCategory(a.category._1)).toSeq.iterator
    override def variable: ActionLabel.this.type = ActionLabel.this
  }

  private def isAllowedCategory(actionIdx: Int): Boolean = {
    actionIdx match {
      /*Left*/   case 1 => { stack.size > 1 && tree.parentIndex(stack(0)) == ParseTree.noIndex && input.size > 0 }
      /*Right*/  case 2 => { input.size > 0 }
      /*Shift*/  case 3 => { input.size > 1 }
      /*Reduce*/ case 4 => { stack.size > 1 && tree.parentIndex(stack(0)) != ParseTree.noIndex }
    }
  }
}

// define the feature extractors
object ParseFeatureExtractors {
  import cc.factorie.app.strings.simplifyDigits
  val nullLemma = new LoadConll2008.Lemma("null")
  // assumes all locations > 0
  def formFeatures(seq: ArrayBuffer[Int], locations: Seq[Int], tree: ParseTree): Seq[(String, Int)] =   locations.filter(_ < seq.size).map(i => ("sf-" + { if (seq(i) == ParseTree.noIndex || seq(i) == ParseTree.rootIndex) "null" else simplifyDigits(tree.sentence.tokens(seq(i)).string) }, i))
  def lemmaFeatures(seq: ArrayBuffer[Int], locations: Seq[Int], tree: ParseTree): Seq[(String, Int)] =  locations.filter(_ < seq.size).map(i => ("lf-" + { if (seq(i) == ParseTree.noIndex || seq(i) == ParseTree.rootIndex) "null" else tree.sentence.tokens(seq(i)).attr.getOrElse[LoadConll2008.Lemma](nullLemma).lemma }, i))
  def tagFeatures(seq: ArrayBuffer[Int], locations: Seq[Int], tree: ParseTree): Seq[(String, Int)] =    locations.filter(_ < seq.size).map(i => ("it-" + { if (seq(i) == ParseTree.noIndex || seq(i) == ParseTree.rootIndex) "null" else tree.sentence.tokens(seq(i)).posLabel.value.toString }, i))
  def depRelFeatures(seq: ArrayBuffer[Int], locations: Seq[Int], tree: ParseTree): Seq[(String, Int)] = locations.filter(_ < seq.size).map(i => ("sd-" + { if (seq(i) == ParseTree.noIndex || seq(i) == ParseTree.rootIndex) "null" else tree.label(seq(i)).value }, i))
  def lChildDepRelFeatures(seq: ArrayBuffer[Int], locations: Seq[Int], tree: ParseTree): Seq[(String, Int)] = {
    locations.filter(_ < seq.size).flatMap(i => tree.leftChildren(seq(i)).map(t => ("lcd-" + t.parseLabel.value.toString(), i))) }
  def rChildDepRelFeatures(seq: ArrayBuffer[Int], locations: Seq[Int], tree: ParseTree): Seq[(String, Int)] = {
    locations.filter(_ < seq.size).flatMap(i => tree.rightChildren(seq(i)).map(t => ("rcd-" + t.parseLabel.value.toString(), i))) }
}

// define the model
object ParserFeaturesDomain extends CategoricalTensorDomain[(String, Int)] {
  // for deserialization
  dimensionDomain.string2T = {
    (s: String) => {
      val (str, i) = s.splitAt(s.lastIndexOf(","))
      (str.substring(1), Integer.parseInt(i.substring(1,i.size-1)))
    }
  }
}
class ShiftReduceDependencyParserFeatures(val label: ActionLabel) extends BinaryFeatureVectorVariable[(String, Int)] {
  def domain = ParserFeaturesDomain
  override def skipNonCategories = ActionModel.skipNonCategories
}

object ActionModel extends TemplateModel {
  var skipNonCategories = false

  val localTemplate = new DotTemplateWithStatistics2[ActionLabel, ShiftReduceDependencyParserFeatures] {
    //override def statisticsDomains = ((ActionDomain, ParserFeaturesDomain))
    lazy val weights = new la.DenseTensor2(ActionDomain.size, ParserFeaturesDomain.dimensionSize)
    def unroll1(label: ActionLabel) = Factor(label, label.features)
    def unroll2(features: ShiftReduceDependencyParserFeatures) = Factor(features.label, features)
  }

  this += localTemplate
}

object ShiftReduceDependencyParser {
  import Actions._

  var useLabels = true

  def load(modelFile: String = "dep-parsing.fac") = {
    ActionModel.load(modelFile)
    ActionModel.skipNonCategories = true
  }

  var modelFile = "dep-parsing.fac"
  def save(extra: String = "") = ActionModel.save(modelFile + extra)

  def main(args: Array[String]) {
    object opts extends cc.factorie.util.DefaultCmdOptions {
      val trainFile = new CmdOption("train", "", "FILE", "CoNLL-2008 train file.")
      val devFile =   new CmdOption("dev", "", "FILE", "CoNLL-2008 dev file")
      val testFile =  new CmdOption("test", "", "FILE", "CoNLL-2008 test file.")
      val unlabeled = new CmdOption("unlabeled", "", "BOOLEAN", "Whether to ignore labels.")
      val modelDir =  new CmdOption("model", "", "DIR", "Directory in which to save the trained model.")
      val outputDir = new CmdOption("output", "", "DIR", "Directory in which to save the parsed output (to be scored by eval.pl).")
      val warmStartModel = new CmdOption("warmStartModel", "", "DIR", "dep-parsing.fac to warm-start more training with.")
    }
    opts.parse(args)

    modelFile = opts.modelDir.value + "/dep-parsing.fac"
    if (opts.unlabeled.wasInvoked)
      useLabels = false

    if (opts.warmStartModel.wasInvoked) {
      print("Loading " + opts.warmStartModel.value + " as a warm-start model.....")
      load(opts.warmStartModel.value)
      print("DONE\n")
    }

    val trainDocs = LoadConll2008.fromFilename(opts.trainFile.value).head
    val testDocs  = LoadConll2008.fromFilename(opts.testFile.value).head
    val devDocs   = LoadConll2008.fromFilename(opts.devFile.value).head

    // create classifier
    val classifier = new Classifier[ActionLabel] {
      val model = ActionModel
      val labelDomain = ActionDomain
      override def classification(label: ActionLabel) = {
        val probs = label.proportions(model)
        val proportions = Array.ofDim[Double](labelDomain.size)
        label.settings.foreach(s => proportions(label.intValue) = probs(label.intValue))
        // normalize proportions
        val n = proportions.sum
        for (i <- 0 until proportions.size)
          proportions(i) /= n
        val dp = new DenseProportions1(proportions)
        new Classification(label, this, dp)
      }
    }

    def train(): Unit = {
      println("starting training...")
      println("generating labels...")
      var actions = new LabelList[ActionLabel,ShiftReduceDependencyParserFeatures]((action: ActionLabel) => action.features)
      for (s <- trainDocs.sentences)
        actions ++= generateTrainingLabels(s)
      //val trainer = new LogLinearMaximumLikelihood(ActionModel, modelFile = modelFile)
      //trainer.processAll(actions.map(List(_)))
      println("generating pieces...")
      actions = null // collect garbage
      throw new Error("Not yet implemented in new cc.factorie.optimize scheme")
//      val pieces = actions.par.map(v => bp.TransientModelPiece(ActionModel, Seq(v))).seq
//      println("generating optimizable...")
//      val optimizable = new ParallelTrainer(ActionModel, pieces)
//      println("optimizing...")
//      new optimize.LimitedMemoryBFGS(optimizable) {
//        tolerance = 1e-5
//        override def postIteration(i: Int): Unit = {
//          println("finished iteration " + i)
//          save("-iteration-" + i)
//          test()
//          writeResults("-iteration-" + i)
//        }
//      }.optimize()
    }

    def test(): Unit = {
      ActionModel.skipNonCategories = true
      val predictor = predict(classifier)(_, _, _)
      println("Predicting train set..."); trainDocs.sentences.par.foreach { s => parse(s, predictor) }
      println("Predicting test set...");  testDocs.sentences.par.foreach { s => parse(s, predictor) }
      println("Predicting dev set...");   devDocs.sentences.par.foreach { s => parse(s, predictor) }
      ActionModel.skipNonCategories = false
    }

    def writeResults(extra: String): Unit = {
      println("Writing results...")
      WriteConll2008.toFile(opts.outputDir.value + "/dep.train" + extra, trainDocs, opts.trainFile.value)
      WriteConll2008.toFile(opts.outputDir.value + "/dep.test" + extra, testDocs, opts.testFile.value)
      WriteConll2008.toFile(opts.outputDir.value + "/dep.dev" + extra, devDocs, opts.devFile.value)
    }

    train()
    test()
    writeResults("-final")

  }

  def applyAction(tree: ParseTree, stack: ArrayBuffer[Int], input: ArrayBuffer[Int], actionIdx: Int, relation: String) {
    actionIdx match {
      case 1 => applyLeftArc(tree,stack,input,relation)
      case 2 => applyRightArc(tree,stack,input,relation)
      case 3 => applyShift(tree,stack,input,relation)
      case 4 => applyReduce(tree,stack,input,relation)
      case _ => throw new Error("ActionLabel category value is invalid.")
    }
  }

  def parse(s: Sentence, predict: (ArrayBuffer[Int], ArrayBuffer[Int], ParseTree) => (ActionLabel, (Int, String))): Seq[ActionLabel] = {
    val actionsPerformed = new ArrayBuffer[ActionLabel]
    s.attr.remove[ParseTree]
    val tree = new ParseTree(s)
    s.attr += tree
    val stack = ArrayBuffer[Int](ParseTree.rootIndex)
    val input = ArrayBuffer[Int](); for (i <- (0 until s.length)) input.append(i)
    while(input.nonEmpty) {
      val (action,  (actionIdx, relation)) = predict(stack, input, tree)
      actionsPerformed.append(action)
      applyAction(tree, stack, input, actionIdx, relation)
      while (input.isEmpty && stack.size > 1) {
        if (tree.parentIndex(stack.head) == ParseTree.noIndex)
          input.append(stack.remove(0))
        else
          stack.remove(0)
      }
    }
    actionsPerformed
  }

  def predict(classifier: Classifier[ActionLabel])(stack: ArrayBuffer[Int], input: ArrayBuffer[Int], tree: ParseTree): (ActionLabel, (Int, String)) = {
    val v = new ActionLabel((4, ""), stack, input, tree)
    (v, { maxClassifierModelScoreSetting(v, classifier); v.categoryValue })
  }

  def trainClassifier(learner: app.classify.ClassifierTrainer)(sentences: Seq[Sentence]): Classifier[ActionLabel] = {
    val actions = new LabelList[ActionLabel,ShiftReduceDependencyParserFeatures]((action: ActionLabel) => action.features)
    for (s <- sentences)
      actions ++= generateTrainingLabels(s)
    learner.train(actions)
  }

  def maxClassifierModelScoreSetting(actionLabel: ActionLabel, classifier: Classifier[ActionLabel]): ActionLabel = {
    classifier.classify(actionLabel)
    actionLabel
  } 
    
  //maxModelScoreSetting(actionLabel, classifier.model)
//  def maxModelScoreSetting(actionLabel: ActionLabel, model: Model = ActionModel): ActionLabel = {
//    var (maxScore, maxSetting) = (Double.NegativeInfinity, actionLabel.categoryValue)
//    for (s <- actionLabel.settings) { // just iterating over
//      val score = model.score(Seq(actionLabel))
//      if (score > maxScore) { maxScore = score; maxSetting = actionLabel.categoryValue }
//    }
//    actionLabel.set(actionLabel.domain.value(maxSetting))(null)
//    actionLabel
//  }

  def generateTrainingLabels(ss: Seq[Sentence]): Seq[Seq[ActionLabel]] = ss.map(generateTrainingLabels(_))
  def generateTrainingLabels(s: Sentence): Seq[ActionLabel] = {
    val origTree = s.attr[ParseTree]
    val tree = new ParseTree(s)
    val stack = ArrayBuffer[Int](ParseTree.rootIndex)
    val input = ArrayBuffer[Int](); for (i <- 0 until s.length) input.append(i)
    val actionLabels = ArrayBuffer[ActionLabel]()
    while (input.nonEmpty) {
      var done = false
      val inputIdx = input(0)
      val inputIdxParent = origTree.parentIndex(inputIdx)
      val stackIdx = stack(0)
      val stackIdxParent = {
        if (stackIdx == ParseTree.rootIndex) -3
        else origTree.parentIndex(stackIdx)
      }
      if (inputIdxParent == stackIdx) {
        val action = new ActionLabel((2, { if (useLabels) origTree.label(inputIdx).categoryValue else "" }), stack, input, tree) // RightArc
        actionLabels.append(action)
        applyAction(tree, stack, input, action.categoryValue._1, action.categoryValue._2)
        done = true
      }
      else if (inputIdx == stackIdxParent) {
        val action = new ActionLabel((1, { if (useLabels) origTree.label(stackIdx).categoryValue else "" }), stack, input, tree) // LeftArc
        actionLabels.append(action)
        applyAction(tree, stack, input, action.categoryValue._1, action.categoryValue._2)
        done = true
      }
      else {
        for (si <- stack.drop(1);
             if (inputIdx != ParseTree.rootIndex &&
               ((inputIdxParent == si) || (inputIdx == { if (si == ParseTree.rootIndex) -3 // -3 doesn't conflict with noIndex or rootIndex //ParseTree.noIndex
                                                         else  origTree.parentIndex(si) })))) {// is the -2 right here?
          val action = new ActionLabel((4, ""), stack, input, tree) // Reduce
          actionLabels.append(action)
          applyAction(tree, stack, input, action.categoryValue._1, action.categoryValue._2)
          done = true
        }
      }
      if (!done) {
        val action = new ActionLabel((3, ""), stack, input, tree) // Shift
        actionLabels.append(action)
        applyAction(tree, stack, input, action.categoryValue._1, action.categoryValue._2)
      }
    }
    actionLabels
  }

}
