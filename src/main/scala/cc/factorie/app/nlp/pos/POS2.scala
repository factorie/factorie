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

package cc.factorie.app.nlp.pos

import cc.factorie._
import cc.factorie.util._
import cc.factorie.la._
import cc.factorie.optimize._
import cc.factorie.app.nlp._

class POS2 extends Infer with util.FastLogging {
  def this(savedModelDir:String) = { this(); PosModel.load(savedModelDir)}
  
  object PosFeaturesDomain extends CategoricalTensorDomain[String]
  class PosFeatures(val token:Token) extends BinaryFeatureVectorVariable[String] {
    def domain = PosFeaturesDomain
    //override def skipNonCategories = true
  }

  var useSentenceBoundaries = false
  class PosModel extends TemplateModel {
    // Bias term on each individual label 
    val biasTemplate = new DotTemplateWithStatistics1[PosLabel] {
      lazy val weights = new la.DenseTensor1(PosDomain.size)
    }
    // Factor between label and observed token
    val localTemplate = new DotTemplateWithStatistics2[PosLabel,PosFeatures] {
      lazy val weights = new la.DenseTensor2(PosDomain.size, PosFeaturesDomain.dimensionSize)
      def unroll1(label: PosLabel) = Factor(label, label.token.attr[PosFeatures])
      def unroll2(tf: PosFeatures) = Factor(tf.token.attr[PosLabel], tf)
    }
    // Transition factors between two successive labels
    val transTemplate = new DotTemplateWithStatistics2[PosLabel, PosLabel] {
      lazy val weights = new la.DenseTensor2(PosDomain.size, PosDomain.size)
      def unroll1(label: PosLabel) = {
        if (useSentenceBoundaries) {
          if (label.token.sentenceHasPrev) Factor(label.token.sentencePrev.attr[PosLabel], label) else Nil
        } else {
          if (label.token.hasPrev) Factor(label.token.prev.attr[PosLabel], label) else Nil
        }
      }
      def unroll2(label: PosLabel) = {
        if (useSentenceBoundaries) {
          if (label.token.sentenceHasNext) Factor(label, label.token.sentenceNext.attr[PosLabel]) else Nil
        } else {
          if (label.token.hasNext) Factor(label, label.token.next.attr[PosLabel]) else Nil
        }
      }
    }
    this += biasTemplate
    this += localTemplate
    this += transTemplate
    
    override def factors(labels:Iterable[Variable]): Iterable[Factor] = labels match {
      case labels:IndexedSeq[PosLabel] => {
        //println("POS2.model.factors labels.size="+labels.size)
        val result = new collection.mutable.ArrayBuffer[Factor]
        var prevLabel: PosLabel = null
        for (label <- labels) {
          result += biasTemplate.Factor(label)
          result += localTemplate.Factor(label, label.token.attr[PosFeatures])
          if (prevLabel ne null) result += transTemplate.Factor(prevLabel, label)
          prevLabel = label
        }
        result
      }
    }
    // TODO Add def factors(Sentence) for convenience
  }
  object model extends PosModel
  
  def initPosAttr(sentenceTokens:Seq[Token]): Unit = {
    if (sentenceTokens.length < 1 || sentenceTokens.head.attr[PosFeatures] != null) return
    var addedPosFeatures = false
    for (token <- sentenceTokens) {
      if (token.attr[PosLabel] eq null) token.attr += new PosLabel(token, PosDomain.category(0)) // initial value doesn't matter
      if (token.attr[PosFeatures] eq null) {
        addedPosFeatures = true
        val rawWord = token.string
        val word = cc.factorie.app.strings.simplifyDigits(rawWord)
        val features = token.attr += new PosFeatures(token)
        features += "W="+word.toLowerCase
        features += "SHAPE="+cc.factorie.app.strings.stringShape(rawWord, 2)
        features += "SUFFIX3="+word.takeRight(3)
        features += "PREFIX3="+word.take(3)
        if (token.isPunctuation) features += "PUNCTUATION"
      }
    }
    //sentenceTokens.foreach(t => println(t.hashCode+" "+t.attr[PosFeatures])); println()
    if (addedPosFeatures)
      cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions(sentenceTokens, (t:Token)=> t.attr[PosFeatures], List(1), List(-1))
  }
  def initPosAttr(document:Document): Unit = initPosAttr(document.tokens) // TODO Note that this always ignores Sentence boundaries, because Observation.addNeighborFeatureConjunctions is broken in this regard.
  
  class PosLikelihoodExample(val labels:IndexedSeq[PosLabel]) extends Example[PosModel] {
    def accumulateExampleInto(model: PosModel, gradient: WeightsTensorAccumulator, value: DoubleAccumulator, margin:DoubleAccumulator): Unit = {
      if (labels.size == 0) return
      val summary = BP.inferChainSum(labels, model)
      if (value != null) { var incr = 0.0; summary.factors.foreach(f => incr += f.assignmentScore(TargetAssignment)); value.accumulate(incr - summary.logZ) } 
      if (gradient != null) {
        summary.factors.asInstanceOf[Iterable[DotFamily#Factor]].foreach(factor => {
          gradient.accumulate(factor.family, factor.assignmentStatistics(TargetAssignment))
          gradient.accumulate(factor.family, summary.marginalTensorStatistics(factor), -1.0)
        })
      }
    }
  }
  
  def train(trainDocuments:Iterable[Document], testDocuments:Iterable[Document]): Unit = {
    model.transTemplate.limitedDiscreteValues12.zero
    for (document <- trainDocuments ++ testDocuments) {
      initPosAttr(document)
      for (factor <- model.transTemplate.factors(document.tokens.map(_.attr[PosLabel]))) factor.asInstanceOf[Factor2[DiscreteVar,DiscreteVar]].addLimitedDiscreteCurrentValues12
    }
    println("POS.train sparse transitions "+model.transTemplate.limitedDiscreteValues12.activeDomainSize+" out of "+PosDomain.size*PosDomain.size)
//    // Make two iterations of SampleRank training
//    val trainer1 = new SampleRankTrainer(new GibbsSampler(model, HammingLossObjective))
//    val labels = trainDocuments.flatMap(_.tokens.map(_.attr[PosLabel]))
//    labels.foreach(_.setRandomly())
//    println("First label factors: "+model.factors(labels.head))
//    println("Third sentence factors"+model.factors(trainDocuments.toSeq(3).sentences(3).tokens.map(_.attr[PosLabel])))
//    for (i <- 1 to 4) {
//      //for (document <- trainDocuments) trainer1.processContexts(document.tokens.map(_.attr[PosLabel]))
//      trainer1.processContexts(labels)
//      printAccuracy("SampleRank Train", trainDocuments)
//    }
    // Then train by Likelihood LBFGS to convergence
    val examples = for (document <- trainDocuments; sentence <- document.sentences) yield new PosLikelihoodExample(sentence.tokens.map(_.attr[PosLabel]))
    val trainer2 = new BatchTrainer(model)
    while (!trainer2.isConverged) {
      trainer2.processExamples(examples)
      printAccuracy("Train", trainDocuments)
    }
    logger.info("FINAL")
    printAccuracy("Train", trainDocuments)
    printAccuracy("Test ", testDocuments)
  }
  
  def printAccuracy(msg:String, documents:Iterable[Document]): Unit = {
    //documents.foreach(apply(_))
    //val icm = new IteratedConditionalModes[PosLabel](model, null); for (doc <- documents; token <- doc.tokens) icm.process(token.attr[PosLabel])
    for (doc <- documents) BP.inferChainMax(doc.tokens.map(_.attr[PosLabel]), model)
    logger.info(msg+" token accuracy = "+HammingObjective.accuracy(documents.flatMap(_.tokens.map(_.attr[PosLabel]))))
  }

  /** Predict the part-of-speech tag of the words in the document, and store it in attr[PosLabel] */
  def apply(document:Document): Unit = {
    initPosAttr(document)
    if (useSentenceBoundaries) for (sentence <- document.sentences) BP.inferChainMax(sentence.map(_.attr[PosLabel]), model)
    else BP.inferChainMax(document.tokens.map(_.attr[PosLabel]), model)
  }
  
  def apply(labels:Seq[PosLabel]): BPSummary = {
    initPosAttr(labels.map(_.token))
    BP.inferChainMax(labels, model)
  }
  
  override def infer(variables:Iterable[Variable], model:Model, summary:Summary[Marginal] = null): Option[Summary[Marginal]] = variables match {
    case labels:Seq[PosLabel] if (labels.forall(_.isInstanceOf[PosLabel])) => Some(apply(labels))
    case _ => None
  }

  // Add run as server
}




// For example:
// POS1 --train /Users/mccallum/research/data/ie/ner2003/eng.train --test /Users/mccallum/research/data/ie/ner2003/eng.testa --model pos.fac
// POS1 --model pos.fac --run ~/research/data/text/nipstxt/nips11/0620.txt
object POS2 extends POS2 {
  def main(args: Array[String]): Unit = {
    object opts extends cc.factorie.util.DefaultCmdOptions {
      val trainFile =    new CmdOption("train", "eng.train", "FILE", "CoNLL 2003 format file from which to get training data.")
      val testFile =     new CmdOption("test", "eng.testa", "FILE", "CoNLL 2003 format file from which to get testing data.")
      val modelDir =     new CmdOption("model", "pos.fac", "DIR", "Directory in which to save the trained model.")
      val runFiles =     new CmdOption("run", List("input.txt"), "FILE...", "Plain text files from which to get data on which to run.")
    }
    opts.parse(args)
    if (opts.trainFile.wasInvoked) train()
    else if (opts.runFiles.wasInvoked) run()
    else throw new Error("Must use either --train or --run.")
        
    def train(): Unit = {
      // Read in the data
      val trainDocuments = LoadConll2003.fromFilename(opts.trainFile.value).take(10)
      val testDocuments = LoadConll2003.fromFilename(opts.testFile.value).take(10)

      // Add features for POS
      trainDocuments.foreach(initPosAttr(_))
      testDocuments.foreach(initPosAttr(_))
      println("Example Token features")
      println(trainDocuments(3).tokens.take(10).map(_.attr[PosFeatures].toString).mkString("\n"))
      println("Num TokenFeatures = "+PosFeaturesDomain.dimensionSize)
      
      this.train(trainDocuments, testDocuments)
    
      if (opts.modelDir.wasInvoked)
        PosModel.save(opts.modelDir.value)
    }
    
    
    def run(): Unit = {
      PosModel.load(opts.modelDir.value)
      for (filename <- opts.runFiles.value) {
        val document = new Document("", io.Source.fromFile(filename).getLines.mkString("\n"))
        segment.Tokenizer.process(document)
        segment.SentenceSegmenter.process(document)
        initPosAttr(document)
        apply(document)
        for (token <- document.tokens)
          println("%s %s".format(token.string, token.attr[PosLabel].categoryValue))
      }
    }
  }

  
}