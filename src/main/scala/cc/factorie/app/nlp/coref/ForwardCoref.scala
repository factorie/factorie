/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie.app.nlp.coref

import cc.factorie.app.nlp.wordnet.WordNet
import cc.factorie.app.nlp.{Token, Document, DocumentAnnotator}
import java.util.concurrent.ExecutorService
import cc.factorie.optimize._
import java.io._
import cc.factorie.util._
import scala.collection.mutable.ArrayBuffer
import cc.factorie.app.nlp.phrase._
import cc.factorie.app.nlp.pos.PennPosTag

/**Forward Coreference on Proper Noun, Pronoun and Common Noun Mentions*/
class ParseForwardCoref extends ForwardCoref {
  override def prereqAttrs: Seq[Class[_]] = ParseAndNerBasedPhraseFinder.prereqAttrs.toSeq ++ ForwardCoref.prereqAttrs
  override def annotateMentions(document:Document): Unit = {
    if(document.coref.mentions.isEmpty) ParseAndNerBasedPhraseFinder.getPhrases(document).foreach(document.coref.addMention)
    document.coref.mentions.foreach(mention => NounPhraseEntityTypeLabeler.process(mention.phrase))
    document.coref.mentions.foreach(mention => NounPhraseGenderLabeler.process(mention.phrase))
    document.coref.mentions.foreach(mention => NounPhraseNumberLabeler.process(mention.phrase))
  }
}

object ParseForwardCoref extends ParseForwardCoref {
  deserialize(new DataInputStream(ClasspathURL[ParseForwardCoref](".factorie").openConnection().getInputStream))
}

/** Forward Coreference on Ner and Pronoun Mentions*/
class NerForwardCoref extends ForwardCoref {
  override def prereqAttrs: Seq[Class[_]] = (ConllProperNounPhraseFinder.prereqAttrs ++ AcronymNounPhraseFinder.prereqAttrs++PronounFinder.prereqAttrs ++ NnpPosNounPhraseFinder.prereqAttrs ++ ForwardCoref.prereqAttrs).distinct
  override def annotateMentions(document:Document): Unit = {
    if(document.coref.mentions.isEmpty) (ConllProperNounPhraseFinder(document) ++ PronounFinder(document) ++ NnpPosNounPhraseFinder(document)++ AcronymNounPhraseFinder(document)).distinct.foreach(phrase => document.getCoref.addMention(phrase))
    document.coref.mentions.foreach(mention => NounPhraseEntityTypeLabeler.process(mention.phrase))
    document.coref.mentions.foreach(mention => NounPhraseGenderLabeler.process(mention.phrase))
    document.coref.mentions.foreach(mention => NounPhraseNumberLabeler.process(mention.phrase))
  }
}

object NerForwardCoref extends NerForwardCoref {
  deserialize(new DataInputStream(ClasspathURL[NerForwardCoref](".factorie").openConnection().getInputStream))
}

class ForwardCoref extends ForwardCorefBase {
  val model = new BaseCorefModel
}

object ForwardCoref extends ForwardCoref

class ForwardCorefImplicitConjunctions extends ForwardCorefBase {
  val model = new ImplicitCrossProductCorefModel
}

abstract class ForwardCorefBase extends CorefSystem[Seq[MentionPairLabel]] {
  val options = new CorefOptions
  val model:PairwiseCorefModel


  /**Store head words which are seen over a default 20 times in the model
   * @param trainDocs Documents to generate counts from*/
  def preprocessCorpus(trainDocs:Seq[Document]) = {
    val nonPronouns = trainDocs.flatMap(_.targetCoref.mentions.filterNot(m => m.phrase.isPronoun))
    model.CorefTokenFrequencies.counter = new TopTokenFrequencies(TokenFreqs.countWordTypes(nonPronouns,(t) => t.phrase.headToken.string.toLowerCase,20))
  }

  def instantiateModel(optimizer:GradientOptimizer,pool:ExecutorService) = new LeftRightParallelTrainer(optimizer,pool)

  /**Generate the labels used for training
   * @param coref This is expected to be the true coreference class for the document
   * @return Sequence of training labels for this document*/
  def getCorefStructure(coref:WithinDocCoref): Seq[MentionPairLabel] = {
    val mentions = coref.mentions.sortBy(m=>m.phrase.start)
    assertSorted(mentions)
    val labels = new ArrayBuffer[MentionPairLabel]
    for (i <- 0 until mentions.size){
      if(!options.usePronounRules || !mentions(i).phrase.isPronoun)
        labels ++= generateTrainingLabelsForOneAnaphor(mentions, i)
    }
    labels
  }

  /**
   * Given the index of a mention, create positive and negative labels for this mention and its prodecessors
   * @param orderedMentions Mentions for this document
   * @param anaphorIndex Index of current mention to generate labels for
   * @return Training Labels for this Mention */
  protected def generateTrainingLabelsForOneAnaphor(orderedMentions: Seq[Mention], anaphorIndex: Int): Seq[MentionPairLabel] = {
    val labels = new ArrayBuffer[MentionPairLabel]
    val m1 = orderedMentions(anaphorIndex)
    var numAntecedents = 0
    var i = anaphorIndex - 1
    while (i >= 0 && (numAntecedents < options.numPositivePairsTrain || !options.pruneNegTrain)) {
      val m2 = orderedMentions(i)
      val label = m1.entity != null & m1.entity == m2.entity
      if (!pruneMentionPairTraining(m1,m2,label,numAntecedents)) {
        val cl = new MentionPairLabel(model, m1, m2, orderedMentions, label, options=options)
        if(label) numAntecedents += 1
        labels += cl
      }
      i -= 1
    }
    labels
  }
  case class MentionPairLabelFeatures(label: MentionPairLabel,features: MentionPairFeatures)

  /** Given a sequence of MentionPairLabels for a document, compute features of the pair and return both*/
  protected def generateFeatures(labels: Seq[MentionPairLabel]): Seq[MentionPairLabelFeatures] = {
    val previousLabels = new ArrayBuffer[MentionPairLabelFeatures]()
    labels.foreach{ label =>
      val candidateLabelFeatures = label.genFeatures()
      //If we want to merge features of our antecedent with any of it's previous mentions,
      if(options.mergeFeaturesAtAll && label.mention2.entity != null){
        val matchingPreviousLabelsFeatures = previousLabels.lastIndexWhere(l => l.label.mention2.entity == label.mention2.entity)
        if(matchingPreviousLabelsFeatures != -1) mergeFeatures(candidateLabelFeatures, previousLabels(matchingPreviousLabelsFeatures).features)
      }
      previousLabels += new MentionPairLabelFeatures(label,candidateLabelFeatures)
    }
    previousLabels
  }

  class LeftRightParallelTrainer(optimizer: GradientOptimizer, pool: ExecutorService, miniBatchSize: Int = 1) extends ParallelTrainer(optimizer,pool){
    def map(in: Seq[MentionPairLabel]): Seq[Example] = {
     // |**("Adding Features for Labels")
      val examples = MiniBatchExample(miniBatchSize,generateFeatures(in).map{trainingInstance => model.getExample(trainingInstance.label,trainingInstance.features,options.slackRescale)})
     // **|
      examples
    }
  }

  def mergeFeatures(l: MentionPairFeatures, mergeables: MentionPairFeatures) {
    if (options.mergeFeaturesAtAll) {
      assert(l.features.activeCategories.forall(!_.startsWith("NBR")))
      val mergeLeft = ArrayBuffer[MentionPairFeatures]()
      l.features ++= mergeables.features.mergeableAllFeatures.map("NBRR_" + _)
    }
  }

  /**Types of Pairs Pruned during Training
   *     - cataphora since we do not corefer these
   *     - Any pair of mentions which overlap each other*/
  def pruneMentionPairTraining(anaphor: Mention,antecedent: Mention,label: Boolean,numAntecedents: Int): Boolean = {
    val cataphora = antecedent.phrase.isPronoun && !anaphor.phrase.isPronoun
    if(cataphora) {
      if (label && !options.allowPosCataphora || !label && !options.allowNegCataphora) {
        return true
      }
    }
    if(!anaphor.phrase.tokens.intersect(antecedent.phrase.tokens).isEmpty) return true
    if (label && numAntecedents > 0 && !options.pruneNegTrain) return true
    return false
  }
  def pruneMentionPairTesting(anaphor: Mention,antecedent: Mention): Boolean = {
    val cataphora = antecedent.phrase.isPronoun && !anaphor.phrase.isPronoun
    if(options.usePronounRules && antecedent.phrase.isPronoun) return true
    else if(cataphora || options.allowTestCataphora) return true
    if(!anaphor.phrase.tokens.intersect(antecedent.phrase.tokens).isEmpty) return true
    return false
  }

  /**Find each mentions best scoring antecedent.  If the antecedent has a cluster add the new mention if not, create a new entity and add both mentions
   * Currently does not create singleton entities
   * @param coref Expects nontarget coref class that is pre annotated with mentions
   * @return
   */
  def infer(coref: WithinDocCoref): WithinDocCoref = {
    val mentions = coref.mentions.sortBy(m => m.phrase.start)
    for (i <- 0 until coref.mentions.size) {
      val m1 = mentions(i)
      val bestCand = getBestCandidate(coref,mentions, i)
      if (bestCand != null) {
        if(bestCand.entity ne null){
          bestCand.entity += m1
        }
        else{
          val entity = coref.newEntity(); entity += bestCand; entity += m1
        }
      }else {val entity = coref.newEntity(); entity += m1}
    }
    coref
  }

  def getBestCandidate(coref: WithinDocCoref, mentions: Seq[Mention], mInt: Int): Mention = {
    val candidateLabels = ArrayBuffer[MentionPairFeatures]()
    var bestCandidate: Mention = null
    var bestScore = Double.MinValue
    var anteIdx = mInt
    val m1 = mentions(mInt)
    var numPositivePairs = 0
    while (anteIdx >= 0 && (numPositivePairs < options.numPositivePairsTest || !options.pruneNegTest)) {
      val m2 = mentions(anteIdx)
      if (!pruneMentionPairTesting(m1,m2)) {
        val candidateLabel = new MentionPairFeatures(model, m1, m2, mentions, options=options)
        val mergeables = candidateLabels.lastIndexWhere(l => l.mention2.entity != null &&l.mention2.entity == candidateLabel.mention2.entity)
        if(mergeables != -1) mergeFeatures(candidateLabel, candidateLabels(mergeables))
        candidateLabels += candidateLabel
        val score =  if (m1.phrase.isProperNoun && m1.attr[MentionCharacteristics].nounWords.forall(m2.attr[MentionCharacteristics].nounWords.contains)
                                && m2.attr[MentionCharacteristics].nounWords.forall(m1.attr[MentionCharacteristics].nounWords.contains)
                                || options.mergeMentionWithApposition && (m1.phrase.isAppositionOf(m2.phrase)
                                || m2.phrase.isAppositionOf(m1.phrase))) Double.PositiveInfinity
        else model.predict(candidateLabel.value)
        if (score > 0.0) {
          numPositivePairs += 1
          if (bestScore <= score) {
            bestCandidate = m2
            bestScore = score
          }
        }
      }
      anteIdx -= 1
    }
    bestCandidate
  }
}




/**Base class for any coreference system
 * @tparam CoreferenceStructure The type used as a training instance, ex. MentionPairLabel or MentionGraph,
 *                              In the examples above, the training instance is either one pair or the whole document respectively*/
abstract class CorefSystem[CoreferenceStructure] extends DocumentAnnotator with Trackable{
  val model:CorefModel
  val options:CorefOptions
  def prereqAttrs: Seq[Class[_]] = Seq(classOf[Token],classOf[PennPosTag])
  def postAttrs = Seq(classOf[WithinDocCoref])
  def tokenAnnotationString(token:Token): String = {
    val entities = token.document.coref.entities.toSeq
    var outputString = token.document.coref.mentions.filter(mention => mention.phrase.contains(token)) match {
      case ms:Seq[Mention] if ms.length > 0 => ms.filter(m => m.entity != null && !m.entity.isSingleton).map{
          m => if (m.phrase.length == 1) "("+entities.indexOf(m.entity)+")"
               else if(m.phrase.indexOf(token) == 0) "("+entities.indexOf(m.entity)
               else if(m.phrase.indexOf(token) == m.phrase.length - 1) entities.indexOf(m.entity)+")"
               else ""
      }.mkString("|")
      case _ => "_"
    }
    if(outputString == "") outputString = "_"
    else if(outputString.endsWith("|")) outputString = outputString.substring(0,outputString.length-1)
    "%15s".format(outputString)
  }

  def process(document: Document) = {
    document.annotators += classOf[WithinDocCoref] -> this.getClass
    if(document.getCoref.mentions.isEmpty)
      annotateMentions(document)
    infer(document.getCoref)
    document
  }

  def annotateMentions(document: Document): Unit = {
    if(options.useGoldBoundaries){
      assert(document.targetCoref ne null,"Gold Boundaries cannot be used without gold data.")
      document.targetCoref.mentions.foreach{m =>
        if(options.useEntityType){
          val newMention = document.getCoref.addMention(new Phrase(m.phrase.value.chain,m.phrase.start,m.phrase.length,m.phrase.headTokenOffset))
          newMention.phrase.attr += m.phrase.attr[OntonotesPhraseEntityType]
          newMention.phrase.attr += m.phrase.attr[NounPhraseType]
        }
        else {
          val newMention = document.getCoref.addMention(new Phrase(m.phrase.value.chain,m.phrase.start,m.phrase.length,m.phrase.headTokenOffset))
          NounPhraseEntityTypeLabeler.process(newMention.phrase)
          newMention.phrase.attr += m.phrase.attr[NounPhraseType]
        }
      }
      NounPhraseGenderLabeler.process(document)
      MentionPhraseNumberLabeler.process(document)
    }
  }

  /**Perform any preprocessing such as getting top used words
   * @param trainDocs Documents to generate counts from */
  def preprocessCorpus(trainDocs: Seq[Document]): Unit

  /**Returns training labels for data in the format that should be used for training
   * @param coref Gold Coref to be used for training */
  def getCorefStructure(coref: WithinDocCoref): CoreferenceStructure
  def instantiateModel(optimizer: GradientOptimizer,pool: ExecutorService): ParallelTrainer
  def infer(doc: WithinDocCoref): WithinDocCoref

  abstract class ParallelTrainer(optimizer: GradientOptimizer, val pool: ExecutorService) {
    def map(in: CoreferenceStructure): Seq[Example]
    def reduce(states: Iterable[Seq[Example]]) {
      for (examples <- states) {
        val trainer = new OnlineTrainer(model.parameters, optimizer, maxIterations = 1, logEveryN = examples.length - 1)
        trainer.trainFromExamples(examples)
      }
    }
    def runParallel(ins: Seq[CoreferenceStructure]){
      reduce(cc.factorie.util.Threading.parMap(ins, pool)(map))
    }
    def runSequential(ins: Seq[CoreferenceStructure]){
      reduce(ins.map(map))
    }
  }

  def train(trainDocs: Seq[Document], testDocs: Seq[Document], wn: WordNet, rng: scala.util.Random, saveModelBetweenEpochs: Boolean,saveFrequency: Int,filename: String, learningRate: Double = 1.0): Double =  {
    val optimizer = if (options.useAverageIterate) new AdaGrad(learningRate) with ParameterAveraging else if (options.useAdaGradRDA) new AdaGradRDA(rate = learningRate,l1 = options.l1) else new AdaGrad(rate = learningRate)
    for(doc <- trainDocs; mention <- doc.targetCoref.mentions) mention.attr += new MentionCharacteristics(mention)
    preprocessCorpus(trainDocs)
    |**("Training Structure Generated")
    var i = 0
    val trainingFormat: Seq[CoreferenceStructure] = trainDocs.map{doc => i +=1 ; if(i % 100 == 0) println("Processing Labels for: " + i + " of " + trainDocs.size); getCorefStructure(doc.targetCoref)}
    **|
    val pool = java.util.concurrent.Executors.newFixedThreadPool(options.numThreads)
    var accuracy = 0.0
    try {
      val trainer = instantiateModel(optimizer, pool)
      for (iter <- 0 until options.numTrainingIterations) {
        val shuffledDocs = rng.shuffle(trainingFormat)
        val batches = shuffledDocs.grouped(options.featureComputationsPerThread*options.numThreads).toSeq
        for ((batch, b) <- batches.zipWithIndex) {
          if (options.numThreads > 1) trainer.runParallel(batch)
          else trainer.runSequential(batch)
        }
        if (!model.MentionPairFeaturesDomain.dimensionDomain.frozen) model.MentionPairFeaturesDomain.dimensionDomain.freeze()
        if (!options.useAdaGradRDA && options.useAverageIterate) optimizer match {case o: ParameterAveraging => o.setWeightsToAverage(model.parameters) }
        println("Train docs")
        doTest(trainDocs.take((trainDocs.length*options.trainPortionForTest).toInt), wn, "Train")
        println("Test docs")
        |**("Running Test")
        accuracy = doTest(testDocs, wn, "Test")
        **|("End Test")
        if(saveModelBetweenEpochs && iter % saveFrequency == 0)
          serialize(filename + "-" + iter)
        if (!options.useAdaGradRDA && options.useAverageIterate) optimizer match {case o: ParameterAveraging => o.unSetWeightsToAverage(model.parameters) }
      }
      if (!options.useAdaGradRDA&& options.useAverageIterate) optimizer match {case o: ParameterAveraging => o.setWeightsToAverage(model.parameters) }
      accuracy
    } finally {
      pool.shutdown()
    }
  }

  class CorefTester(scorer: CorefConllOutput, scorerMutex: Object, val pool: ExecutorService){
    def map(doc: Document): Unit = {
      assert(doc.targetCoref ne null,"Cannot perform test on document without test key.")
      val trueCoref = doc.targetCoref
      val predCoref = doc.coref

      predCoref.resetPredictedMapping()
      for(mention <- predCoref.mentions) if(mention.attr[MentionCharacteristics] eq null) mention.attr += new MentionCharacteristics(mention)

      infer(predCoref)

      val b3 = ClusterF1Evaluation.BCubedNoSingletons(predCoref, trueCoref)
      val ce = ClusterF1Evaluation.CeafE(predCoref,trueCoref)
      val muc = ClusterF1Evaluation.MUCNoSingletons(predCoref, trueCoref)
      val cm = ClusterF1Evaluation.CeafM(predCoref,trueCoref)

      scorerMutex.synchronized {
        scorer.microB3.microAppend(b3)
        scorer.microCE.microAppend(ce)
        scorer.microCM.microAppend(cm)
        scorer.microMUC.microAppend(muc)
      }
    }
    def runParallel(ins: Seq[Document]) = cc.factorie.util.Threading.parMap(ins, pool)(map)
    def runSequential(ins: Seq[(Document)]) = ins.map(map)
  }

  def doTest(testDocs: Seq[Document], wn: WordNet, name: String): Double = {
    val scorer = new CorefConllOutput
    object ScorerMutex
    val pool = java.util.concurrent.Executors.newFixedThreadPool(options.numThreads)
    var accuracy = 0.0
    try {
      val tester = new CorefTester(scorer, ScorerMutex, pool)
      tester.runParallel(testDocs)
      println("-----------------------")
      println("  * Overall scores")
      scorer.printInhouseScore(name)
      accuracy = scorer.microMUC.f1
    } finally pool.shutdown()
    accuracy
  }

  def assertSorted(mentions: Seq[Mention]): Unit = {
    for(i <- 0 until mentions.length -1)
      assert(mentions(i).phrase.tokens.head.stringStart <= mentions(i+1).phrase.tokens.head.stringStart, "the mentions are not sorted by their position in the document. Error at position " +i+ " of " + mentions.length)
  }

  def deserialize(stream: DataInputStream) {
    val config = options.getConfigHash
    BinarySerializer.deserialize(config, stream)
    options.setConfigHash(config)
    println("deserializing with config:\n" + options.getConfigHash.iterator.map(x => x._1 + " = " + x._2).mkString("\n"))
    model.deserialize(stream)
    model.MentionPairFeaturesDomain.dimensionDomain.freeze()
    println("model weights 1norm = " + model.parameters.oneNorm)
    stream.close()
  }

  def deserialize(filename: String) {
    deserialize(new DataInputStream(new FileInputStream(filename)))
  }

  def serialize(filename: String) {
    println("serializing with config:\n" + options.getConfigHash.iterator.map(x => x._1 + " = " + x._2).mkString("\n"))
    val stream = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(new File(filename))))
    BinarySerializer.serialize(options.getConfigHash, stream)
    model.serialize(stream)
    println("model weights 1norm = " + model.parameters.oneNorm)
    stream.close()
  }
}
