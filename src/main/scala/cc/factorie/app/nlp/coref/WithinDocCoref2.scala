package cc.factorie.app.nlp.coref

import cc.factorie.app.nlp.wordnet.WordNet
import cc.factorie.app.nlp.{Document, DocumentAnnotator}
import cc.factorie.app.nlp.mention.{Mention, MentionList}
import cc.factorie.util.coref.{CorefEvaluator, GenericEntityMap}
import java.util.concurrent.ExecutorService
import cc.factorie.optimize._
import java.io.{File, FileInputStream, DataInputStream}
import cc.factorie.util.BinarySerializer
import scala.collection.mutable.ArrayBuffer

/**
 * User: apassos
 * Date: 6/27/13
 * Time: 12:25 PM
 */

class WithinDocCoref2(val model: PairwiseCorefModel, val options: Coref2Options, val wordnet: WordNet, val gazetteers: CorefGazetteers) extends DocumentAnnotator {
  def prereqAttrs = Seq(classOf[MentionList])
  def postAttrs = Seq(classOf[GenericEntityMap[Mention]])
  def process1(document: Document) = {
    if (options.useEntityLR) document.attr += processDocumentOneModelFromEntities(document)
    else document.attr += processDocumentOneModel(document)
    document
  }

  trait CorefParallelHelper[T] {
    def pool: ExecutorService
    def map(in: Document): T
    def reduce(states: Iterable[T])
    def runParallel(ins: Seq[Document]) {
      reduce(TrainerHelpers.parMap(ins, pool)(map(_)))
    }
    def runSequential(ins: Seq[Document]) {
      reduce(ins.map(map))
    }
  }

  def deserialize(stream: DataInputStream, configStream: DataInputStream) {
    import cc.factorie.util.CubbieConversions._
    val config = options.getConfigHash
    BinarySerializer.deserialize(config, configStream)
    configStream.close()
    options.setConfigHash(config)
    println("deserializing with config:\n" + options.getConfigHash.iterator.map(x => x._1 + " = " + x._2).mkString("\n"))
    model.deserialize(stream)
  }

  def deserialize(filename: String) {
    deserialize(new DataInputStream(new FileInputStream(filename)), new DataInputStream(new FileInputStream(filename + ".config")))
  }

  def serialize(filename: String) {
    import cc.factorie.util.CubbieConversions._
    println("serializing with config:\n" + options.getConfigHash.iterator.map(x => x._1 + " = " + x._2).mkString("\n"))
    BinarySerializer.serialize(options.getConfigHash,new File(filename + ".config"))
    model.serialize(filename)
  }


  def generateTrainingInstances(d: Document, map: GenericEntityMap[Mention]): Seq[MentionPairLabel] = {
    generateMentionPairLabels(d.attr[MentionList].map(CorefMention.mentionToCorefMention(_, wordnet, gazetteers)), map)
  }

  protected def generateMentionPairLabels(mentions: Seq[CorefMention], map: GenericEntityMap[Mention] = null): Seq[MentionPairLabel] = {
    val labels = new ArrayBuffer[MentionPairLabel]
    for (i <- 0 until mentions.size)
      labels ++= generateMentionPairLabelsForOneAnaphor(mentions, i, map)
    /*if (Options.debug) println("# of ex = " + labels.size)*/
    labels
  }

  protected def generateMentionPairLabelsForOneAnaphor(orderedMentions: Seq[CorefMention], anaphorIndex: Int, map: GenericEntityMap[Mention] = null): Seq[MentionPairLabel] = {
    val labels = new ArrayBuffer[MentionPairLabel]
    val m1 = orderedMentions(anaphorIndex)
    var numAntecedent = 0
    var i = anaphorIndex - 1
    while (i >= 0 && (numAntecedent < options.numPositivePairsTrain || !options.pruneNegTrain)) {
      val m2 = orderedMentions(i)
      val cataphora = m2.isPRO && !m1.isPRO
      val label = m1.parentEntity == m2.parentEntity

      var skip = false

      if (cataphora) {
        if (label && !options.allowPosCataphora || !label && !options.allowNegCataphora) {
          skip = true
        }
      }

      if (label) {
        if (numAntecedent > 0 && !options.pruneNegTrain) skip = true
        else if (!skip) numAntecedent += 1
      }

      if (!skip) {
        val cl = new MentionPairLabel(model, m1, m2, orderedMentions, label, options=options)

        // merge features from neighboring mentions that are in the same chain as m2
        assert(map != null, "Options.mergeNeighborFeatures requires non-null mention-entityID map")
        val mergeables = labels.filter(l => map.reverseMap(l.mention2.mention) == map.reverseMap(m2.mention))
        mergeFeatures(cl.features, mergeables.map(_.features))

        labels += cl
      }
      i -= 1
    }

    labels
  }

  class LeftRightParallelTrainer(model: PairwiseCorefModel, optimizer: GradientOptimizer, trainTrueMaps: Map[String, GenericEntityMap[Mention]], val pool: ExecutorService, miniBatchSize: Int = 1)
    extends CorefParallelHelper[Seq[Example]] {
    def map(d: Document): Seq[Example] = MiniBatchExample(miniBatchSize,
      generateTrainingInstances(d, trainTrueMaps(d.name)).map(i => model.getExample(i, options.slackRescale)))
    def reduce(states: Iterable[Seq[Example]]) {
      for (examples <- states) {
        val trainer = new OnlineTrainer(model.parameters, optimizer, maxIterations = 1, logEveryN = examples.length - 1)
        trainer.trainFromExamples(examples)
      }
    }
  }

  def train(trainDocs: Seq[Document], testDocs: Seq[Document], wn: WordNet, rng: scala.util.Random, trainTrueMaps: Map[String, GenericEntityMap[Mention]], testTrueMaps: Map[String, GenericEntityMap[Mention]],saveModelBetweenEpochs: Boolean,saveFrequency: Int,filename: String) {
    val trainTrueMaps = trainDocs.map(d => d.name -> model.generateTrueMap(d.attr[MentionList])).toMap
    val optimizer = if (options.useAverageIterate) new AdaGrad(1.0) with ParameterAveraging else if (options.useMIRA) new AdaMira(1.0) with ParameterAveraging else new AdaGrad(rate = 1.0)
    model.MentionPairLabelThing.tokFreq  ++= trainDocs.flatMap(_.tokens).groupBy(_.string.trim.toLowerCase.replaceAll("\\n", " ")).mapValues(_.size)
    val pool = java.util.concurrent.Executors.newFixedThreadPool(options.numThreads)
    val trainer = new LeftRightParallelTrainer(model, optimizer, trainTrueMaps, pool)
    for (iter <- 0 until options.numTrainingIterations) {
      val shuffledDocs = rng.shuffle(trainDocs)
      val batches = shuffledDocs.grouped(options.featureComputationsPerThread*options.numThreads).toSeq
      for ((batch, b) <- batches.zipWithIndex) {
        if (options.numThreads > 1) trainer.runParallel(batch)
        else trainer.runSequential(batch)
      }
      if (!model.MentionPairFeaturesDomain.dimensionDomain.frozen) model.MentionPairFeaturesDomain.dimensionDomain.freeze()
      optimizer match {case o: ParameterAveraging => o.setWeightsToAverage(model.parameters) }
      println("Train docs")
      doTest(trainDocs.take((trainDocs.length*options.trainPortionForTest).toInt), wn, trainTrueMaps, "Train")
      println("Test docs")
      doTest(testDocs, wn, testTrueMaps, "Test")

      if(saveModelBetweenEpochs && iter % saveFrequency == 0)
        serialize(filename + "-" + iter)

      optimizer match {case o: ParameterAveraging => o.unSetWeightsToAverage(model.parameters) }
    }
    optimizer match {case o: ParameterAveraging => o.setWeightsToAverage(model.parameters) }
    pool.shutdown()
  }

  abstract class CorefTester(model: PairwiseCorefModel, scorer: CorefScorer[Mention], scorerMutex: Object, testTrueMaps: Map[String, GenericEntityMap[Mention]], val pool: ExecutorService)
    extends CorefParallelHelper[Null] {
    def getPredMap(doc: Document, model: PairwiseCorefModel): GenericEntityMap[Mention]
    def reduce(states: Iterable[Null]) { }
    def map(doc: Document): Null = {
      val fId = doc.name
      val trueMap = testTrueMaps(fId)
      val predMap = getPredMap(doc, model)

      if(options.useNonGoldBoundaries){
        val gtMentions = trueMap.entities.values.flatten.toSet
        val predMention = predMap.entities.values.flatten.toSet
        gtMentions.diff(predMention).foreach(predMap.addMention(_,predMap.numMentions + 1))
      }
      val pw = CorefEvaluator.Pairwise.evaluate(predMap, trueMap)
      val b3 = CorefEvaluator.BCubedNoSingletons.evaluate(predMap, trueMap)
      val muc = CorefEvaluator.MUC.evaluate(predMap, trueMap)

      scorerMutex.synchronized {
        scorer.macroMUC.macroAppend(muc)
        scorer.macroPW.macroAppend(pw)
        scorer.microB3.microAppend(b3)
        scorer.microMUC.microAppend(muc)
        scorer.microPW.microAppend(pw)
      }
      null
    }
  }

  def mergeFeatures(l: MentionPairFeatures, mergeables: Seq[MentionPairFeatures]) {
    if (options.mergeFeaturesAtAll) {
      assert(l.features.activeCategories.forall(!_.startsWith("NBR")))
      val mergeLeft = ArrayBuffer[MentionPairLabel]()
      mergeables.take(1).diff(mergeLeft).foreach(l.features ++= _.features.mergeableAllFeatures.map("NBRR_" + _))
    }
  }

  def processDocumentOneModel(doc: Document): GenericEntityMap[Mention] = {
    val ments = doc.attr[MentionList]
    val corefMentions = ments.map(CorefMention.mentionToCorefMention(_, wordnet, gazetteers))
    val map = processDocumentOneModelFromMentions(corefMentions)
    map
  }

  def processDocumentOneModelFromMentions(ments: Seq[CorefMention]): GenericEntityMap[Mention] = {
    val predMap = new GenericEntityMap[Mention]
    ments.foreach(m => predMap.addMention(m.mention, predMap.numMentions.toLong))
    for (i <- 0 until ments.size) {
      val m1 = ments(i)
      val bestCand = processOneMention(ments, i, predMap)
      if (bestCand != null) {
        predMap.addCoreferentPair(m1.mention, bestCand.mention)
      }
    }
    predMap
  }

  def processDocumentOneModelFromEntities(doc: Document): GenericEntityMap[Mention] = {
    val ments = doc.attr[MentionList].map(CorefMention.mentionToCorefMention(_, wordnet, gazetteers))
    val predMap = new GenericEntityMap[Mention]
    ments.foreach(m => predMap.addMention(m.mention, predMap.numMentions.toLong))
    val candidateMentionsToTheLeft = ArrayBuffer[CorefMention]()

    for (i <- 0 until ments.size) {
      val m1 = ments(i)
      val bestCand = processOneMentionByEntities(ments, i, candidateMentionsToTheLeft ,predMap)
      if (bestCand != null) {
        predMap.addCoreferentPair(m1.mention, bestCand.mention)
      }
      if(bestCand != null || m1.isProper){
        candidateMentionsToTheLeft += m1
      }
    }
    predMap
  }



  def processOneMention(orderedMentions: Seq[CorefMention], mentionIndex: Int, predMap: GenericEntityMap[Mention]): CorefMention = {
    val m1 = orderedMentions(mentionIndex)
    val candLabels = ArrayBuffer[MentionPairFeatures]()
    var bestCand: CorefMention = null
    var bestScore = Double.MinValue

    var j = mentionIndex - 1
    var numPositivePairs = 0
    val m1IsPro = m1.isPRO
    while (j >= 0 && (numPositivePairs < options.numPositivePairsTest || !options.pruneNegTest)) {
      val m2 = orderedMentions(j)

      val cataphora = m2.isPRO && !m1IsPro

      if (!cataphora || options.allowTestCataphora) {
        val candLabel = new MentionPairFeatures(model, m1, m2, orderedMentions, options=options)
        val mergeables = candLabels.filter(l => predMap.reverseMap(l.mention2.mention) == predMap.reverseMap(m2.mention))
        mergeFeatures(candLabel, mergeables)
        candLabels += candLabel
        val score = if (m1.isProper && m1.nounWords.forall(m2.nounWords.contains) || options.mergeMentionWithApposition && m1.isAppositionOf(m2)) Double.PositiveInfinity
        else model.score(candLabel.value)
        // Pronouns should always link to something
        if (score > 0.0) {
          numPositivePairs += 1
          if (bestScore <= score) {
            bestCand = m2
            bestScore = score
          }
        }
      }
      j -= 1
    }
    bestCand
  }

  //this doesn't look at the preceeding mentions, but only the mentions that are in an existing entity or are proper
  def processOneMentionByEntities(orderedMentions: Seq[CorefMention], mentionIndex: Int, candidateMentionsToTheLeft: ArrayBuffer[CorefMention], predMap: GenericEntityMap[Mention]): CorefMention = {
    val m1 = orderedMentions(mentionIndex)
    val candLabels = ArrayBuffer[MentionPairFeatures]()
    var bestCand: CorefMention = null
    var bestScore = Double.MinValue

    var j = mentionIndex - 1
    val m1IsPro = m1.isPRO
    var numPositivePairs = 0
    val mentionsComparedWith = collection.mutable.HashSet[CorefMention]()
    //this while loop is the same as in the normal left-right code
    var numCompared = 0
    while (j >= 0 && (numPositivePairs < options.numPositivePairsTest || !options.pruneNegTest) && numCompared < options.numCompareToTheLeft){
      val m2 = orderedMentions(j)
      numCompared += 1
      val cataphora = m2.isPRO && !m1IsPro

      if (!cataphora || options.allowTestCataphora) {
        val candLabel = new MentionPairFeatures(model, m1, m2, orderedMentions, options=options)
        val mergeables = candLabels.filter(l => predMap.reverseMap(l.mention2.mention) == predMap.reverseMap(m2.mention))
        mergeFeatures(candLabel, mergeables)
        candLabels += candLabel
        // Always link exact head matches
        val score = if (m1.isProper && m1.nounWords.forall(m2.nounWords.contains) || options.mergeMentionWithApposition && m1.isAppositionOf(m2)) Double.PositiveInfinity
             else model.score(candLabel.value)
        // Pronouns should always link to something
        if (score > 0.0) {
          numPositivePairs += 1
          if (bestScore <= score) {
            bestCand = m2
            bestScore = score
          }
        }
      }
      mentionsComparedWith += m2
      j -= 1
    }
    //now, look at the list of candidateMentionsToTheLeft and compare to things that you haven't compared to yet
    //the expectation is that candidateMentionsToTheLeft is sorted

    var candidateIdx = candidateMentionsToTheLeft.length - 1
    val thereAreMentionsRemaining = j > 0
    if(thereAreMentionsRemaining){
      while(candidateIdx >= 0 && numPositivePairs < options.numPositivePairsTest) {
        val m2 = candidateMentionsToTheLeft(candidateIdx)
        if(!mentionsComparedWith.contains(m2)){
          val cataphora = m2.isPRO && !m1.isPRO
          if(!cataphora || options.allowTestCataphora){
            val candLabel = new MentionPairFeatures(model, m1, m2, orderedMentions, options=options)
            val mergeables = candLabels.filter(l => predMap.reverseMap(l.mention2.mention) == predMap.reverseMap(m2.mention))
            mergeFeatures(candLabel, mergeables)
            candLabels += candLabel
            // Always link exact head matches
            val score = if (m1.isProper && m1.nounWords.forall(m2.nounWords.contains) || options.mergeMentionWithApposition && m1.isAppositionOf(m2)) Double.PositiveInfinity
            else model.score(candLabel.value)
            // Pronouns should always link to something
            if (score > 0.0) {
              numPositivePairs += 1
              if (bestScore <= score) {
                bestCand = m2
                bestScore = score
              }
            }
          }
        }
        candidateIdx -= 1
      }
    }
    bestCand
  }

  trait LeftRightTester {
    def getPredMap(doc: Document, model: PairwiseCorefModel): GenericEntityMap[Mention] =
      processDocumentOneModel(doc)
  }

  trait LeftRightTesterFromEntities {
    def getPredMap(doc: Document, model: PairwiseCorefModel): GenericEntityMap[Mention] =
      processDocumentOneModelFromEntities(doc)
  }

  def doTest(testDocs: Seq[Document], wn: WordNet, testTrueMaps: Map[String, GenericEntityMap[Mention]], name: String) {
    val scorer = new CorefScorer[Mention]
    object ScorerMutex
    val pool = java.util.concurrent.Executors.newFixedThreadPool(options.numThreads)
    val tester = if (options.useEntityLR) new CorefTester(model, scorer, ScorerMutex, testTrueMaps, pool) with LeftRightTesterFromEntities
      else new CorefTester(model, scorer, ScorerMutex, testTrueMaps, pool) with LeftRightTester
    tester.runParallel(testDocs)
    println("-----------------------")
    println("  * Overall scores")
    scorer.printInhouseScore(name)
    pool.shutdown()
  }

}
