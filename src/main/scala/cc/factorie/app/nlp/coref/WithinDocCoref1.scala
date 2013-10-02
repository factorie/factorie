package cc.factorie.app.nlp.coref

import cc.factorie.app.nlp.wordnet.WordNet
import cc.factorie.app.nlp.{Token, Document, DocumentAnnotator}
import cc.factorie.app.nlp.mention._
import cc.factorie.util.coref.{CorefEvaluator, GenericEntityMap}
import java.util.concurrent.ExecutorService
import cc.factorie.optimize._
import java.io._
import cc.factorie.util.{ClasspathURL, BinarySerializer}
import scala.collection.mutable.ArrayBuffer

/**
 * User: apassos
 * Date: 6/27/13
 * Time: 12:25 PM
 */

abstract class BaseWithinDocCoref1 extends DocumentAnnotator {
  val options = new Coref1Options
  val model: PairwiseCorefModel

  def prereqAttrs: Seq[Class[_]] = Seq(classOf[MentionList], classOf[MentionEntityType], classOf[MentionGenderLabel], classOf[MentionNumberLabel])
  def postAttrs = Seq(classOf[GenericEntityMap[Mention]])
  def process(document: Document) = {
    if (options.useEntityLR) document.attr += processDocumentOneModelFromEntities(document)
    else document.attr += processDocumentOneModel(document)
    document
  }
  def tokenAnnotationString(token:Token): String = {
    val emap = token.document.attr[GenericEntityMap[Mention]]
    token.document.attr[MentionList].filter(mention => mention.contains(token)) match {
      case ms:Seq[Mention] if ms.length > 0 => ms.map(m => m.attr[MentionType].categoryValue+":"+m.indexOf(token)+"e"+emap.getEntity(m)).mkString(", ")
      case _ => "_"
    }
  }

  trait CorefParallelHelper[T] {
    def pool: ExecutorService
    def map(in: Document): T
    def reduce(states: Iterable[T])
    def runParallel(ins: Seq[Document]) {
      reduce(cc.factorie.util.Threading.parMap(ins, pool)(map))
    }
    def runSequential(ins: Seq[Document]) {
      reduce(ins.map(map))
    }
  }

  def deserialize(stream: DataInputStream) {
    import cc.factorie.util.CubbieConversions._
    val config = options.getConfigHash
    BinarySerializer.deserialize(config, stream)
    options.setConfigHash(config)
    println("deserializing with config:\n" + options.getConfigHash.iterator.map(x => x._1 + " = " + x._2).mkString("\n"))
    //BinarySerializer.deserialize(model, stream)
    model.deserialize(stream)
    model.MentionPairFeaturesDomain.dimensionDomain.freeze()
    println("model weights 1norm = " + model.parameters.oneNorm)
    stream.close()
  }

  def deserialize(filename: String) {
    deserialize(new DataInputStream(new FileInputStream(filename)))
  }

  def serialize(filename: String) {
    import cc.factorie.util.CubbieConversions._
    println("serializing with config:\n" + options.getConfigHash.iterator.map(x => x._1 + " = " + x._2).mkString("\n"))
    val stream = new DataOutputStream(new FileOutputStream(new File(filename)))
    BinarySerializer.serialize(options.getConfigHash, stream)
    model.serialize(stream)
    println("model weights 1norm = " + model.parameters.oneNorm)
    stream.close()
  }


  def generateTrainingInstances(d: Document, map: GenericEntityMap[Mention]): Seq[MentionPairLabel] = {
    generateMentionPairLabels(d.attr[MentionList].map(CorefMention.mentionToCorefMention), map)
  }

  protected def generateMentionPairLabels(mentions: Seq[CorefMention], map: GenericEntityMap[Mention] = null): Seq[MentionPairLabel] = {
    val labels = new ArrayBuffer[MentionPairLabel]
    for (i <- 0 until mentions.size){
      if(!options.usePronounRules || !mentions(i).isPRO)
        labels ++= generateMentionPairLabelsForOneAnaphor(mentions, i, map)
    }
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
      if(options.usePronounRules && m2.isPRO)
        skip = true

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

  def train(trainDocs: Seq[Document], testDocs: Seq[Document], wn: WordNet, rng: scala.util.Random, trainTrueMaps: Map[String, GenericEntityMap[Mention]], testTrueMaps: Map[String, GenericEntityMap[Mention]],saveModelBetweenEpochs: Boolean,saveFrequency: Int,filename: String, learningRate: Double = 1.0) {
    val trainTrueMaps = trainDocs.map(d => d.name -> model.generateTrueMap(d.attr[MentionList])).toMap
    val optimizer = if (options.useAverageIterate) new AdaGrad(learningRate) with ParameterAveraging else if (options.useMIRA) new AdaMira(learningRate) with ParameterAveraging else new AdaGrad(rate = learningRate)
    model.MentionPairLabelThing.tokFreq  ++= trainDocs.flatMap(_.tokens).groupBy(_.string.trim.toLowerCase.replaceAll("\\n", " ")).mapValues(_.size)
    val pool = java.util.concurrent.Executors.newFixedThreadPool(options.numThreads)
    try {
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
    } finally {
      pool.shutdown()
    }
  }

  abstract class CorefTester(model: PairwiseCorefModel, scorer: CorefScorer[Mention], scorerMutex: Object, testTrueMaps: Map[String, GenericEntityMap[Mention]], val pool: ExecutorService)
    extends CorefParallelHelper[Null] {
    def getPredMap(doc: Document, model: PairwiseCorefModel): GenericEntityMap[Mention]
    def reduce(states: Iterable[Null]) { }
    def map(doc: Document): Null = {
      val fId = doc.name
      val trueMap = testTrueMaps(fId)
      val predMap = getPredMap(doc, model)

      val gtMentions = trueMap.entities.values.flatten.toSet
      val predMentions = predMap.entities.values.flatten.toSet

      val nm = predMap.numMentions
      gtMentions.diff(predMentions).seq.toSeq.zipWithIndex.foreach(mi =>  predMap.addMention(mi._1, mi._2+ nm))
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
  def assertSorted(ments: Seq[Mention]): Unit = {
     val len = ments.length
     var i = 1
     while(i < len){
       assert(ments(i).tokens.head.stringStart >= ments(i-1).tokens.head.stringStart, "the mentions are not sorted by their position in the document. Error at position " +i+ " of " + len)
       i +=1
     }
  }

  def processDocumentOneModel(doc: Document): GenericEntityMap[Mention] = {
    val ments = doc.attr[MentionList]
    assertSorted(ments)
    val corefMentions = ments.map(CorefMention.mentionToCorefMention)
    val map = processDocumentOneModelFromMentions(corefMentions)
    map
  }

  def processDocumentOneModelFromMentions(ments: Seq[CorefMention]): GenericEntityMap[Mention] = {
    val predMap = new GenericEntityMap[Mention]
    assertSorted(ments.map(_.mention))
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
    processDocumentOneModelFromEntitiesFromMentions(doc.attr[MentionList].sortBy(mention => (mention.tokens.head.stringStart, mention.length)))
  }

  def processDocumentOneModelFromEntitiesFromMentions(inputMentions: Seq[Mention]): GenericEntityMap[Mention] = {

    val allMents = inputMentions.map(CorefMention.mentionToCorefMention)
    val ments = if(options.usePronounRules) allMents.filter(!_.isPRO) else allMents

    val predMap = new GenericEntityMap[CorefMention]
    allMents.foreach(m => predMap.addMention(m, predMap.numMentions.toLong))
    val candidateMentionsToTheLeft = ArrayBuffer[CorefMention]()

    for (i <- 0 until ments.size) {
      val m1 = ments(i)
      val bestCand = processOneMentionByEntities(ments, i, candidateMentionsToTheLeft ,predMap)
      if (bestCand != null) {
        predMap.addCoreferentPair(m1, bestCand)
      }
      if(bestCand != null || m1.isProper){
        candidateMentionsToTheLeft += m1
      }
    }
    if(options.usePronounRules)
      doCorefOnPronounsUsingRules(predMap,allMents)

    // we need to convert it into an entity Map of Mentions, rather than CorefMentions
    val predMap2 = new GenericEntityMap[Mention]
    predMap2.entities ++= predMap.entities.map(kv => (kv._1,kv._2.map(m => m.mention)))
    predMap2.reverseMap ++= predMap.reverseMap.map(kv => (kv._1.mention,kv._2))
    predMap2
  }

  case class CoarseMentionType(gender: String, number: String)

  def doCorefOnPronounsUsingRules(predMap: GenericEntityMap[CorefMention],allMentions: Seq[CorefMention]): Unit = {

    //first, make a map from entity ids in the predMap to sets of CoarseMentionTypes that appear somewhere in the entity.
    val numGenderSetsForEntities = predMap.entities.map(kv => (kv._1,kv._2.map(m => CoarseMentionType(m.gender,m.number)).toSet))

    //make such a set for every mention. For those mentions that aren't in entities, the set is just a singleton.
    val numGenderSets = allMentions.map(m => numGenderSetsForEntities.getOrElse(predMap.getEntity(m),Set(CoarseMentionType(m.gender,m.number))))

    for(i <- 0 until allMentions.length; if allMentions(i).isPRO){
      val m1 = allMentions(i)
      assert(numGenderSets(i).size == 1)
      val numGender1 = numGenderSets(i).head
      var j = i -1
      var searching = true
      while(j > 0 && searching){
        val m2 = allMentions(j)
        //find the first mention that isn't a pronoun and has a number-gender that is compatible with m1
        if(!m2.isPRO &&  numGenderSets(j).contains(numGender1)){
          predMap.addCoreferentPair(m1,m2)
          searching = false
        }
        j -= 1
      }
    }
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
        val score = if (m1.isProper && m1.nounWords.forall(m2.nounWords.contains) && m2.nounWords.forall(m1.nounWords.contains)  || options.mergeMentionWithApposition && (m1.isAppositionOf(m2) || m2.isAppositionOf(m1))) Double.PositiveInfinity
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
  def processOneMentionByEntities(orderedMentions: Seq[CorefMention], mentionIndex: Int, candidateMentionsToTheLeft: ArrayBuffer[CorefMention], predMap: GenericEntityMap[CorefMention]): CorefMention = {
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
      //val proPro = m1.isPRO && m2.isPRO       //uncomment this and the !proPro part below if you want to prohibit
      //pronoun-pronoun comparison

      if (/*!proPro && */ !cataphora || options.allowTestCataphora) {
        val candLabel = new MentionPairFeatures(model, m1, m2, orderedMentions, options=options)
        val mergeables = candLabels.filter(l => predMap.reverseMap(l.mention2) == predMap.reverseMap(m2))
        mergeFeatures(candLabel, mergeables)
        candLabels += candLabel
        // Always link exact head matches
        val score = if (m1.isProper && m1.nounWords.forall(m2.nounWords.contains) &&  m2.nounWords.forall(m1.nounWords.contains) || options.mergeMentionWithApposition && (m1.isAppositionOf(m2) || m2.isAppositionOf(m1))) Double.PositiveInfinity
             else model.score(candLabel.value)
        // Pronouns should always link to something
        if (score > 0.0) {
          numPositivePairs += 1
          if (bestScore <= score) {
            bestCand = m2
            bestScore = score
          }
        }
        mentionsComparedWith += m2
      }
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
            val mergeables = candLabels.filter(l => predMap.reverseMap(l.mention2) == predMap.reverseMap(m2))
            mergeFeatures(candLabel, mergeables)
            candLabels += candLabel
            // Always link exact head matches
            val score = if (m1.isProper && m1.nounWords.forall(m2.nounWords.contains) && m2.nounWords.forall(m1.nounWords.contains) || options.mergeMentionWithApposition && (m1.isAppositionOf(m2) || m2.isAppositionOf(m1))) Double.PositiveInfinity
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
    try {
      if(options.usePronounRules) assert(options.useEntityLR)
      val tester = if (options.useEntityLR) new CorefTester(model, scorer, ScorerMutex, testTrueMaps, pool) with LeftRightTesterFromEntities
      else new CorefTester(model, scorer, ScorerMutex, testTrueMaps, pool) with LeftRightTester
      tester.runParallel(testDocs)
      println("-----------------------")
      println("  * Overall scores")
      scorer.printInhouseScore(name)
    } finally pool.shutdown()
  }

}

class WithinDocCoref1 extends BaseWithinDocCoref1 {
  val model = new BaseCorefModel
}

class ImplicitConjunctionWithinDocCoref1 extends BaseWithinDocCoref1 {
  val model = new ImplicitCrossProductCorefModel
}

object WithinDocCoref1 extends WithinDocCoref1 {
  override def prereqAttrs: Seq[Class[_]] = Seq(classOf[MentionEntityType], classOf[MentionGenderLabel], classOf[MentionNumberLabel])
  deserialize(new DataInputStream(ClasspathURL[WithinDocCoref1](".factorie").openConnection().getInputStream))
}

// This should only be used when using the NerAndPronounMentionFinder to find mentions
object WithinDocCoref1Ner extends WithinDocCoref1 {
  override def prereqAttrs: Seq[Class[_]] = Seq(classOf[NerMentionList], classOf[MentionEntityType], classOf[MentionGenderLabel], classOf[MentionNumberLabel])
  deserialize(new DataInputStream(ClasspathURL[WithinDocCoref1]("-NER.factorie").openConnection().getInputStream))
}
