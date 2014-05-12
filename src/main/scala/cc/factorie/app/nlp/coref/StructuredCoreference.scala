package cc.factorie.app.nlp.coref

import cc.factorie.app.nlp.phrase._
import cc.factorie.optimize._
import java.util.concurrent.ExecutorService
import cc.factorie.variable.LabeledCategoricalVariable
import cc.factorie.app.nlp.{Sentence, Token, Document}
import cc.factorie.app.nlp.pos.PennPosTag
import cc.factorie.app.nlp.phrase.ParseAndNerBasedPhraseFinder
import java.io.DataInputStream
import cc.factorie.util.ClasspathURL

object NERAndPronounStructuredCoreference extends NERAndPronounStructuredCoreference{
  deserialize(new DataInputStream(ClasspathURL[NERAndPronounStructuredCoreference](".factorie").openConnection().getInputStream))
}

class NERAndPronounStructuredCoreference extends StructuredCoref{
  override def prereqAttrs: Seq[Class[_]] = (ConllProperNounPhraseFinder.prereqAttrs ++ AcronymNounPhraseFinder.prereqAttrs++PronounFinder.prereqAttrs ++ NnpPosNounPhraseFinder.prereqAttrs).distinct
  override def annotateMentions(doc:Document): Unit = {
    (ConllProperNounPhraseFinder(doc) ++ PronounFinder(doc) ++ NnpPosNounPhraseFinder(doc)++ AcronymNounPhraseFinder(doc)).distinct.foreach(phrase => doc.getCoref.addMention(phrase))
    NounPhraseEntityTypeLabeler.process(doc)
    NounPhraseGenderLabeler.process(doc)
    NounPhraseNumberLabeler.process(doc)
  }
}

object ParseStructuredCoreference extends ParseStructuredCoreference{
  deserialize(new DataInputStream(ClasspathURL[ParseStructuredCoreference](".factorie").openConnection().getInputStream))
}

//Uses Parse Based Mention Finding, best for data with nested mentions in the ontonotes annotation style
class ParseStructuredCoreference extends StructuredCoref{
  override def prereqAttrs: Seq[Class[_]] = ParseAndNerBasedPhraseFinder.prereqAttrs.toSeq
  override def annotateMentions(doc:Document): Unit = {
    ParseAndNerBasedPhraseFinder.process(doc)
    NounPhraseEntityTypeLabeler.process(doc)
    NounPhraseGenderLabeler.process(doc)
    NounPhraseNumberLabeler.process(doc)
  }
}

//Todo: The base Structured Coref class uses Gold Mentions, used for evaluation and tests on ConllData
class StructuredCoref extends CorefSystem[MentionGraph]{
  val options = new CorefOptions
  val model: StructuredCorefModel = new StructuredCorefModel
  override def prereqAttrs: Seq[Class[_]] = Seq(classOf[Sentence],classOf[PennPosTag])
  def tokenAnnotationString(token:Token):String = ???

  def preprocessCorpus(trainDocs:Seq[Document]) = {
    val nonPronouns = trainDocs.flatMap(_.targetCoref.mentions.filterNot(m => m.phrase.isPronoun))
    model.CorefTokenFrequencies.counter = new TopTokenFrequencies(nonPronouns,Vector("Head","First","Last","Prec","Follow","Shape","WordForm"))
  }

  def getCorefStructure(coref:WithinDocCoref) = new MentionGraph(model,coref,options,train=true)

  def instantiateModel(optimizer:GradientOptimizer,pool:ExecutorService) = new SoftMaxParallelTrainer(optimizer,pool)

  class SoftMaxParallelTrainer(optimizer: GradientOptimizer, pool: ExecutorService) extends ParallelTrainer(optimizer,pool){
    def map(d:MentionGraph): Seq[Example] = model.getExample(d)
  }

  def infer(coref: WithinDocCoref): WithinDocCoref = {
    val instance = new MentionGraph(model,coref,options)
    val scores = model.scoreGraph(instance)
    for(i <- 0 until coref.mentions.size){
      val m1 = coref.mentions(i)
      val (bestCandIndx,score) = getBestCandidate(instance, scores(i), i)
      val bestCand = coref.mentions(bestCandIndx)
      if(bestCand != m1){
        if(bestCand.entity ne null)
          bestCand.entity += m1
        else {val entity = coref.newEntity(); entity += m1; entity += bestCand }
      }
    }
    coref
  }

  def getBestCandidate(mentionGraph: MentionGraph, scores: Array[Double], currMentionIdx: Int): (Int,Double) = {
    val antecedentScores = model.normAntecedents(scores)
    var bestIdx = -1
    var bestProb = Double.NegativeInfinity
    for (anteIdx <- 0 to currMentionIdx) {
      val currProb = antecedentScores(anteIdx)
      if (bestIdx == -1 || currProb > bestProb) {
        bestIdx = anteIdx
        bestProb = currProb
      }
    }
    (bestIdx,bestProb)
  }
}

class MentionGraphLabel(model: CorefModel,val currentMention: Int, val linkedMention: Int, val initialValue: Boolean, val lossScore: Double, mentions: Seq[Mention],options: CorefOptions) extends LabeledCategoricalVariable(if (initialValue) "YES" else "NO"){
  def domain = model.MentionPairLabelDomain
  lazy val features = new MentionPairFeatures(model,mentions(currentMention),mentions(linkedMention),mentions,options)
}

class MentionGraph(model: CorefModel, val coref: WithinDocCoref, options: CorefOptions, train: Boolean = false){
  var orderedMentionList = coref.mentions.sortBy(m=>m.phrase.start)
  var graph = new Array[Array[MentionGraphLabel]](orderedMentionList.size)
  var prunedEdges = new Array[Array[Boolean]](orderedMentionList.size)

  for (i <- 0 until orderedMentionList.size) {
    prunedEdges(i) = Array.fill(i+1)(false)
  }

  for (currMentionIdx <- 0 until orderedMentionList.size) {
    graph(currMentionIdx) = new Array[MentionGraphLabel](currMentionIdx+1)
    val currentMention = orderedMentionList(currMentionIdx)
    for (anteMentionIdx <- 0 to currMentionIdx) {
      val anteMention = orderedMentionList(anteMentionIdx)
      //If we don't have a constraint on the pair, then add the linking mention as a possible antecedent
      if(!pruneMentionPair(currMentionIdx,anteMentionIdx)){
        val lossScore = if(train) getLossScore(currentMention,anteMention) else 0.0
        var initialValue = false
        if(train){
          initialValue = if(currentMention == anteMention){
            //This is ugly but it's a side effect of having singleton clusters during training
            currentMention.entity == null || (currentMention.entity != null && currentMention == currentMention.entity.getFirstMention)
          } else currentMention.entity != null && anteMention.entity != null && currentMention.entity == anteMention.entity
        }
        graph(currMentionIdx)(anteMentionIdx) = new MentionGraphLabel(model,currMentionIdx,anteMentionIdx,initialValue,lossScore,orderedMentionList,options)
      } else prunedEdges(currMentionIdx)(anteMentionIdx) = true
    }
  }

  def pruneMentionPair(currMentionIdx:Int, anteMentionIdx:Int):Boolean = {
    var skip = false
    val currentMention = orderedMentionList(currMentionIdx)
    val antecedentMention = orderedMentionList(anteMentionIdx)
    val currSentIdx = currentMention.phrase.sentence.indexInSection
    val anteSentIdx = antecedentMention.phrase.sentence.indexInSection
    val cataphora = antecedentMention.phrase.isPronoun && !currentMention.phrase.isPronoun
    val label = currentMention.entity == antecedentMention.entity
    if(cataphora){
        if (label && !options.allowPosCataphora || !label && !options.allowNegCataphora) {
          skip = true
        }
    }
    if (anteMentionIdx < currMentionIdx - options.maxMentDist || (currentMention.phrase.isPronoun && currSentIdx - anteSentIdx > options.maxPronDist)) {
      skip = true
    }
    if(antecedentMention != currentMention && !antecedentMention.phrase.tokens.intersect(currentMention.phrase.tokens).isEmpty) skip = true
    skip
  }

  def getLossScore(currMention:Mention, antMention:Mention):Double = {
    val headCluster = if(currMention.entity ne null) currMention.entity.getFirstMention else currMention
    if (headCluster == currMention && currMention != antMention) {
      falseLinkScore
    } else if (headCluster != currMention && currMention == antMention) {
      falseNewScore
    } else if (headCluster != currMention && currMention.entity == antMention.entity) {
      wrongLinkScore
    } else {
      0.0
    }
  }
  //Berkeley's Tuned Scores
  val falseLinkScore = -0.1
  val falseNewScore = -3.0
  val wrongLinkScore = -1.0
}
