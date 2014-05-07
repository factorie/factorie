package cc.factorie.app.nlp.coref

import cc.factorie.app.nlp.phrase._
import cc.factorie.optimize._
import java.util.concurrent.ExecutorService
import cc.factorie.variable.LabeledCategoricalVariable
import cc.factorie.app.nlp.{Sentence, Token, Document}
import cc.factorie.app.nlp.pos.PennPosTag
import cc.factorie.app.nlp.coref.mention.{ParseAndNerBasedMentionFinding, ParseBasedMentionFinding}


/**
*
*/
//NamedEntity Mention Finding
class NamedEntityStructuredCoreference extends StructuredCoref{
  override def prereqAttrs: Seq[Class[_]] = (ConllProperNounPhraseFinder.prereqAttrs ++ AcronymNounPhraseFinder.prereqAttrs++PronounFinder.prereqAttrs ++ NnpPosNounPhraseFinder.prereqAttrs).distinct
  override def annotateMentions(doc:Document): Unit = {
    (ConllProperNounPhraseFinder(doc) ++ PronounFinder(doc) ++ NnpPosNounPhraseFinder(doc)++ AcronymNounPhraseFinder(doc)).distinct.foreach(phrase => doc.getCoref.addMention(phrase))
    NounPhraseEntityTypeLabeler.process(doc)
    NounPhraseGenderLabeler.process(doc)
    NounPhraseNumberLabeler.process(doc)
  }
}
//Uses Parse Based Mention Finding, best for data with nested mentions in the ontonotes annotation style
class ParseStructuredCoreference extends StructuredCoref{
  override def prereqAttrs: Seq[Class[_]] = ParseBasedMentionFinding.prereqAttrs.toSeq
  override def annotateMentions(doc:Document): Unit = {
    ParseAndNerBasedMentionFinding.process(doc)
    NounPhraseEntityTypeLabeler.process(doc)
    NounPhraseGenderLabeler.process(doc)
    NounPhraseNumberLabeler.process(doc)
  }
}
//Todo: The base Structured Coref class uses Gold Mentions, used for evaluation and tests on ConllData
class StructuredCoref extends CorefSystem[MentionGraph]{
  val options = new Coref1Options
  val model: StructuredCorefModel = new StructuredCorefModel
  override def prereqAttrs: Seq[Class[_]] = Seq(classOf[Sentence],classOf[PennPosTag])
  def tokenAnnotationString(token:Token):String = ???

  def infer(coref:WithinDocCoref):WithinDocCoref = {
    val instance = new MentionGraph(model,coref,options)
    instance.generateGraph(false)
    getPredCorefClusters(instance)
  }

  var numDocs = 0
  def instantiateModel(optimizer:GradientOptimizer,pool:ExecutorService) = new SoftMaxParallelTrainer(optimizer,pool)
  def preprocessCorpus(trainDocs:Seq[Document]) = model.CorefTokenFrequencies.lexicalCounter = LexicalCounter.countLexicalItems(trainDocs.flatMap{_.coref.mentions},trainDocs,20)
  def getCorefStructure(coref:WithinDocCoref) = {
    println("document processed: "+coref.document.name)
    val struct = new MentionGraph(model,coref,options)
    struct.generateGraph(true)
    struct
  }

  def getPredCorefClusters(graph:MentionGraph): WithinDocCoref = {
    //If we add the best mention within a range, then it will have it's own matches and we can have a chain works well.
    val bestMatches = model.decodeAntecedents(graph)
    for(edgeIdx <- 0 until graph.graph.length){
      val bestMatch = graph.orderedMentionList(bestMatches(edgeIdx)._1)
      if(bestMatch!=graph.orderedMentionList(edgeIdx)){
        if(bestMatch.entity ne null) bestMatch.entity += graph.orderedMentionList(edgeIdx)
        else {val entity = graph.coref.newEntity(); entity += graph.orderedMentionList(edgeIdx); entity += graph.orderedMentionList(bestMatches(edgeIdx)._1) }
      }//else entity is a singleton so do not add it to the entities   //Todo:Add it as a singleton cluster again
    }
    graph.coref
  }

  class SoftMaxParallelTrainer(optimizer: GradientOptimizer, pool: ExecutorService) extends ParallelTrainer(optimizer,pool){
    def map(d:MentionGraph): Seq[Example] = model.getExample(d)
  }
}

class MentionGraphLabel(model:CorefModel, val currentMention: Int, val linkedMention: Int, val initialValue: Boolean, val lossScore:Double) extends LabeledCategoricalVariable(if (initialValue) "YES" else "NO"){
  var score = 0.0
  def domain = model.MentionPairLabelDomain
}

class MentionGraph(model:CorefModel,val coref: WithinDocCoref,options:Coref1Options){
  var orderedMentionList = coref.mentions.toSeq
  var graph = new Array[Array[MentionGraphLabel]](orderedMentionList.size)
  val features = new Array[Array[MentionPairFeatures]](orderedMentionList.size)
  var prunedEdges = new Array[Array[Boolean]](orderedMentionList.size)

  for (i <- 0 until orderedMentionList.size) {
    features(i) = Array.fill(i+1)(null.asInstanceOf[MentionPairFeatures])
    prunedEdges(i) = Array.fill(i+1)(false)
  }

  def generateGraph(train: Boolean) = {
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
              currentMention.entity == null || currentMention.entity != null && currentMention == currentMention.entity.children.head
            } else currentMention.entity != null && anteMention.entity != null && currentMention.entity == anteMention.entity
          }
          graph(currMentionIdx)(anteMentionIdx) = new MentionGraphLabel(model,currMentionIdx,anteMentionIdx,initialValue,lossScore)
          features(currMentionIdx)(anteMentionIdx) = new MentionPairFeatures(model,currentMention,anteMention,orderedMentionList,options)
        } else prunedEdges(currMentionIdx)(anteMentionIdx) = true
      }
    }
  }

  def getGoldAntecendents(mentionIdx: Int): Seq[Mention]={
    graph(mentionIdx).filter(_.initialValue).map(_.linkedMention).map(orderedMentionList)
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
    skip

    //Add Constraints
  }

  def getLossScore(currMention:Mention, antMention:Mention):Double = {
    val oracleCluster = if(currMention.entity ne null) currMention.entity.children else Iterable(currMention)
    val headGold = oracleCluster.head
    if (headGold == currMention && currMention != antMention) {
      falseLinkScore
    } else if (headGold != currMention && currMention == antMention) {
      falseNewScore
    } else if (headGold != currMention && currMention.entity == antMention.entity) {
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
