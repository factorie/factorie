package cc.factorie.app.nlp.coref

import cc.factorie.app.nlp.phrase.{Phrase, NounPhraseTypeDomain, NounPhraseType, OntonotesPhraseEntityType}
import cc.factorie.app.nlp.wordnet.WordNet
import cc.factorie.optimize._
import java.util.concurrent.ExecutorService
import cc.factorie.variable.LabeledCategoricalVariable
import cc.factorie.app.nlp.{Token, Document}
import cc.factorie.app.nlp.ner.OntonotesEntityTypeDomain


/**
*
*/

class StructuredCoref extends CorefSystem[MentionGraph]{
  val options = new Coref1Options
  val model: StructuredCorefModel = new StructuredCorefModel
  override def prereqAttrs: Seq[Class[_]] = Seq(classOf[WithinDocCoref], classOf[OntonotesPhraseEntityType])
  def tokenAnnotationString(token:Token):String = {
    ""
  }
  override def process(document:Document) = {
    val phrases = getPhrases(document)
    val coref = new WithinDocCoref(document)
    infer(document.coref)
    document
  }

  //requires coref on document first
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
    val corefMentions = graph.coref.mentions
    //If we add the best mention within a range, then it will have it's own matches and we can have a chain works well.
    val bestMatches = model.decodeAntecedents(graph)
    for(edgeIdx <- 0 until graph.graph.length){
      //doc.orderedMentionList(edgeIdx).setDecision(bestMatches(edgeIdx))
      val bestMatch = graph.orderedMentionList(bestMatches(edgeIdx)._1)
      if(bestMatch.entity ne null) bestMatch.entity += graph.orderedMentionList(edgeIdx)
      else {val entity = graph.coref.newEntity(); entity += graph.orderedMentionList(edgeIdx); entity += graph.orderedMentionList(bestMatches(edgeIdx)._1) }
    }
    graph.coref
  }

  class SoftMaxParallelTrainer(optimizer: GradientOptimizer, pool: ExecutorService) extends ParallelTrainer(optimizer,pool){
    def map(d:MentionGraph): Seq[Example] = {
      println("Document: " +d.coref.document.name + "   " + numDocs)
      numDocs += 1
      model.getExample(d)
    }
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
  var prunedEdges = new Array[Array[Boolean]](orderedMentionList.size)  //toTensor2

  for (i <- 0 until orderedMentionList.size) {
    features(i) = Array.fill(i+1)(null.asInstanceOf[MentionPairFeatures])
    prunedEdges(i) = Array.fill(i+1)(false)
  }

  //lazy val firstMentions:Map[WithinDocEntity,Mention] = doc.coref.target.entities.map(entity => entity -> entity.mentions.minBy(_.phrase.start)).toMap

  def generateGraph(train: Boolean) = {
    for (currMentionIdx <- 0 until orderedMentionList.size) {
      graph(currMentionIdx) = new Array[MentionGraphLabel](currMentionIdx+1)
      val currentMention = orderedMentionList(currMentionIdx)
      for (anteMentionIdx <- 0 to currMentionIdx) {
        val anteMention = orderedMentionList(anteMentionIdx)
        //If we don't have a constraint on the pair, then add the linking mention as a possible antecedent
        if(!pruneMentionPair(currentMention,anteMention)){
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

  def pruneMentionPair(currentMention:Mention, antecedentMention:Mention):Boolean = {
    val cataphora = antecedentMention.phrase.isPronoun && !currentMention.phrase.isPronoun
    val label = currentMention.entity == antecedentMention.entity
    if(cataphora){
        if (label && !options.allowPosCataphora || !label && !options.allowNegCataphora) {
          true
        }
        else false
    }else false
    //false
    //Add Constraints
  }

  private def constrainPronounDistance() {
    var iSentIdx = 0
    var jSentIdx = 0
    for (i <- 0 until prunedEdges.length) {
      for (j <- 0 to i) {
        iSentIdx = orderedMentionList(i).phrase.sentence.indexInSection
        jSentIdx = orderedMentionList(j).phrase.sentence.indexInSection
        if (j < i - options.maxMentDist || (orderedMentionList(i).attr[MentionCharacteristics].isPRO && iSentIdx - jSentIdx > options.maxPronDist)) {
          prunedEdges(i)(j) = true
          features(i)(j) = null
        }
      }
    }
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
  val falseLinkScore = -0.1
  val falseNewScore = -3.0
  val wrongLinkScore = -1.0

}
