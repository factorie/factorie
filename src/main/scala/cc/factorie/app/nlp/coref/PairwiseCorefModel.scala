package cc.factorie.app.nlp.coref

import cc.factorie._
import cc.factorie.la
import cc.factorie.util.DoubleAccumulator
import cc.factorie.la.{SparseBinaryTensor, DenseTensor1, WeightsMapAccumulator, Tensor1}
import cc.factorie.optimize.{OptimizableObjectives, PredictorExample, Example}
import java.io._
import cc.factorie.util.BinarySerializer
import cc.factorie.util.BasicEvaluatableClustering
import cc.factorie.variable.{VectorDomain, DiscreteDomain, CategoricalVectorDomain, CategoricalDomain}
import cc.factorie.model.Parameters
import scala.collection.mutable
import cc.factorie.app.nlp.coref.DefaultHashMap

/**
 * User: apassos
 * Date: 6/27/13
 * Time: 12:20 PM
 */

trait CorefModel extends Parameters {
  val MentionPairFeaturesDomain = new CategoricalVectorDomain[String] {
    dimensionDomain.maxSize = 1e6.toInt
    dimensionDomain.growPastMaxSize = false
  }
  val MentionPairCrossFeaturesDomain = new VectorDomain {
    def dimensionDomain: DiscreteDomain = new DiscreteDomain(5e6.toInt + 1)
  }

  val MentionPairLabelDomain = new CategoricalDomain[String] { this += "YES"; this += "NO"; freeze() }

  object CorefTokenFrequencies{
    var lexicalCounter:LexicalCounter = null
  }

  def deserialize(stream: DataInputStream) {
    val headWordCounts = new DefaultHashMap[String,Int](0)
    BinarySerializer.deserialize(headWordCounts, stream)
    BinarySerializer.deserialize(MentionPairFeaturesDomain, stream)
    BinarySerializer.deserialize(new CategoricalVectorDomain[String] { val domain = new CategoricalDomain[String]} , stream)
    BinarySerializer.deserialize(this, stream)
    CorefTokenFrequencies.lexicalCounter = new LexicalCounter(headWordCounts)
    stream.close()
    MentionPairFeaturesDomain.freeze()
  }

  def deserialize(filename: String) {
    deserialize(new DataInputStream(new BufferedInputStream(new FileInputStream(filename))))
  }

  def serialize(stream: DataOutputStream) {
    BinarySerializer.serialize(CorefTokenFrequencies.lexicalCounter.headWordCounts,stream)
    MentionPairFeaturesDomain.freeze()
    BinarySerializer.serialize(MentionPairFeaturesDomain , stream)
    BinarySerializer.serialize(new CategoricalVectorDomain[String] { val domain = new CategoricalDomain[String]}, stream)
    BinarySerializer.serialize(this,stream)
  }

//  def generateTrueClustering(mentions: Seq[Mention]): BasicEvaluatableClustering = {
//    val trueMap = new GenericEntityMap[Mention]
//    mentions.foreach(m => trueMap.addMention(m, trueMap.numMentions.toLong))
//    val entities = mentions.groupBy(_.entity)
//    entities.flatMap(_._2.sliding(2)).foreach(p => {
//      if (p.size == 2) trueMap.addCoreferentPair(p(0), p(1))
//    })
//    trueMap
//  }

}

abstract class PairwiseCorefModel extends app.classify.backend.OptimizablePredictor[Double,Tensor1] with CorefModel{
  def getExample(label: MentionPairLabel,features:MentionPairFeatures, scale: Double): Example = new PredictorExample(this, label.features.value, if (label.target.categoryValue == "YES") 1 else -1, OptimizableObjectives.hingeScaledBinary(1.0, 3.0))

}

class BaseCorefModel extends PairwiseCorefModel {
  val pairwise = Weights(new la.DenseTensor1(MentionPairFeaturesDomain.dimensionDomain.maxSize))
  def predict(pairwiseStats: Tensor1) = pairwise.value dot pairwiseStats
  def accumulateObjectiveGradient(accumulator: WeightsMapAccumulator, features: Tensor1, gradient: Double, weight: Double) = accumulator.accumulate(pairwise, features, gradient * weight)
}

class ImplicitCrossProductCorefModel extends PairwiseCorefModel {
  val products = Weights(new DenseTensor1(MentionPairCrossFeaturesDomain.dimensionDomain.size))
  val pairwise = Weights(new la.DenseTensor1(MentionPairFeaturesDomain.dimensionDomain.maxSize))
  val domain = new ImplicitDomain(MentionPairFeaturesDomain.dimensionSize)
  def predict(pairwiseStats: Tensor1) =
    pairwise.value.dot(pairwiseStats) + products.value.dot(new ImplicitFeatureConjunctionTensor(MentionPairCrossFeaturesDomain.dimensionSize, pairwiseStats.asInstanceOf[SparseBinaryTensor], domain))
  def accumulate(acc: WeightsMapAccumulator, pairwiseStats: Tensor1, f: Double) {
    acc.accumulate(pairwise, pairwiseStats, f)
    acc.accumulate(products, new ImplicitFeatureConjunctionTensor(
    MentionPairCrossFeaturesDomain.dimensionSize, pairwiseStats.asInstanceOf[SparseBinaryTensor], domain), f)
  }

  def accumulateObjectiveGradient(accumulator: WeightsMapAccumulator, features: Tensor1, gradient: Double, weight: Double) = {
    accumulator.accumulate(pairwise, features, gradient)
    accumulator.accumulate(products, new ImplicitFeatureConjunctionTensor(
    MentionPairCrossFeaturesDomain.dimensionSize, features.asInstanceOf[SparseBinaryTensor], domain), gradient * weight)
  }
}



class StructuredCorefModel extends CorefModel {
  val pairwiseWeights = Weights(new la.DenseTensor1(MentionPairFeaturesDomain.dimensionDomain.size))

  def predict(pairwiseStats: Tensor1) = pairwiseWeights.value dot pairwiseStats

  def getExample(mentionGraph: MentionGraph): Seq[Example] = {
    Seq(new GraphExample(this, mentionGraph))
  }

  def getExamples(graphs: Seq[MentionGraph]): Seq[Example] = {
    graphs.map{g => new GraphExample(this,g)}
  }

  override def deserialize(stream: DataInputStream) {
    val firstWordCounts = new DefaultHashMap[String,Int](0)
    val headWordCounts = new DefaultHashMap[String,Int](0)
    val lastWordCounts = new DefaultHashMap[String,Int](0)
    val precWordCounts = new DefaultHashMap[String,Int](0)
    val followWordCounts = new DefaultHashMap[String,Int](0)
    val classCounts = new DefaultHashMap[String,Int](0)
    val shapeCounts = new DefaultHashMap[String,Int](0)
    BinarySerializer.deserialize(headWordCounts,stream)
    BinarySerializer.deserialize(firstWordCounts,stream)
    BinarySerializer.deserialize(lastWordCounts,stream)
    BinarySerializer.deserialize(precWordCounts,stream)
    BinarySerializer.deserialize(followWordCounts,stream)
    BinarySerializer.deserialize(classCounts,stream)
    BinarySerializer.deserialize(shapeCounts,stream)
    BinarySerializer.deserialize(MentionPairFeaturesDomain, stream)
    BinarySerializer.deserialize(new CategoricalVectorDomain[String] { val domain = new CategoricalDomain[String]} , stream)
    BinarySerializer.deserialize(this, stream)
    val newLexicalCounts = new LexicalCounter(headWordCounts,firstWordCounts,lastWordCounts,precWordCounts,followWordCounts,classCounts,shapeCounts)
    stream.close()
    CorefTokenFrequencies.lexicalCounter = newLexicalCounts
    MentionPairFeaturesDomain.freeze()
  }

  override def serialize(stream: DataOutputStream) {
    MentionPairFeaturesDomain.freeze()
    BinarySerializer.serialize(CorefTokenFrequencies.lexicalCounter.headWordCounts,stream)
    BinarySerializer.serialize(CorefTokenFrequencies.lexicalCounter.firstWordCounts,stream)
    BinarySerializer.serialize(CorefTokenFrequencies.lexicalCounter.lastWordCounts,stream)
    BinarySerializer.serialize(CorefTokenFrequencies.lexicalCounter.precedingWordCounts,stream)
    BinarySerializer.serialize(CorefTokenFrequencies.lexicalCounter.followingWordCounts,stream)
    BinarySerializer.serialize(CorefTokenFrequencies.lexicalCounter.shapeCounts,stream)
    BinarySerializer.serialize(CorefTokenFrequencies.lexicalCounter.classCounts,stream)
    BinarySerializer.serialize(MentionPairFeaturesDomain, stream)
    BinarySerializer.serialize(new CategoricalVectorDomain[String] { val domain = new CategoricalDomain[String]}, stream)
    BinarySerializer.serialize(this,stream)
  }

  def decodeAntecedents(mentionGraph: MentionGraph): Array[(Int,Double)] = {
    val scores = scoreGraph(mentionGraph)
    val cluster = findBestAntecedents(mentionGraph, (idx: Int) => {
      val probs = scores(idx).map(Math.exp)
      val total = probs.reduce(_+_)
      var i = 0
      while (i < probs.size) {
        probs(i) /= total
        i += 1
      }
      probs
    })
    cluster
  }

  def findBestAntecedents(mentionGraph: MentionGraph, scoreFunction: Int => Array[Double]): Array[(Int,Double)] = {
    val backpointers = new Array[(Int,Double)](mentionGraph.graph.length)
    for (currMentionIdx <- 0 until mentionGraph.graph.length) {
      val allAnteCandidates = scoreFunction(currMentionIdx)
      var bestIdx = -1
      var bestProb = Double.NegativeInfinity
      for (anteIdx <- 0 to currMentionIdx) {
        val currProb = allAnteCandidates(anteIdx)
        if (bestIdx == -1 || currProb > bestProb) {
          bestIdx = anteIdx
          bestProb = currProb
        }
      }
      backpointers(currMentionIdx) = (bestIdx,bestProb)
    }
    backpointers
  }

  def scoreGraph(mentionGraph: MentionGraph): Array[Array[Double]] = {
    val scores = new Array[Array[Double]](mentionGraph.graph.length)
    for (i <- 0 until mentionGraph.graph.length) {
      scores(i) = new Array[Double](i+1)
      for (j <- 0 until mentionGraph.graph(i).length; if(!mentionGraph.prunedEdges(i)(j))) {
        require(mentionGraph.features(i)(j).domain.dimensionSize > 0)
        scores(i)(j) = predict(mentionGraph.features(i)(j).value)
      }
    }
    scores
  }

  def calculateMarginals(scores:Array[Array[Double]],mentionGraph: MentionGraph, gold: Boolean = false):Array[Array[Double]] = {
    val marginals = new Array[Array[Double]](mentionGraph.graph.length)
    for (i <- 0 until mentionGraph.graph.length) {
      var normalizer = 0.0
      val goldAntecedents = if (gold) mentionGraph.graph(i).filter(p => p != null && p.initialValue) else null
      marginals(i) = Array.fill(mentionGraph.graph(i).length)(0.0)
      for(edgeIdx<- 0 until mentionGraph.graph(i).length){
        if(!mentionGraph.prunedEdges(i)(edgeIdx)){
        val edge = mentionGraph.graph(i)(edgeIdx)
        if (!gold || goldAntecedents.contains(edge)) {
          //pair loss score is set at graph generation
          val unnormalizedProb = Math.exp(scores(i)(edgeIdx) - edge.lossScore)
          marginals(i)(edgeIdx) = unnormalizedProb
          normalizer += unnormalizedProb
        } else {
          marginals(i)(edgeIdx) = 0.0
        } }else marginals(i)(edgeIdx) = 0.0
      }
      for(edgeIdx<- 0 until mentionGraph.graph(i).length){
        marginals(i)(edgeIdx) = if(normalizer == 0) 0.0 else marginals(i)(edgeIdx) / normalizer
      }
    }
    marginals
  }

  def computeOverallLikelihood(trainMentionGraphs: Seq[MentionGraph]): Double = {
    //trainMentionGraphs.foldLeft(0.0)((likelihood, mentionGraph) =>{

    //val predictionMarginalScores = calculateMarginals( mentionGraph,false)
    //val goldMarginalScores = calculateMarginals(mentionGraph,true)
    //likelihood + computeLikelihood(mentionGraph,goldMarginalScores,predictionMarginalScores)
    //})
    -1.0
  }

  def computeLikelihood( mentionGraph: MentionGraph, goldMarginal: Array[Array[Double]],predictedMarginalScores: Array[Array[Double]]): Double = {
    var likelihood = 0.0
    for (currIdx <- 0 to mentionGraph.graph.length-1) {
      val currMention = mentionGraph.orderedMentionList(currIdx)
      var goldAntecedents = if(currMention.entity ne null) currMention.entity.mentions.filter(m => mentionGraph.orderedMentionList.indexOf(m) < currIdx) else Iterable.empty
      if(goldAntecedents.isEmpty) goldAntecedents = Set(currMention)
      val edgeIdx = mentionGraph.orderedMentionList.indexOf(goldAntecedents.head)
      var currProb = 0.0
      for (edgeLink <- goldAntecedents) {
        val edgeIdx = mentionGraph.orderedMentionList.indexOf(edgeLink)
        if(currIdx == -1 || edgeIdx == -1 || mentionGraph.prunedEdges(currIdx)(edgeIdx)) currProb += 0.0
        else currProb += goldMarginal(currIdx)(edgeIdx) - predictedMarginalScores(currIdx)(edgeIdx)
      }
      var currLogProb = Math.log(currProb)
      if (currLogProb.isInfinite) {
        currLogProb = -30
      }
      likelihood += currLogProb
    }
    likelihood
  }
}

class GraphExample[Output, Prediction, Input<:MentionGraph](model: StructuredCorefModel, input: Input) extends Example {
  def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator) {
    val scores = model.scoreGraph(input)
    //Add a parameter if we want to do gold only? Or should this be handled further up
    val predictionMarginalScores = model.calculateMarginals(scores,input,false)
    val goldMarginalScores = model.calculateMarginals(scores,input,true)
    val likelihood = model.computeLikelihood(input,goldMarginalScores,predictionMarginalScores)
    if (value != null) value.accumulate(likelihood)
    //if (gradient != null) model.accumulateObjectiveGradient(gradient, input, ograd, weight)
    for (i <- 0 until input.graph.length) {
      for (edgeIdx <- 0 until input.graph(i).length; if !input.prunedEdges(i)(edgeIdx)) {
        if(gradient != null){
          gradient.accumulate(model.pairwiseWeights, input.features(i)(edgeIdx).value, goldMarginalScores(i)(edgeIdx) - predictionMarginalScores(i)(edgeIdx))
        }
      }
    }
  }
}

