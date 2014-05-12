package cc.factorie.app.nlp.coref

import cc.factorie._
import cc.factorie.la
import cc.factorie.util.DoubleAccumulator
import cc.factorie.la.{SparseBinaryTensor, DenseTensor1, WeightsMapAccumulator, Tensor1}
import cc.factorie.optimize.{OptimizableObjectives, PredictorExample, Example}
import java.io._
import cc.factorie.util.BinarySerializer
import cc.factorie.variable.{VectorDomain, DiscreteDomain, CategoricalVectorDomain, CategoricalDomain}
import cc.factorie.model.Parameters

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
    var counter:TopTokenFrequencies = null
  }

  def deserialize(stream: DataInputStream) {
    val headWords = new DefaultHashMap[String,Int](0)
    BinarySerializer.deserialize(headWords, stream)
    BinarySerializer.deserialize(MentionPairFeaturesDomain, stream)
    BinarySerializer.deserialize(new CategoricalVectorDomain[String] { val domain = new CategoricalDomain[String]} , stream)
    BinarySerializer.deserialize(this, stream)
    CorefTokenFrequencies.counter = new TopTokenFrequencies(headWords)
    stream.close()
    MentionPairFeaturesDomain.freeze()
  }

  def deserialize(filename: String) {
    deserialize(new DataInputStream(new BufferedInputStream(new FileInputStream(filename))))
  }

  def serialize(stream: DataOutputStream) {
    BinarySerializer.serialize(CorefTokenFrequencies.counter.headWords,stream)
    MentionPairFeaturesDomain.freeze()
    BinarySerializer.serialize(MentionPairFeaturesDomain , stream)
    BinarySerializer.serialize(new CategoricalVectorDomain[String] { val domain = new CategoricalDomain[String]}, stream)
    BinarySerializer.serialize(this,stream)
  }

}

abstract class PairwiseCorefModel extends app.classify.backend.OptimizablePredictor[Double,Tensor1] with CorefModel{
  def getExample(label: MentionPairLabel,features:MentionPairFeatures, scale: Double): Example = new PredictorExample(this, features.value, if (label.target.categoryValue == "YES") 1 else -1, OptimizableObjectives.hingeScaledBinary(1.0, 3.0))
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
  val pairwiseWeights = Weights(new la.DenseTensor1(MentionPairFeaturesDomain.dimensionDomain.maxSize))

  def predict(pairwiseStats: Tensor1) = pairwiseWeights.value dot pairwiseStats

  def getExample(mentionGraph: MentionGraph): Seq[Example] = {
    Seq(new GraphExample(this, mentionGraph))
  }

  def getExamples(graphs: Seq[MentionGraph]): Seq[Example] = {
    graphs.map{g => new GraphExample(this,g)}
  }

  override def deserialize(stream: DataInputStream) {
    val firstWords = new DefaultHashMap[String,Int](0)
    val headWords = new DefaultHashMap[String,Int](0)
    val lastWords = new DefaultHashMap[String,Int](0)
    val precContext = new DefaultHashMap[String,Int](0)
    val followContext = new DefaultHashMap[String,Int](0)
    val wordForm = new DefaultHashMap[String,Int](0)
    val shapes = new DefaultHashMap[String,Int](0)
    BinarySerializer.deserialize(headWords,stream)
    BinarySerializer.deserialize(firstWords,stream)
    BinarySerializer.deserialize(lastWords,stream)
    BinarySerializer.deserialize(precContext,stream)
    BinarySerializer.deserialize(followContext,stream)
    BinarySerializer.deserialize(wordForm,stream)
    BinarySerializer.deserialize(shapes,stream)
    BinarySerializer.deserialize(MentionPairFeaturesDomain, stream)
    BinarySerializer.deserialize(new CategoricalVectorDomain[String] { val domain = new CategoricalDomain[String]} , stream)
    BinarySerializer.deserialize(this, stream)
    val newLexicalCounts = new TopTokenFrequencies(headWords,firstWords,lastWords,precContext,followContext,wordForm,shapes)
    stream.close()
    CorefTokenFrequencies.counter = newLexicalCounts
    MentionPairFeaturesDomain.freeze()
  }

  override def serialize(stream: DataOutputStream) {
    MentionPairFeaturesDomain.freeze()
    BinarySerializer.serialize(CorefTokenFrequencies.counter.headWords,stream)
    BinarySerializer.serialize(CorefTokenFrequencies.counter.firstWords,stream)
    BinarySerializer.serialize(CorefTokenFrequencies.counter.lastWords,stream)
    BinarySerializer.serialize(CorefTokenFrequencies.counter.precContext,stream)
    BinarySerializer.serialize(CorefTokenFrequencies.counter.followContext,stream)
    BinarySerializer.serialize(CorefTokenFrequencies.counter.shapes,stream)
    BinarySerializer.serialize(CorefTokenFrequencies.counter.wordForm,stream)
    BinarySerializer.serialize(MentionPairFeaturesDomain, stream)
    BinarySerializer.serialize(new CategoricalVectorDomain[String] { val domain = new CategoricalDomain[String]}, stream)
    BinarySerializer.serialize(this,stream)
  }

  def getBestCandidatesAntecedents(mentionGraph: MentionGraph): Array[(Int,Double)] = {
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

  def normAntecedents(scores: Array[Double]): Array[Double] = {
    val antecedents = scores.map(Math.exp)
    val total = antecedents.reduce(_+_)
    for(anteIn <- 0 until antecedents.length) {
      antecedents(anteIn) /= total
    }
    antecedents
  }

  def findBestAntecedents(mentionGraph: MentionGraph, scoreFunction: Int => Array[Double]): Array[(Int,Double)] = {
    val anaphors = new Array[(Int,Double)](mentionGraph.graph.length)
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
      anaphors(currMentionIdx) = (bestIdx,bestProb)
    }
    anaphors
  }

  def scoreGraph(mentionGraph: MentionGraph): Array[Array[Double]] = {
    val scores = new Array[Array[Double]](mentionGraph.graph.length)
    for (i <- 0 until mentionGraph.graph.length) {
      scores(i) = new Array[Double](i+1)
      for (j <- 0 until mentionGraph.graph(i).length) {
        if(mentionGraph.prunedEdges(i)(j)) scores(i)(j) = Double.NegativeInfinity
        else{
          require(mentionGraph.graph(i)(j).features.domain.dimensionSize > 0)
          scores(i)(j) = predict(mentionGraph.graph(i)(j).features.value)
        }
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

  def computeLikelihood( mentionGraph: MentionGraph, goldMarginal: Array[Array[Double]],predictedMarginalScores: Array[Array[Double]]): Double = {
    var likelihood = 0.0
    for (currIdx <- 0 until mentionGraph.graph.length) {
      val currMention = mentionGraph.orderedMentionList(currIdx)
      var goldAntecedents = if(currMention.entity ne null) currMention.entity.mentions.filter(m => m.phrase.start < currMention.phrase.start) else Iterable.empty
      if(goldAntecedents.isEmpty) goldAntecedents = Set(currMention)
      var currProb = 0.0
      for (linkIdx <- 0 until goldAntecedents.size) {
        if(currIdx == -1 || linkIdx == -1 || mentionGraph.prunedEdges(currIdx)(linkIdx)) currProb += 0.0
        else currProb += goldMarginal(currIdx)(linkIdx) - predictedMarginalScores(currIdx)(linkIdx)
      }
      var currLogProb = Math.log(currProb)
      if (currLogProb.isInfinite) currLogProb = -30
      likelihood += currLogProb
    }
    likelihood
  }
}

class GraphExample[Output, Prediction, Input<:MentionGraph](model: StructuredCorefModel, input: Input) extends Example {
  def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator) {
    val scores = model.scoreGraph(input)
    val predictionMarginalScores = model.calculateMarginals(scores,input,gold = false)
    val goldMarginalScores = model.calculateMarginals(scores,input,gold = true)
    val likelihood = model.computeLikelihood(input,goldMarginalScores,predictionMarginalScores)
    if (value != null) value.accumulate(likelihood)
    for (i <- 0 until input.graph.length) {
      for (edgeIdx <- 0 until input.graph(i).length; if !input.prunedEdges(i)(edgeIdx)) {
        if(gradient != null){
          gradient.accumulate(model.pairwiseWeights, input.graph(i)(edgeIdx).features.value, goldMarginalScores(i)(edgeIdx) - predictionMarginalScores(i)(edgeIdx))
        }
      }
    }
  }
}

