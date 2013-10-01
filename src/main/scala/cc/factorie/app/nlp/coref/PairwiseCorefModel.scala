package cc.factorie.app.nlp.coref

import cc.factorie._
import scala.collection.mutable
import cc.factorie.la.{SparseBinaryTensor, DenseTensor1, WeightsMapAccumulator, Tensor1}
import cc.factorie.optimize.Example
import java.io.{DataOutputStream, File, FileInputStream, DataInputStream}
import cc.factorie.util.BinarySerializer
import cc.factorie.app.nlp.mention.Mention
import cc.factorie.util.coref.GenericEntityMap
import cc.factorie.variable.{VectorDomain, DiscreteDomain, CategoricalVectorDomain, CategoricalDomain}
import cc.factorie.model.Parameters

/**
 * User: apassos
 * Date: 6/27/13
 * Time: 12:20 PM
 */

trait PairwiseCorefModel extends Parameters {
  val MentionPairFeaturesDomain = new CategoricalVectorDomain[String] {
    dimensionDomain.maxSize = 1e6.toInt
    dimensionDomain.growPastMaxSize = false
  }
  val MentionPairCrossFeaturesDomain = new VectorDomain {
    def dimensionDomain: DiscreteDomain = new DiscreteDomain(5e6.toInt + 1)
  }

  val MentionPairLabelDomain = new CategoricalDomain[String] { this += "YES"; this += "NO"; freeze() }

  object MentionPairLabelThing {
    val tokFreq = new mutable.HashMap[String, Int]()
  }
  def score(pairwiseStats: Tensor1): Double
  def accumulate(acc: WeightsMapAccumulator, pairwiseStats: Tensor1, f: Double)
  def getExample(label: MentionPairLabel, scale: Double): Example

  def deserialize(stream: DataInputStream) {
    BinarySerializer.deserialize(MentionPairLabelThing.tokFreq, stream)
    BinarySerializer.deserialize(MentionPairFeaturesDomain, stream)
    BinarySerializer.deserialize(new CategoricalVectorDomain[String] { val domain = new CategoricalDomain[String]} , stream)
    BinarySerializer.deserialize(this, stream)
    stream.close()
    MentionPairFeaturesDomain.freeze()
  }

  def deserialize(filename: String) {
    deserialize(new DataInputStream(new FileInputStream(filename)))
  }

  def serialize(stream: DataOutputStream) {
    BinarySerializer.serialize(MentionPairLabelThing.tokFreq,stream)
    MentionPairFeaturesDomain.freeze()
    BinarySerializer.serialize(MentionPairFeaturesDomain , stream)
    BinarySerializer.serialize(new CategoricalVectorDomain[String] { val domain = new CategoricalDomain[String]}, stream)
    BinarySerializer.serialize(this,stream)
  }

  def generateTrueMap(mentions: Seq[Mention]): GenericEntityMap[Mention] = {
    val trueMap = new GenericEntityMap[Mention]
    mentions.foreach(m => trueMap.addMention(m, trueMap.numMentions.toLong))
    val entities = mentions.groupBy(_.attr[cc.factorie.app.nlp.mention.Entity])
    entities.flatMap(_._2.sliding(2)).foreach(p => {
      if (p.size == 2) trueMap.addCoreferentPair(p(0), p(1))
    })
    trueMap
  }

}

class BaseCorefModel extends PairwiseCorefModel {
  val pairwise = Weights(new la.DenseTensor1(MentionPairFeaturesDomain.dimensionDomain.maxSize))
  def accumulate(acc: WeightsMapAccumulator, pairwiseStats: Tensor1, f: Double) {
    acc.accumulate(pairwise, pairwiseStats, f)
  }
  def score(pairwiseStats: Tensor1) = pairwise.value dot pairwiseStats

  def getExample(label: MentionPairLabel, scale: Double) = new LeftRightExample(pairwise, label,scale)
}

class ImplicitCrossProductCorefModel extends PairwiseCorefModel {
  val products = Weights(new DenseTensor1(MentionPairCrossFeaturesDomain.dimensionDomain.size))
  val pairwise = Weights(new la.DenseTensor1(MentionPairFeaturesDomain.dimensionDomain.maxSize))
  val domain = new ImplicitDomain(MentionPairFeaturesDomain.dimensionSize)
  def score(pairwiseStats: Tensor1) =
    pairwise.value.dot(pairwiseStats) + products.value.dot(new ImplicitFeatureConjunctionTensor(MentionPairCrossFeaturesDomain.dimensionSize, pairwiseStats.asInstanceOf[SparseBinaryTensor], domain))
  def accumulate(acc: WeightsMapAccumulator, pairwiseStats: Tensor1, f: Double) {
    acc.accumulate(pairwise, pairwiseStats, f)
    acc.accumulate(products, new ImplicitFeatureConjunctionTensor(
        MentionPairCrossFeaturesDomain.dimensionSize, pairwiseStats.asInstanceOf[SparseBinaryTensor], domain), f)
  }

  def getExample(label: MentionPairLabel, scale: Double) = new LeftRightImplicitConjunctionExample(this, label,scale)
}

