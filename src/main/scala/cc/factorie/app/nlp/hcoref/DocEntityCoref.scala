package cc.factorie.app.nlp.hcoref

import cc.factorie.app.nlp.Document
import cc.factorie.app.nlp.coref.{CrossDocEntity, WithinDocCoref}
import cc.factorie.variable.DiffList

import scala.util.Random

/**
 * @author John Sullivan
 */
abstract class DocEntityCoref {
  _settings =>
  def autoStopThreshold:Int
  def estimateIterations(mentionCount:Int):Int
  def model:CorefModel[DocEntityVars]
  implicit val random:Random


  def process(docs:Iterable[Document]):Iterable[CrossDocEntity] = {
    assert(docs.forall(_.hasAnnotation(classOf[WithinDocCoref])))

    // by mentions here we mean cross-doc mentions that correspond to within-doc entities
    val mentions = docs.flatMap { doc =>
      doc.coref.entities.map{ winDocEntity =>
        new Mention[DocEntityVars](DocEntityVars.fromWithinDocEntity(winDocEntity), java.util.UUID.randomUUID.toString, winDocEntity.uniqueId)(null)
      }
    }

    val sampler = getSampler(mentions)

    sampler.infer

    mentions.map(_.root).toSeq
  }


  def getSampler(mentions:Iterable[Node[DocEntityVars]]) = new CorefSampler[DocEntityVars](_settings.model, mentions, _settings.estimateIterations(mentions.size))
    with AutoStoppingSampler[DocEntityVars]
    with CanopyPairGenerator[DocEntityVars]
    with NoSplitMoveGenerator[DocEntityVars]
    with DebugCoref[DocEntityVars]
    with TrainingObjective[DocEntityVars] {
    def newInstance(implicit d: DiffList) = new Node[DocEntityVars](new DocEntityVars())

    val autoStopThreshold = _settings.autoStopThreshold
  }

}

class DocEntityCorefModel(namesWeights:Double, namesShift:Double, nameEntropy:Double, contextsWeight:Double, contextsShift:Double, genderWeight:Double, genderShift:Double, mentionWeight:Double, mentionShift:Double, numberWeight:Double, numberShift:Double) extends CorefModel[DocEntityVars] {
  this += new ChildParentCosineDistance(namesWeights, namesShift, {v:DocEntityVars => v.names})
  this += new ChildParentCosineDistance(contextsWeight, contextsShift, {v:DocEntityVars => v.context})
  this += new ChildParentCosineDistance(genderWeight, genderShift, {v:DocEntityVars => v.nerType})
  this += new ChildParentCosineDistance(mentionWeight, mentionShift, {v:DocEntityVars => v.mention})
  this += new ChildParentCosineDistance(numberWeight, numberShift, {v:DocEntityVars => v.number})
  this += new BagOfWordsEntropy(nameEntropy, {v:DocEntityVars => v.names})
}

// todo code for model training
// todo train model on tac entity linking
// todo serialize and deserialize models
