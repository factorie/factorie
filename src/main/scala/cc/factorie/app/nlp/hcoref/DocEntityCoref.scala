/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
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
    with TrainingObjective[DocEntityVars]
    with PrintlnLogger {
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
