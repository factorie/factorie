/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie.app.chain

import cc.factorie._
import cc.factorie.la._
import cc.factorie.optimize._
import scala.collection.mutable.{ListBuffer,ArrayBuffer}

class ChainModel[Label<:LabeledMutableDiscreteVarWithTarget[_], Features<:CategoricalTensorVar[String], Token<:Observation[Token]]
(val labelDomain:DiscreteDomain,
 val featuresDomain:CategoricalTensorDomain[String],
 val labelToFeatures:Label=>Features,
 val labelToToken:Label=>Token,
 val tokenToLabel:Token=>Label) 
 (implicit lm:Manifest[Label], fm:Manifest[Features], tm:Manifest[Token])
extends ModelWithContext[IndexedSeq[Label]] with Infer //with Trainer[ChainModel[Label,Features,Token]]
{
  val labelClass = lm.erasure
  val featureClass = fm.erasure
  val tokenClass = tm.erasure
  object bias extends DotFamilyWithStatistics1[Label] {
    factorName = "Label"
    lazy val weights = new la.DenseTensor1(labelDomain.size)
  }
  object obs extends DotFamilyWithStatistics2[Label,Features] {
    factorName = "Label,Token"
    lazy val weights = new la.DenseTensor2(labelDomain.size, featuresDomain.dimensionSize)
  }
  object markov extends DotFamilyWithStatistics2[Label,Label] {
    factorName = "Label,Label"
    lazy val weights = new la.DenseTensor2(labelDomain.size, labelDomain.size)
  }
  object obsmarkov extends DotFamilyWithStatistics3[Label,Label,Features] {
    factorName = "Label,Label,Token"
    lazy val weights = new la.Dense2LayeredTensor3(labelDomain.size, featuresDomain.dimensionSize, featuresDomain.dimensionSize, new la.SparseTensor1(_))
  }
  override def families = Seq(bias, obs, markov, obsmarkov) 
  def factorsWithContext(labels:IndexedSeq[Label]): Iterable[Factor] = {
    val result = new ListBuffer[Factor]
    for (i <- 0 until labels.length) {
      result += bias.Factor(labels(i))
      result += obs.Factor(labels(i), labelToFeatures(labels(i)))
      if (i > 0) result += markov.Factor(labels(i-1), labels(i))
    }
    result
  }
  override def factors(variables:Iterable[Variable]): Iterable[Factor] = variables match {
    case variables:IndexedSeq[Label] if (variables.forall(v => labelClass.isAssignableFrom(v.getClass))) => factorsWithContext(variables)
    case _ => super.factors(variables)
  }
  def factors(v:Variable) = v match {
    case label:Label if (label.getClass eq labelClass) => {
      val result = new ArrayBuffer[Factor](4)
      result += bias.Factor(label)
      result += obs.Factor(label, labelToFeatures(label))
      val token = labelToToken(label)
      if (token.hasPrev)
        result += markov.Factor(tokenToLabel(token.prev), label)
      if (token.hasNext)
        result += markov.Factor(label, tokenToLabel(token.next))
      result
    }
  }
  
  class ChainSummary extends Summary[DiscreteMarginal] {
    def marginals: Iterable[DiscreteMarginal] = Nil
    def marginal(vs:Variable*): DiscreteMarginal = null
  }

  // Inference
  def inferBySumProduct(labels:IndexedSeq[Label]): ChainSummary = {
    val factors = this.factorsWithContext(labels)
    val summary = new ChainSummary
    summary
  }
  override def infer(variables:Iterable[Variable], model:Model, summary:Summary[Marginal] = null): Option[Summary[Marginal]] = {
    None
  }

  // Training
  val objective = new HammingTemplate[Label]
}