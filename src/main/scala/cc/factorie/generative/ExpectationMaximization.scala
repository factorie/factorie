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

package cc.factorie.generative
import cc.factorie._

class EMInferencer[M<:Parameter with Estimation[M]] extends Inferencer[M,MixtureChoiceVariable] {
  type LatticeType = EMLattice[M]
  def infer(variables:Iterable[M], varying:Iterable[MixtureChoiceVariable]): LatticeType = {
    val em = new EMLattice[M](varying, variables)
    em.process()
    em
  }
}

/** Currently uses IID estimation for m-step inference, 
    but in the future the selection of inference method may be more configurable. 
    @author Andrew McCallum */
class EMLattice[M<:Parameter with Estimation[M]]
(eVariables:Iterable[MixtureChoiceVariable], mVariables:Iterable[M],
 eInferencer: VariableInferencer[MixtureChoiceVariable] = new IIDDiscreteInferencer[MixtureChoiceVariable](cc.factorie.generative.defaultGenerativeModel))
extends Lattice[M]
{
  var eLattice: Lattice[MixtureChoiceVariable] = null
  def eStep: Unit = eLattice = eInferencer.infer(eVariables)
  def mStep: Unit = {
    /** Map access to variable marginals. */
    val latticeMapping: scala.collection.Map[Variable,Variable] = new scala.collection.DefaultMap[Variable,Variable] {
      def get(v:Variable): Option[Variable] = v match {
        case v:MixtureChoiceVariable => eLattice.marginal(v) //.asInstanceOf[Variable]
        case _ => None
      }
      def iterator: Iterator[(Variable,Variable)] = throw new Error
    }
    mVariables.foreach(v => v.estimate(v.defaultEstimator, latticeMapping))
  }
  /** Return true iff converged. */
  // TODO Implement a proper convergence criterion
  def process(iterations:Int = 10): Boolean = {
    forIndex(iterations)(i => {
      eStep
      mStep
    })
    false // TODO Determine convergence criterion
  }
}
