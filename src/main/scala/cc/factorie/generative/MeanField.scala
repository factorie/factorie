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
import scala.collection.mutable.HashMap

/** A model with independent generative factors for a collection of variables. */
// TODO Consider making this a Lattice since it holds some "marginals"
class MeanField(variables:Iterable[Variable]) extends Model {
  private val _factor = new HashMap[Variable,Factor]
  def init(variables:Iterable[Variable]): Unit = {
    for (v <- variables) v match {
      case d:DiscreteVar => _factor(v) = new Discrete.Factor(d, new DenseProportions(d.domain.size))
      case r:RealVar => _factor(v) = new Gaussian.Factor(r, new RealVariable, new RealVariable)
    }
  }
  init(variables)
  def factors(variables:Iterable[Variable]): Seq[Factor] = variables.flatMap(v => _factor.get(v)).toSeq
  def factor(v:Variable): Factor = _factor(v)
  /*override def marginal(v:Variable): Option[Variable] = _factor(v) match {
    case null => None
    case f:Discrete.Factor => Some(f._2)
    // TODO Is this how it should work?  Missing type information.  How to implement generically?  Should we actually return the factor instead?
  }*/
}

/**  */
class MeanFieldInferencer(val variables:Iterable[Variable], val model:Model, val qModel:Model) {
  def this(vs:Iterable[Variable], model:Model) = this(vs, model, new MeanField(vs))
  def updateQ(v:Variable): Unit = {
    val seqv = Seq(v)
    val qFactors = qModel.factors(seqv)
    if (qFactors.size > 1) throw new Error("Multiple factors touching a variable in qModel; structured mean field not yet implemented.")
    qFactors.head match {
      case f:Discrete.Factor => {
        val d = f._1.asInstanceOf[DiscreteVariable]
        val p = f._2.asInstanceOf[DenseProportions]
        val distribution = new Array[Double](p.size)
        for (i <- 0 until p.size) {
          val diff = new DiffList
          d.set(i)(diff)
          val factors = model.factors(diff)
          // Inefficient to have this in inner loop; but what is the alternative?
          if (factors.flatMap(_.variables).exists(v => qModel.factors(seqv) != Nil)) throw new Error("Not yet implemented neighboring mean fields")
          distribution(i) = diff.scoreAndUndo(model)
        }
        maths.expNormalize(distribution)
        p.set(distribution)(null)
      }
      case f:Factor => throw new Error("MeanFieldInferencer does not know how to handle factors of type "+f.getClass)
    }
  }
  def updateQ: Unit = variables.foreach(updateQ(_))
}