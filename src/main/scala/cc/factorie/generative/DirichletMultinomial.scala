/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.generative
import cc.factorie._

trait DirichletMultinomial extends Proportions with CollapsedParameter with GeneratedVar {
  def mean: Proportions
  def precision: RealVarParameter
  def parents = List(mean, precision)
  mean.addChild(this)(null)
  precision.addChild(this)(null)
  def increment(index:Int, incr:Double)(implicit d:DiffList): Unit
  def zero: Unit
  def counts(index:Int): Double
  def countsTotal: Double
  def pr = 1.0 // TODO implement.  Since this is collapsed, what should it be?  1.0?
  def detatch: Unit = { mean.removeChild(this)(null); precision.removeChild(this)(null) } // TODO Is this necessary?
  override def apply(index:Int) : Double = {
    val alphaSum = precision.doubleValue
    val result = (counts(index) + mean(index) * alphaSum) / (countsTotal + alphaSum)
    assert(result >= 0.0, "alphaSum="+alphaSum+" count="+counts(index)+" mean="+mean(index)+" countsTotal="+countsTotal)
    result
  }
  /*override def addChild(c:GeneratedVar)(implicit d:DiffList): Unit = {
    // xxx c match { case v:DiscreteVar => increment(v.intValue, 1.0); case _ => throw new Error } // xxx This seems to be the slowness culprit
    super.addChild(c)(d)
  }
  override def removeChild(c:GeneratedVar)(implicit d:DiffList): Unit = {
    //println("DirichletMultinomial.removeChild "+c)
    // xxx c match { case v:DiscreteVar => increment(v.intValue, -1.0); case _ => throw new Error } 
    super.removeChild(c)(d)
  }*/
  def clearChildStats: Unit = this.zero
  def updateChildStats(child:Variable, weight:Double): Unit = child match {
    case d:DiscreteVar => increment(d.intValue, weight)(null)
    case p:Proportions if (p.length == length) => forIndex(length)(i => increment(i, p(i) * weight)(null))
    case _ => {} // TODO Should we really not throw an error here?
  }
  // Perhaps DirichletMultinomial should not be a GeneratedVariable?  But it does have parents and children.
  def sample(implicit d:DiffList): Unit = throw new Error
  def sampleFrom(parents:Seq[Variable])(implicit d:DiffList): Unit = throw new Error
  def prFrom(parents:Seq[Parameter]): Double = throw new Error
}

class DenseDirichletMultinomial(val mean:Proportions, val precision:RealVarParameter) extends DenseCountsProportions(mean.length) with DirichletMultinomial {
  def this(size:Int, alpha:Double) = this(new UniformProportions(size), new RealVariableParameter(alpha*size))
  //def this(dirichlet:Dirichlet) = this(dirichlet.mean, dirichlet.precision)
}

class GrowableDenseDirichletMultinomial(val alpha:Double) extends GrowableDenseCountsProportions with DirichletMultinomial {
  lazy val mean = new GrowableUniformProportions(this)
  lazy val precision = new RealFunction {
    def doubleValue = alpha * GrowableDenseDirichletMultinomial.this.length
    def pr = 1.0
    def parents = List(GrowableDenseDirichletMultinomial.this) // TODO But note that GrowableDenseDirichletMultinomial doesn't have this as a child.
  }
}

