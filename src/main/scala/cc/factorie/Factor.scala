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

package cc.factorie

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer, FlatHashTable}
import scala.util.{Random,Sorting}
import scala.util.Random
import scala.math
import scala.util.Sorting
import cc.factorie.la._
import cc.factorie.util.Substitutions
import java.io._

/** A single factor in a factor graph.  In other words, a factor
    template packaged with a set of variables neighboring the
    factor.
    @author Andrew McCallum */
trait Factor extends Model with Ordered[Factor] {
  /** In some cases a factor "belongs" to some outer factor which uses this inner one as part of its score calculation.
      In this case this inner factor should not also be used for score generation because it would be redundant.
      For example, see method Template{1,2,3,4}.factors() */
  // TODO Change these back to def's
  //var outer: Factor
  //var inner: Seq[Factor]
  def outer: Factor = null
  def outer_=(f:Factor): Unit = throw new Error("Re-assigning Factor.outer not supported.")
  def inner: Seq[Factor] = Nil
  def inner_=(f:Seq[Factor]): Unit = throw new Error("Re-assigning Factor.inner not supported.")
  /** The number of variables neighboring this factor. */
  def numVariables: Int
  def variable(index: Int): Variable
  def statistics: Statistics
  /** Optionally return pre-calculated Statistics.  By default not actually cached, but may be overridden in subclasses. */
  def cachedStatistics: Statistics = statistics
  // The next two methods implement the Model trait
  def touches(variable:Variable): Boolean = this.variables.contains(variable) || inner.exists(_.touches(variable))
  def factors(variables:Iterable[Variable]): Seq[Factor] = if (variables.exists(touches(_))) Seq(this) else Nil
  /** This factors contribution to the unnormalized log-probability of the current possible world. */
  override def score: Double = statistics.score
  /** Returns the collection of variables neighboring this factor. */
  def variables: Seq[Variable] // = { val result = new ArrayBuffer[Variable](numVariables); for (i <- 0 until numVariables) result += variable(i); result }
  def values: Values                   
  /** Randomly selects and returns one of this factor's neighbors. */
  @deprecated def randomVariable(implicit random:Random = cc.factorie.random): Variable = variable(random.nextInt(numVariables))
  /** Return a copy of this factor with some neighbors potentially substituted according to the mapping in the argument. */
  def copy(s:Substitutions): Factor
  // Implement Ordered, such that worst (lowest) scores are considered "high"
  def compare(that: Factor) = {val d = that.score - this.score; if (d > 0.0) 1 else if (d < 0.0) -1 else 0}
  /** In order to two Factors to satisfy "equals", the value returned by this method for each Factor must by "eq" . */
  def equalityPrerequisite: AnyRef = this.getClass
  // Implement equality based on class assignability and Variable contents equality
  //override def canEqual(other: Any) = (null != other) && other.isInstanceOf[Factor]; // TODO Consider putting this back in
  override def equals(other: Any): Boolean = other match {
    case other:Factor =>
      (this eq other) || ((this.equalityPrerequisite eq other.equalityPrerequisite)
                          && forallIndex(numVariables)(i =>
                            (this.variable(i) eq other.variable(i)) ||
                            (this.variable(i).isInstanceOf[Vars[_]] && this.variable(i) == other.variable(i))))
    case _ => false
  }
  var _hashCode = -1
  override def hashCode: Int = {
    if (_hashCode == -1) {
      _hashCode = getClass.hashCode
      forIndex(numVariables)(i => { val v = variable(i); _hashCode += 31*i + (if (v eq null) 0 else v.hashCode) })
    }
    _hashCode
  }
  def factorName = "Factor"
  override def toString: String = variables.mkString(factorName+"(", ",", ")")
}

/** A Factor is a Model because it can return a factor (itself) and a score.
    A Model is not a Factor because Factors *must* be able to list all the variables they touch;
     (this is part of how they are de-duplicated);
     yet a model may only generate Factors on the fly in response to query variables.
    Factors are deduplicated.  Models are not; 
     multiple models may each contribute Factors, all of which define the factor graph.  
    Models can have inner Models, which are used to obtain factors from each.
     Typically all factors from all inner models are summed together.
     But a "Case" model may exist, which simply will not return factors that are not in effect;
     that is, factors from an inner model that are not in effect will never be seen; 
     whereas inner factors may be seen but know to point to the outer factor that knows how to handle them.  
    Factors can have inner factors, which are used to calculate its score; often not summed.
     This is a special case of a Model having inner Models.
    When you ask an inner Factor for its score, it returns the score of the outer Factor to which it contributes.
     (Is this right?  Perhaps not.)
    When you ask an inner Model for its score, it returns its score alone.
    */




/** A container for all the values of the variables neighboring a factor.
    These are necessary to construct a Statistics object. */
trait Values /* extends Product with Ordered[Values] */ {
  // def factor: Factor // TODO Consider adding this method
  def outer: Values = null
  def inner: Seq[Values] = Nil
  def statistics: Statistics
  def score: Double = statistics.score
  //def productArity: Int
  //def canEqual(other:Any) = other match { case other:Values => }
}

/** A container for sufficient statistics of a Factor.  
    There is one of these for each Factor. */
trait Statistics {
  // def factor: Factor // TODO Consider adding this method
  def outer: Statistics = null
  def inner: Seq[Statistics] = Nil
  def score: Double
}

