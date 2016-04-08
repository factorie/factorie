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

package cc.factorie.model

import cc.factorie.la._
import cc.factorie.variable.{Assignment, Var, Vars}

/** A single factor in a factor graph.  From a Factor you can get its neighboring variables,
    the factor's score using the neighboring variable's current values,
    the factor's score using some Assignment to the the neighboring variable's values,
    sufficient statistics, 
    
    @author Andrew McCallum */
trait Factor extends Ordered[Factor] {
  type StatisticsType <: Any
  
  // Getting the neighboring variables
  
  /** Returns the collection of variables neighboring this factor. */
  def variables: Seq[Var]
  /** The number of variables neighboring this factor. */
  def numVariables: Int
  /** The Nth neighboring variable of this factor. */
  def variable(index: Int): Var
  /** Does this Factor have the given variable among its neighbors? */
  def touches(variable:Var): Boolean = this.variables.contains(variable)
  /** Does this Factor have any of the given variables among its neighbors? */
  def touchesAny(variables:Iterable[Var]): Boolean = variables.exists(touches(_))
  
  // Getting the score
  
  /** This factor's contribution to the unnormalized log-probability of the current possible world. */
  def currentScore: Double
  /** This factor's score, using the neighbors' values from the given assignment, not necessarily their current values.. */
  def assignmentScore(a:Assignment): Double
  /** Return the score for Factors whose values can be represented as a Tensor, otherwise throw an Error.
      For Factors/Family in which the Statistics are the values, this method simply calls statisticsScore(Tensor). */
  def valuesScore(tensor:Tensor): Double = throw new Error(s"This Factor class ${this.getClass.getName} does not implement valuesScore(Tensor).")
  // Do not declare statisticsScore(tensor:Tensor) here, because it should be abstract in TensorFactor2, etc.

  // Getting the statistics
  
  /** Return this Factor's sufficient statistics of the current values of the Factor's neighbors. */
  def currentStatistics: StatisticsType = throw new Error("This Factor class does not implement statistics.") // currentAssignment.asInstanceOf[StatisticsType] // A dummy default for statistics
  /** Return this Factor's sufficient statistics for the values in the Assignment. */
  def assignmentStatistics(a:Assignment): StatisticsType = throw new Error("This Factor class does not implement statistics.") // currentAssignment.asInstanceOf[StatisticsType] // A dummy default for statistics
  /** Given a Tensor representation of the values, return a Tensor representation of the statistics.  We assume that if the values have Tensor representation that the StatisticsType does also.
      Note that (e.g. in BP) the Tensor may represent not just a single value for each neighbor, but a distribution over values */
  def valuesStatistics(tensor:Tensor): Tensor = throw new Error("This Factor class does not implement valuesStatistics(Tensor).")
  /** True iff the statistics are the values (without transformation), e.g. valuesStatistics simply returns its argument. */
  def statisticsAreValues: Boolean = false
  
  /** Return the score and statistics of the current neighbor values; this method enables special cases in which it is more efficient to calculate them together. */
  def currentScoreAndStatistics: (Double,StatisticsType) = (currentScore, currentStatistics)
  def assignmentScoreAndStatistics(a:Assignment): (Double,StatisticsType) = (assignmentScore(a), assignmentStatistics(a))
  def valuesScoreAndStatistics(t:Tensor): (Double,Tensor) = (valuesScore(t), valuesStatistics(t))

  // Getting Assignments

  /** Return a record of the current values of this Factor's neighbors. */
  def currentAssignment: Assignment

  /** Return an object that can iterate over all value assignments to the neighbors of this Factor */
  //def valuesTensorIterator??
  //def assignmentIterator??

  // Implement Ordered, such that worst (lowest) scores are considered "high"
  def compare(that: Factor) = {val d = that.currentScore - this.currentScore; if (d > 0.0) 1 else if (d < 0.0) -1 else 0}
  /** In order to two Factors to satisfy "equals", the value returned by this method for each Factor must by "eq".
      This method is overridden in Family to deal with Factors that are inner classes. */
  def equalityPrerequisite: AnyRef = this.getClass
  // Implement equality based on class assignability and Variable contents equality
  //override def canEqual(other: Any) = (null != other) && other.isInstanceOf[Factor]; // TODO Consider putting this back in
  override def equals(other: Any): Boolean = other match {
    case other:Factor =>
      (this eq other) || ((this.equalityPrerequisite eq other.equalityPrerequisite)
                          && (this.hashCode == other.hashCode)
                          && (0 until numVariables).forall(i =>
                            (this.variable(i) eq other.variable(i)) || // TODO Consider getting rid of this, and just using == all the time.
                            (this.variable(i).isInstanceOf[Vars[_]] && this.variable(i) == other.variable(i))))
                            // TODO with the == above, some Vars classes should implement equals based on sameContents
    case _ => false
  }
  var _hashCode = -1
  /** A hashCode matching the criteria in equals, using hashCode of the Factor's class and of the Factor's neighboring variables. */
  override def hashCode: Int = {
    if (_hashCode == -1) {
      _hashCode = getClass.hashCode
      var i = 0
      while (i < numVariables) {
        val v = variable(i)
        _hashCode += 31*i + (if (v eq null) 0 else v.hashCode)
        i += 1
      }
    }
    _hashCode
  }
  def factorName = "Factor"
  override def toString: String = variables.mkString(factorName+"(", ",", ")")
}

