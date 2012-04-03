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
import cc.factorie.la._
import collection.mutable.HashMap

/** For parameter estimation methods that use a gradient to update weight parameters.
    Subclasses will create and store a gradient.  
    Calls to addGradient should increment this gradient.
    Calls to updateWeights should use this gradient to change the weights. 
    @author Andrew McCallum */
trait WeightUpdates {
  type UpdateFamilyType = DotFamily
  //def templateClassToUpdate: Class[TemplatesToUpdate]
  /** The model's list of factor families whose weights will be updated. */
  def familiesToUpdate: Seq[DotFamily]
  /** The number of times 'updateWeights' has been called. */
  var updateCount : Int = 0
  /** Call this method to use the current gradient to change the weight parameters.  
      When you override it, you should call super.updateWeights, which will increment updateCount. */
  def updateWeights : Unit = updateCount += 1
  /** Adds the current gradient (as calculated by the recipient) to the accumulator, scaled by 'rate'. */
  def addGradient(accumulator:UpdateFamilyType=>Vector, rate:Double): Unit

  def newGradientAccumulator: HashMap[DotFamily, Vector] = {
    new HashMap[DotFamily,Vector] {
      override def default(template:DotFamily) = {
        template.freezeDomains
        val vector = template.newWeightsTypeVector(0.0)
        this(template) = vector
        vector
      }
    }
  }
}
