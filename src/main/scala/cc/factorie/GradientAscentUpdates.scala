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

//import scalala.Scalala._
//import scalala.tensor.dense.DenseVector
//import scalala.tensor.sparse.SparseVector
//import scalala.tensor.Vector
import scala.reflect.Manifest
import scala.collection.mutable.HashMap


trait GradientAscentUpdates extends WeightUpdates {
  type TemplatesToUpdate = DotTemplate
  def templateClassToUpdate = classOf[DotTemplate]
  var learningRate = 1.0
  // var learningRateDecay = 0.9 // TODO I'd like to find a way to decay the learning rate automatically without the user having to manage it.
  def model : Model
  //def learningMargin : Double
  override def updateWeights : Unit = {
    // Add the gradient directly to each relevant Template's weight vector, with factor 'learningRate'
    addGradient((template:Template) => template match {case t:TemplatesToUpdate => t.weights}, learningRate)
    // Call super to increment the updateCount
    super.updateWeights 
  }
}
