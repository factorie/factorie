/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie

import scalala.Scalala._
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.SparseVector
import scalala.tensor.Vector
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
