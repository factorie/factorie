/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie
import scala.reflect.Manifest
//import scalala.Scalala._
//import scala.reflect.Manifest
import cc.factorie.la._

/** Collins' structured-perceptron */
abstract class StructuredPerceptron[V<:Variable with TrueSetting](model:Model) extends GradientAscentUpdates {
  //type TemplatesToUpdate = DotTemplate
  var learningMargin = 1.0 // TODO not currently used
  private var difflist: DiffList = null
  
  // Abstract method to be provided elsewhere
  def predict(vs:Seq[V]): Unit
  
  def process(vs:Seq[V]): Unit = {
    predict(vs)
    difflist = new DiffList
    vs.foreach(_.setToTruth(difflist))
    difflist.undo // TODO Consider commenting this out, but consider carefully.  Dangerous for "addGradient" to have side-effects.
    if (difflist.size > 0)
      updateWeights // This will result in a call to "addGradient"
  }

  def addGradient(accumulator:TemplatesToUpdate=>Vector, rate:Double): Unit = {
    if (!difflist.done) difflist.redo
    difflist.factorsOf[TemplatesToUpdate](model).foreach(f => accumulator(f.template) += f.statistic.vector *  rate)
    difflist.undo
    difflist.factorsOf[TemplatesToUpdate](model).foreach(f => accumulator(f.template) += f.statistic.vector * -rate)
  }

}


// TODO What would a MaximumMargin trait look like?
