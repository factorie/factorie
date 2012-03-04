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
import cc.factorie.generative._


// TODO consider making this inherit from Infer
trait Maximize {
  /** Returns true on success, false if this recipe was unable to handle the relevant factors. */
  def apply(variables:Seq[Variable], varying:Seq[Variable], factors:Seq[Factor], qModel:Model): Boolean // Abstract implemented in subclasses
  def apply(variables:Seq[Variable], factors:Seq[Factor], qModel:Model): Boolean = apply(variables, Nil, factors, qModel)
  def apply(variables:Seq[Variable], factors:Seq[Factor]): Boolean = apply(variables, factors, null)
  def apply(variables:Seq[Variable], model:Model): Boolean = apply(variables, model.factors(variables))
  def apply(variables:Seq[Variable], model:Model, qModel:Model): Boolean = apply(variables, model.factors(variables), qModel)
}

/* Contains various recipes to maximize the value of variables to maximize some objective, 
   usually maximum likelihood. 
   @author Andrew McCallum */
class MaximizeVariables extends Maximize {
  def defaultSuite = Seq(MaximizeDiscrete, MaximizeGate, MaximizeProportions)
  val suite = new scala.collection.mutable.ArrayBuffer[Maximize] ++ defaultSuite
  def apply(variables:Seq[Variable], varying:Seq[Variable], factors:Seq[Factor], qModel:Model): Boolean = {
    // The handlers can be assured that the Seq[Factor] will be sorted alphabetically by class name
    // This next line does the maximization
    val option = suite.find(_.apply(variables, varying, factors, qModel))
    if (option == None) throw new Error("No maximizer found for factors "+factors.take(10).map(_ match { case f:Family#Factor => f.family.getClass; case f:Factor => f.getClass }).mkString)
    false
  }
}
object Maximize extends MaximizeVariables // A default instance of this class


