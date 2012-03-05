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


// TODO consider making this inherit from Infer, but the return type is different.
// TODO consider returning an Assignment instead of true/false.
trait Maximize[-V1<:Variable,-V2<:Variable] {
  /** Set the variables to values that maximize some criteria, usually maximum likelihood. */
  def apply(variables:Iterable[V1], varying:Iterable[V2], model:Model, qModel:Model): Unit // Abstract implemented in subclasses
  def apply(variables:Iterable[V1], varying:Iterable[V2], model:Model): Unit = apply(variables, varying, model, null)
  def apply(variables:Iterable[V1], model:Model, qModel:Model): Unit = apply(variables, Nil, model, qModel)
  def apply(variables:Iterable[V1], model:Model): Unit = apply(variables, Nil, model, null)
  /** Called by generic maximize engines that manages a suite of Infer objects, allowing each to attempt an maximize request.
      If you want your Maximize subclass to support such usage by a suite, override this method to check types as a appropriate
      and return a true on success, or false on failure. */
  def attempt(variables:Iterable[Variable], varying:Iterable[Variable], model:Model, qModel:Model): Boolean = false
}


/* A suite containing various recipes to maximize the value of variables to maximize some objective, 
   usually maximum likelihood. 
   @author Andrew McCallum */
class MaximizeVariables extends Maximize[Variable,Variable] {
  def defaultSuite = Seq(MaximizeGeneratedDiscrete, MaximizeGate, MaximizeProportions)
  val suite = new scala.collection.mutable.ArrayBuffer[Maximize[Variable,Variable]] ++ defaultSuite
  def apply(variables:Iterable[Variable], varying:Iterable[Variable], model:Model, qModel:Model): Unit = {
    // The handlers can be assured that the Seq[Factor] will be sorted alphabetically by class name
    // This next line does the maximization
    val option = suite.find(_.attempt(variables, varying, model, qModel))
    if (option == None) throw new Error("No maximizer found for factors "+model.factors(variables).take(10).map(_ match { case f:Family#Factor => f.family.getClass; case f:Factor => f.getClass }).mkString)
  }
}
object Maximize extends MaximizeVariables // A default instance of this class


