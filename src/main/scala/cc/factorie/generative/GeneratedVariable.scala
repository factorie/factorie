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


// A collection of abstract Variables (and a generic Family) for generative models (directed Bayesian networks, 
// as opposed to undirected in which there is not a DAG-shaped generative storyline).


// TODO Are these still necessary?  Consider deleting.  Yes!
/*
trait RealGenerating {
  def sampleDouble: Double
  def pr(x:Double): Double
  def logpr(x:Double): Double
}
trait DiscreteGenerating {
  def length: Int
  def sampleInt: Int
  def pr(index:Int): Double
  def logpr(index:Int): Double
}
trait ProportionGenerating {
  def sampleProportions: Proportions
  def pr(p:Proportions): Double
  def logpr(p:Proportions): Double
}
*/
