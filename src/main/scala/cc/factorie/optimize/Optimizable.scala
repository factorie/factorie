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



package cc.factorie.optimize
import cc.factorie._
import scala.collection.mutable.IndexedSeq

/** Functions that can be maximized by numeric optimization must provide these
    methods for getting and setting their parameters.
    @see Optimizer
    @author Andrew McCallum */
trait Optimizable {
  def numOptimizableParameters: Int
  /** If argument is null, an array will be allocated for you and returned. */
  def getOptimizableParameters(a:Array[Double]) : Unit
  def setOptimizableParameters(a:Array[Double]) : Unit
  def optimizableParameter(index:Int): Double
  def optimizableParameter_=(index:Int, d:Double): Unit
}

/** An Optimizable object that can provide the value of the function being optimized. 
    @author Andrew McCallum */
trait OptimizableByValue extends Optimizable {
  def optimizableValue: Double
}

/** An Optimizable object that can provide the gradient of the function being optimized. 
    @author Andrew McCallum */
trait OptimizableByGradient extends Optimizable {
  /** If argument is null, an array will be allocated for you and returned. */
  def getOptimizableGradient(a:Array[Double])
}

/** An Optimizable object that can provide both the value and the gradient of the function being optimized. 
    @author Andrew McCallum */
trait OptimizableByValueAndGradient extends OptimizableByValue with OptimizableByGradient
