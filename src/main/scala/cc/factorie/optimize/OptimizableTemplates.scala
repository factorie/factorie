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
import cc.factorie.la.ArrayFromVectors

/** Implements Optimizable trait's methods for parameter access in terms of DotTemplate weight parameters.
    Uses the functionality of ArrayFromVectors to pack Vectors into a single Array[Double], and correspondingly unpack them.
    Note that the order in sequence of templates (and thus their vectors) matters tremendously.  The templates must
    be provided in the same order in order to make the resulting arrays compatible with each other.
    @see ArrayFromVectors 
    @author Andrew McCallum */
class OptimizableTemplates(theTemplates:Seq[DotTemplate]) extends ArrayFromVectors(theTemplates.map(_.weights)) with Optimizable {
  def numOptimizableParameters = vectorsArraySize
  def getOptimizableParameters(a:Array[Double]) = getVectorsInArray(a)
  def setOptimizableParameters(a:Array[Double]): Unit = setVectorsFromArray(a)
  def optimizableParameter(index:Int): Double = vectorValueAtArrayIndex(index)
  def optimizableParameter_=(index:Int, d:Double): Unit = vectorValueAtArrayIndex_=(index, d)
}

