/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

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
  def getOptimizableParameters(a:Array[Double] = null): Array[Double] = getVectorsInArray(a)
  def setOptimizableParameters(a:Array[Double]): Unit = setVectorsFromArray(a)
  def optimizableParameter(index:Int): Double = vectorValueAtArrayIndex(index)
  def optimizableParameter_=(index:Int, d:Double): Unit = vectorValueAtArrayIndex_=(index, d)
}

