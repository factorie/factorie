/* Copyright (C) 2008-2009 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0.
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer, FlatHashTable}
import scala.reflect.Manifest
import scala.util.Random
import scala.Math
import scala.util.Sorting
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}
import cc.factorie.util.{Log, ConsoleLogging, LinkedHashSet}
import cc.factorie.util.Implicits._

// Proposals

/** For storing one of the proposals considered.  Note that objectiveScore may not be truly set, in which case it will have value Math.NaN_DOUBLE. */
// TODO Should replace Proposal in Proposal.scala
// TODO rename acceptanceScore to samplingWeight? (more general)
case class Proposal(diff:DiffList, modelScore:Double, objectiveScore:Double,  acceptanceScore:Double)


/**An object (typically a variable or a world) that can propose changes to itself, and possibly also other variables through variable value coordination */
trait Proposer {
  /** Make a random proposal.  Return Metropolis-Hastings' log(q(old|new)/q(new|old)) */
  def propose(model:Model, d:DiffList): Double
}
