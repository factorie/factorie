/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
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

/** For storing one of the proposals considered.
    "diff" is the list of changes represented by this Proposal.
    "modelScore" is the difference in score after-before, according to the model.
    "objectiveScore" is the difference in score after-before, according to the objective function.
    "acceptanceScore" is the score that will be used to accept/rank/select among multiple proposals.  It is typically the modelScore multiplied by a temperature.
    Note that objectiveScore may not be truly set, in which case it will have value Math.NaN_DOUBLE. */
case class Proposal(diff:DiffList, modelScore:Double, objectiveScore:Double,  acceptanceScore:Double)


// TODO The following trait is currently unused.  Remove it?
/** An object (typically a variable or a world) that can propose changes to itself, 
    and possibly also other variables through variable value coordination. */
trait Proposer {
  /** Make a random proposal.  Return Metropolis-Hastings' log(q(old|new)/q(new|old)) */
  def propose(model:Model, d:DiffList): Double
}
