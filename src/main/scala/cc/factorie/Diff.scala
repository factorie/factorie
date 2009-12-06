/* Copyright (C) 2008-2009 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0.
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie

import scala.collection.mutable.{ArrayBuffer}
import scala.reflect.Manifest
//import scala.util.Random
import scala.Math
import scala.util.Sorting
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}
//import cc.factorie.util.{Log, ConsoleLogging, LinkedHashSet}
//import cc.factorie.util.Implicits._

// Diffs

/** A change record for a variable, holding its old and new values, with capability to undo and redo the change.
    @author Andrew McCallum
 */
trait Diff {
  def variable: Variable
  // TODO Make "def redo1" which calls redo, but verfies that we don't do redo twice in a row ??
  def redo: Unit
  def undo: Unit
}
// TODO consider adding state that throws error if users try to "undo" or "redo" twice in a row.

/** A Diff which, when created, performs the change.  
    Thus we avoid duplication of code: rather than doing the change, and then creating a Diff instance, we just create the Diff instance. 
    @author Andrew McCallum
 */
abstract class AutoDiff(implicit d:DiffList) extends Diff {
  if (d != null) d += this
  redo
  //override def toString = this.getClass.toString
}

 
/** A collection of changes to variables; the common representation for the result of a proposed change in configuration.
    Tracks whether the change is in its done or undone state and throws an error if repeated undo or redo is attempted.
    A DiffList can be scored according to a model (or two models) with scoreAndUndo methods.
    @author Andrew McCallum
 */
class DiffList extends ArrayBuffer[Diff] {
  var done = true
  def redo: Unit = {
    if (size == 0) return
    if (done) throw new Error("DiffList already done")
    this.foreach(d => d.redo)
    done = true
  }
  def undo: Unit = {
    if (size == 0) return
    if (!done) throw new Error("DiffList already undone")
    this.reverse.foreach(d => d.undo)
    done = false
  }
  def score(model:Model) = model.score(this) // TODO Should we provide this kind of syntax reversal, or only provide "one" way to do things?
  def scoreAndUndo(model:Model): Double = {
    if (this.length == 0) return 0.0  // short-cut the simple case
    var s = model.score(this)
    //println("Score: " + s)
    //log(Log.DEBUG)("DiffList scoreAndUndo  pre-undo score=" + s)
    this.undo
    // We need to re-calculate the Factors list because the structure may have changed
    s -= model.score(this)
    //log(Log.DEBUG)("DiffList scoreAndUndo post-undo score=" + s)
    s
  }
  /** For comparing the scores of two different models. */
  def scoreAndUndo(model1:Model, model2:Model) : (Double, Double) = {
    if (this.length == 0) return (0.0, if (model2 == null) Math.NaN_DOUBLE else 0.0) // short-cut the simple case
    var s1 = model1.score(this)
    var s2 = if (model2 == null) Math.NaN_DOUBLE else model2.score(this)
    this.undo
    s1 -= model1.score(this)
    if (model2 != null) s2 -= model2.score(this)
    (s1, s2)
  }
  /** More efficient than model.factorsOf[T](difflist) when the difflist might be empty. */
  def factorsOf[T<:Template](model:Model)(implicit m:Manifest[T]) : Seq[T#Factor] = if (this.isEmpty) Nil else model.factorsOf[T](this)(m) 
  /** Appropriate printed name. */
  override protected def stringPrefix = "DiffList" 
}
