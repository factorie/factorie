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

import scala.collection.mutable.ArrayBuffer
import cc.factorie.la._

/** A change record for a variable, holding its old and new values, with capability to undo and redo the change.
    @author Andrew McCallum */
trait Diff {
  def variable: Variable
  def redo: Unit
  def undo: Unit
}

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
  // TODO Should we provide this kind of syntax reversal, or only provide "one" way to do things?
  def score(model:Model) = model.score(this)
  def scoreAndUndo(model:Model): Double = {
    if (this.length == 0) return 0.0  // short-cut the simple case
    var s = model.score(this)
    //log(Log.DEBUG)("DiffList scoreAndUndo  pre-undo score=" + s)
    this.undo
    // We need to re-calculate the Factors list because the structure may have changed
    s -= model.score(this)
    //log(Log.DEBUG)("DiffList scoreAndUndo post-undo score=" + s)
    s
  }
  /** For comparing the scores of two different models. */
  def scoreAndUndo(model1:Model, model2:Model) : (Double, Double) = {
    if (this.length == 0) return (0.0, if (model2 == null) Double.NaN else 0.0) // short-cut the simple case
    var s1 = model1.score(this)
    var s2 = if (model2 == null) Double.NaN else model2.score(this)
    this.undo
    s1 -= model1.score(this)
    if (model2 != null) s2 -= model2.score(this)
    (s1, s2)
  }
  /** More efficient than model.factorsOf[T](difflist) when the difflist might be empty. */
  def factorsOf[T<:Template](model:Model)(implicit m:Manifest[T]) : Seq[T#Factor] = if (this.isEmpty) Nil else model.factorsOf[T](this)(m) 
  def factorsOf[T<:Template](cls:Class[T])(model:Model): Seq[T#Factor] = if (this.isEmpty) Nil else model.factorsOf(cls)(this)
  /** Appropriate printed name. */
  override def stringPrefix = "DiffList" 
}
