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

/** Masses is a Seq[Double] with all elements non-negative.
    Proportions ~ Dirichlet(Masses) */
// TODO Note, this is currently unused, since Dirichlet is instead parameterized by mean & precision
trait Masses extends Parameter with IndexedSeqEqualsEq[Double] {
  override def toString = this.mkString(printName+"(", ",", ")")
}

trait MutableMasses extends Masses {
  def set(p:Seq[Double])(implicit d:DiffList): Unit 
}

class DenseMasses(m:Seq[Double]) extends MutableMasses {
  def this(dim:Int) = this(Seq.fill(dim)(1.0))
  private var _m = new Array[Double](length)
  if (m != Nil) this := m else setUniform(null)
  @inline final def length = _m.size
  @inline final def apply(index:Int) = _m(index)
  def set(m:Seq[Double])(implicit d:DiffList): Unit = {
    assert(m.size == _m.size, "size mismatch: new="+m.size+", orig="+_m.size)
    val newM = m.toArray
    if (d ne null) d += MassesDiff(_m, newM)
    _m = newM
  }
  def :=(p:Seq[Double]) = set(p)(null)
  def setUniform(implicit d:DiffList): Unit = set(Seq.fill(_m.length)(1.0))
  case class MassesDiff(oldM:Array[Double], newM:Array[Double]) extends Diff {
    def variable = DenseMasses.this
    def undo = _m = oldM
    def redo = _m = newM
  }
}
