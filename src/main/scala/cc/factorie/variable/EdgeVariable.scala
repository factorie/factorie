/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie.variable


/** An abstract variable representing an edge between two nodes in a graph.
    The value of the variable is a Tuple2 of the source and destination nodes.
    @author Andrew McCallum */
trait EdgeVar[A,B] extends Var {
  type Value <: (A,B)
  def src: A
  def dst: B
}

/** A variable representing an edge between two nodes in a graph.  
    The value of the variable is a Tuple2 of the source and destination nodes.
    @author Andrew McCallum */
class EdgeVariable[A,B](initialSrc:A, initialDst:B) extends EdgeVar[A,B] with MutableVar {
  type Value = (A,B)
  private var _src = initialSrc
  private var _dst = initialDst
  def src = _src
  def dst = _dst
  def value = (_src, _dst)
  def set(newSrc:A, newDst:B)(implicit d:DiffList): Unit = {
    if (d ne null) d += EdgeDiff(_src, newSrc, _dst, newDst)
    _src = newSrc
    _dst = newDst
  }
  final def set(value:(A,B))(implicit d:DiffList): Unit = set(value._1, value._2)
  final def setSrc(newSrc:A)(implicit d:DiffList): Unit = set(newSrc, _dst)
  final def setDst(newDst:B)(implicit d:DiffList): Unit = set(_src, newDst)
  case class EdgeDiff(oldSrc:A, newSrc:A, oldDst:B, newDst:B) extends Diff {
    def variable = EdgeVariable.this
    def redo() = { _src = newSrc; _dst = newDst }
    def undo() = { _src = oldSrc; _dst = oldDst }
    override def toString = "EdgeDiff(%s,%s,%s,%s)".format(oldSrc.toString, newSrc.toString, oldDst.toString, newDst.toString)
  }
}

/** An EdgeVar in which the "src" is constant, and the "dst" is mutable.
    @author Andrew McCallum */
class ArrowVariable[A<:AnyRef,B](val src:A, initialDst:B) extends EdgeVar[A,B] {
  private var _dst = initialDst
  type Value = (A,B)
  def dst = _dst
  def value = (src, _dst)
  def set(newDst:B)(implicit d:DiffList): Unit = {
    if (d ne null) d += ArrowDiff(_dst, newDst)
    _dst = newDst
  }
  final def set(newSrc:A, newDst:B)(implicit d:DiffList): Unit = {
    require(newSrc eq src)
    set(newDst)
  }
  //final def set(value:(A,B))(implicit d:DiffList): Unit = set(value._1, value._2) // Was here to enable MutableVar, but clashes with set(newDst:B)
  case class ArrowDiff(oldDst:B, newDst:B) extends Diff {
    def variable = ArrowVariable.this
    def redo() = _dst = newDst
    def undo() = _dst = oldDst
    override def toString = "ArrowDiff(%s,%s)".format(oldDst, newDst)
  }
}

// TODO We should add an ArrowVariable in which the "src" is always "this"
// I was thinking about this for Mention

