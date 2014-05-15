/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
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
package cc.factorie.app.nlp.hcoref

import cc.factorie.variable.{SettingIterator, DiffList}
import cc.factorie.infer.SettingsSampler

/**
 * User: escher, John Sullivan, akobren
 * Date: 10/28/13
 *
 */
trait MoveSettingIterator[Vars <: NodeVariables[Vars]] extends SettingIterator{
  def moves:IndexedSeq[Move[Vars]]

  var i = 0

  def hasNext = i < moves.size
  def next(diff:DiffList) = {val d = newDiffList; moves(i).perform(d); i += 1; d}
  def reset = i = 0
}


trait MoveGenerator[Vars <: NodeVariables[Vars]]  {

  this: SettingsSampler[(Node[Vars], Node[Vars])] =>

  def newInstance(implicit d:DiffList):Node[Vars]
}

trait Move[Vars <: NodeVariables[Vars]] {

  def name: String

  def perform(d:DiffList):Unit

  def isSymmetric(node1:Node[Vars], node2:Node[Vars]): Boolean // is the move symmetric for this pair of nodes?

  def isValid(node1: Node[Vars], node2:Node[Vars]): Boolean
  def operation(node1: Node[Vars], node2:Node[Vars])(d:DiffList): DiffList
  final def apply(node1:Node[Vars], node2:Node[Vars])(d:DiffList):DiffList = Option(d) match {
    case Some(diff) => operation(node1, node2)(diff)
    case None => operation(node1, node2)(new DiffList)
  }
}

class NoMove[Vars <: NodeVariables[Vars]] extends Move[Vars] {
  def name = "No Move"

  def perform(d:DiffList) = Unit

  def isSymmetric(node1: Node[Vars], node2: Node[Vars]): Boolean = true

  def isValid(node1: Node[Vars], node2: Node[Vars]): Boolean = true
  def operation(node1: Node[Vars], node2: Node[Vars])(d:DiffList) = {
    d
  }
}

class MergeLeft[Vars <: NodeVariables[Vars]](val left:Node[Vars], val right:Node[Vars]) extends Move[Vars] {

  def this() = this(null.asInstanceOf[Node[Vars]], null.asInstanceOf[Node[Vars]])

  def perform(d:DiffList) {
    operation(right, left)(d)
  }

  def name = "Merge Left"
  def isValid(right: Node[Vars], left: Node[Vars]) = right.root != left.root && !left.isMention && left.mentionCountVar.value >= right.mentionCountVar.value
  def isSymmetric(node1: Node[Vars], node2: Node[Vars]): Boolean = false

  def operation(right: Node[Vars], left: Node[Vars])(d:DiffList) = {
    right.alterParent(Option(left))(d)
    d
  }
}

class SplitRight[Vars <: NodeVariables[Vars]](val left:Node[Vars], val right:Node[Vars]) extends Move[Vars] {

  def this() = this(null.asInstanceOf[Node[Vars]], null.asInstanceOf[Node[Vars]])

  def perform(d:DiffList) {
    operation(right, left)(d)
  }

  def name = "Split Right"
  def isValid(right: Node[Vars], left: Node[Vars]): Boolean = left.root == right.root && right.mentionCountVar.value >= left.mentionCountVar.value
  def isSymmetric(node1: Node[Vars], node2: Node[Vars]): Boolean = false

  def operation(right:Node[Vars], left: Node[Vars])(d:DiffList) = {
    right.alterParent(None)(d)
    d
  }
}

class MergeUp[Vars <: NodeVariables[Vars]](val left:Node[Vars], val right:Node[Vars])(newInstance:(DiffList => Node[Vars])) extends Move[Vars] {

  def this(newInstance:(DiffList => Node[Vars])) = this(null.asInstanceOf[Node[Vars]], null.asInstanceOf[Node[Vars]])(newInstance)

  def perform(d:DiffList) {
    operation(right, left)(d)
  }

  def name = "Merge Up"
  def isValid(right: Node[Vars], left: Node[Vars]): Boolean = left.root != right.root && (left.isRoot && right.isRoot) && (left.isMention && right.isMention)
  def isSymmetric(node1: Node[Vars], node2: Node[Vars]): Boolean = true

  def operation(right: Node[Vars], left: Node[Vars])(d:DiffList) = {
    val newParent = newInstance(d)
    right.alterParent(Some(newParent))(d)
    left.alterParent(Some(newParent))(d)
    d
  }
}
