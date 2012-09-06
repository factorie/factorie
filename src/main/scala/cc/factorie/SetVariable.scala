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
import scala.collection.mutable.HashSet

trait SetVar[A] extends Variable with VarAndValueGenericDomain[SetVar[A],scala.collection.Set[A]] {
  def value: scala.collection.Set[A]
  def iterator: Iterator[A] = value.iterator
  def foreach[U](f:A=>U): Unit = iterator.foreach(f)
  def map[B](f:A=>B): scala.collection.Set[B] = new HashSet[B] ++= iterator.map(f)
  def forall(f:A=>Boolean): Boolean = iterator.forall(f)
  def exists(f:A=>Boolean): Boolean = iterator.exists(f)
  def size: Int
}

class EmptySetVar[A] extends SetVar[A] {
  override def iterator = Iterator.empty
  def size = 0
  def value = Set.empty[A]
}

/**A variable whose value is a set of other variables */
class SetVariable[A]() extends SetVar[A] with VarAndValueGenericDomain[SetVariable[A],scala.collection.Set[A]] {
  type Value = scala.collection.Set[A]
  // Note that the returned value is not immutable.
  def value = _members
  private val _members = new HashSet[A];
  def members: scala.collection.Set[A] = _members
  override def iterator = _members.iterator
  def size = _members.size
  def contains(x:A) = _members.contains(x)
  def add(x:A)(implicit d: DiffList): Unit = if (!_members.contains(x)) {
    if (d != null) d += new SetVariableAddDiff(x)
    _members += x
  }
  def remove(x: A)(implicit d: DiffList): Unit = if (_members.contains(x)) {
    if (d != null) d += new SetVariableRemoveDiff(x)
    _members -= x
  }
  final def +=(x:A): Unit = add(x)(null)
  final def -=(x:A): Unit = remove(x)(null)
  final def ++=(xs:Iterable[A]): Unit = xs.foreach(add(_)(null))
  final def --=(xs:Iterable[A]): Unit = xs.foreach(remove(_)(null))
  case class SetVariableAddDiff(added: A) extends Diff {
    // Console.println ("new SetVariableAddDiff added="+added)
    def variable: SetVariable[A] = SetVariable.this
    def redo = _members += added //if (_members.contains(added)) throw new Error else
    def undo = _members -= added
    override def toString = "SetVariableAddDiff of " + added + " to " + SetVariable.this
  }
  case class SetVariableRemoveDiff(removed: A) extends Diff {
    //        Console.println ("new SetVariableRemoveDiff removed="+removed)
    def variable: SetVariable[A] = SetVariable.this
    def redo = _members -= removed
    def undo = _members += removed //if (_members.contains(removed)) throw new Error else
    override def toString = "SetVariableRemoveDiff of " + removed + " from " + SetVariable.this
  }
}

class WeakSetVariable[A<:{def present:Boolean}] extends Variable with VarAndValueGenericDomain[WeakSetVariable[A],scala.collection.Set[A]] {
  private val _members = new cc.factorie.util.WeakHashSet[A];
  def value: scala.collection.Set[A] = _members
  def iterator = _members.iterator.filter(_.present)
  //def size = _members.size
  def contains(x: A) = _members.contains(x) && x.present
  def add(x: A)(implicit d: DiffList): Unit = if (!_members.contains(x)) {
    if (d != null) d += new WeakSetVariableAddDiff(x)
    _members += x
  }
  def remove(x: A)(implicit d: DiffList): Unit = if (_members.contains(x)) {
    if (d != null) d += new WeakSetVariableRemoveDiff(x)
    _members -= x
  }
  case class WeakSetVariableAddDiff(added: A) extends Diff {
    // Console.println ("new WeakSetVariableAddDiff added="+added)
    def variable: WeakSetVariable[A] = WeakSetVariable.this
    def redo = _members += added //if (_members.contains(added)) throw new Error else
    def undo = _members -= added
  }
  case class WeakSetVariableRemoveDiff(removed: A) extends Diff {
    //        Console.println ("new WeakSetVariableRemoveDiff removed="+removed)
    def variable: WeakSetVariable[A] = WeakSetVariable.this
    def redo = _members -= removed
    def undo = _members += removed //if (_members.contains(removed)) throw new Error else
    override def toString = "WeakSetVariableRemoveDiff of " + removed + " from " + WeakSetVariable.this
  }
}
