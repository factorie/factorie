/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.util

/** Set implemented as a List. 
    @author Sameer Singh
*/
class LinkedHashSet[A] extends scala.collection.mutable.Set[A] with scala.collection.mutable.FlatHashTable[A] {
  var list = List[A]()
  override def initialSize: Int = 32
  def contains(elem: A): Boolean = containsEntry(elem)
  def +=(elem: A) {add(elem)}
  def add(elem: A): Boolean = {
      if (addEntry(elem)) {
        list = elem :: list
        true
      } else false
  }
  def -=(elem: A) {remove(elem)}
  def remove(elem: A): Boolean = removeEntry(elem) match {
  case None => false
  case Some(elem) => list = list.filter(_ != elem); true
  }
  override def clear() {list = Nil; super.clear()}
  override def iterator = list.iterator
}

