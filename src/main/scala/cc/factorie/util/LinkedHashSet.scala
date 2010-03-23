/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.util

/** Set implemented as a FlatHashTable, with a List for fast iteration. 
    @author Sameer Singh
*/
// TODO Consider putting this back.  It was originally created for efficiency of Factor managment in cc.factorie.Template, but I'm not sure it is necessary any more
/*
class LinkedHashSet[A] extends scala.collection.mutable.Set[A] with scala.collection.mutable.FlatHashTable[A] {
  var list = List[A]()
  override def initialSize: Int = 32
  def contains(elem: A): Boolean = containsEntry(elem)
  def +=(elem: A): this.type = {
    if (addEntry(elem)) list = elem :: list
    this
  }
  def -=(elem: A) {remove(elem)}
  def remove(elem: A): this.type = {
  	if (removeEntry(elem) == None) list = list.filter(_ != elem)
  	this
  }
  override def clear() { list = Nil; super.clear() }
  override def iterator = list.iterator
}
*/
