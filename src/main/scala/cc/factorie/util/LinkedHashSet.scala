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
