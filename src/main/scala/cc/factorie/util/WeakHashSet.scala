/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.util 
import scala.collection.mutable.WeakHashMap

// TODO This is ugly.  Implement this properly, instead of using silly inner WeakHashMap
/** A set with weak references.
    @author Andrew McCallum */
class WeakHashSet[A] extends scala.collection.mutable.Set[A] {
  val _contents = new WeakHashMap[A,AnyRef]
  def contains(key: A): Boolean = _contents.contains(key)
  def iterator: Iterator[A] = _contents.keysIterator
  def +=(elem: A): this.type = { _contents(elem) = Nil; this }
  def -=(elem: A): this.type = { _contents -= elem; this }
  override def empty: this.type = { _contents.empty; this }
  override def size = _contents.size
}
