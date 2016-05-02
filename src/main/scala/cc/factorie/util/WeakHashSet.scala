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
