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

/*
 Copyright 2009 David Hall, Daniel Ramage
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 
 
 http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License. 
*/


import scala.collection.mutable._;

/**
 * Class that mimics Java's string indexer, but for anything.
 *
 * Two extra views are provided: the index.synchronized view
 * enables threadsafe access and the index.immutable view keeps
 * prevents the (view) from being updated. 
 *
 * @author dlwh, dramage, 
 * @author Andrew McCallum (multiple changes from original)
 */
//@serializable 
trait Index[T] extends scala.collection.IndexedSeq[T] {
  /**Forward map from int to object */
  private var _objects = new ArrayBuffer[T]

  /**Map from object back to int index */
  private var _indices = Map[T, Int]()
 
  /** Wipe the Index clean */
  def reset: Unit = {
    _frozen = false
    _objects = new ArrayBuffer[T]
    _indices = Map[T,Int]()
  }
  /** Allow subclasses to access the list objects.  Useful for subclasses calling reset and then re-entering a filtered subset of the old objects. */
  protected def _entries: IndexedSeq[T] = _objects

  /**If positive, throw error if Index reaches size larger than this.  Use for growable multi-dim Factor weights */
  var maxSize = -1
  // TODO consider putting the following method back in later -akm
  //override def maxSize_=(s:Int) : Unit = if (maxSize >= size) maxSize = s else throw new Error("Trying to set maxSize smaller than size.")

  /**If true, do not allow this Index to change. */
  private var _frozen = false
  def frozen = _frozen

  // NOTE This used to be just "freeze", but I ran into troubles overriding freeze in DiscreteDomain
  def freeze0: Unit = _frozen = true

  // NOTE This used to be just "allocSize", but I ran into troubles overriding freeze in DiscreteDomain  
  /**The size others might want to allocate to hold data relevant to this Index.  If maxSize is set can be bigger than size. */
  def allocSize0 = if (maxSize < 0) size0 else maxSize

  def size0 = _indices.size
  def length = size0

  override def iterator = _objects.iterator

  override def contains(entry: Any) = _indices.contains(entry.asInstanceOf[T])
  /*entry match {
  	case e:T => _indices.contains(e)
  	case _ => false
  }*/

  def apply(index:Int) = get(index)

  //def unapply(entry:T): Option[Int] = if (_indices.contains(entry)) Some(_indices(entry)) else None

  /**Return an object at the given position or throws an exception if it's not found. */
  def get(pos: Int): T = _objects(pos)

  /**Return a densely-packed positive integer index for the given object.  By default,
  allocate a new index (at the end) if the object was not found, but if immutable may return -1 */
  def index(entry: T) : Int = {
    def nextMax = {
      val m = _objects.size
      if (maxSize > 0 && m >= maxSize) throw new Error("Index size exceeded maxSize")
      _objects += entry;
      m
    }
    if (_frozen) _indices.getOrElse(entry, -1)
    else _indices.getOrElseUpdate(entry, nextMax);
  }
 
  /** Like index, but throw an exception if the entry is not already there. */
  def getIndex(entry:T) : Int = _indices.getOrElse(entry, throw new Error("Entry not present; use index() to cause a lookup."))

  /** Override indexOf's slow, deprecated behavior. */
  //def indexOf[B >: T](elem: B): Int = index(elem.asInstanceOf[T]);

  /**Clears the index. */
  def clear() = {_indices.clear(); _objects.clear(); }

  // These accessors are kept separate to preserve collection subtype
  def indexAll(c: Iterator[T]) = c map index;
  def indexAll(c: List[T]) = c map index;
  def indexAll(c: Array[T]) = c map index;
  def indexAll(c: Set[T]) = c map index;

  def indexKeys[V](c: scala.collection.Map[T, V]) = Map[T, V]() ++ c.map {case (a, b) => (index(a), b)}

  def indexValues[K](c: scala.collection.Map[K, T]) = Map[K, T]() ++ c.map {case (a, b) => (a, index(b))}

  def getAll(c: Iterator[Int]) = c map get;
  def getAll(c: List[Int]) = c map get;
  def getAll(c: Array[Int]) = c map get;
  def getAll(c: Set[Int]) = c map get;

  // Index views.

  /**Returns an immutable view of the index. */
  def immutable: Index[T] = {
    val outer = this;
    new Index[T] {
      override def iterator = outer.iterator;
      override def size0 = outer.size0;
      override def get(pos: Int) = outer.get(pos);
      override def index(t: T) = outer._indices.getOrElse(t, -1);
      override def clear = {};
    }
  }

  /**Returns a synchronized view of the index. */
  def synchronized: Index[T] = {
    val outer = this;
    new Index[T] {
      override def iterator = outer.iterator;
      override def size0 = outer.size0;
      override def get(pos: Int) = synchronized {outer.get(pos); }

      override def index(t: T) = synchronized {outer.index(t); }

      override def clear = synchronized {outer.clear; }
    }
  }
}

/**
 * Utilities for manipulating and creating Index objects.
 */
object Index extends Index[Any] {
  /**Constructs an empty index. */
  def apply[T](): Index[T] = new Index[T] {};

  /**Constructs an Index from some elements. */
  def apply[T](elements: Iterator[T]): Index[T] = {
    val index = new Index[T] {};
    // read through all elements now -- don't lazily defer evaluation
    for (element <- elements) {
      index.index(element);
    }
    return index;
  }

  /**Constructs an Index from some elements. */
  def apply[T](iterable: Iterable[T]): Index[T] = {
    val index = new Index[T] {};
    // read through all elements now -- don't lazily defer evaluation
    for (element <- iterable) {
      index.index(element);
    }
    return index;
  }

  /**
   * Loads a String index, one line per item with line
   * numbers (starting at 0) as the indices.
   */
  def load(source: {def getLines: Iterator[String]}): Index[String] = {
    apply(source.getLines.map(_.stripLineEnd));
  }
}
