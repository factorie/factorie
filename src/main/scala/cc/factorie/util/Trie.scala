/* Copyright (C) 2008-2009 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0.
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.util

//import cc.factorie.util.Index

import scala.collection.mutable._;

trait Trie[T] extends (Seq[T] => Boolean) {
  private val root = new HashMap[T, Int]

  def add(seq: Seq[T]): Unit

  def present(seq: Seq[T], index: Int): Boolean

  /**Constructs an Index from some elements. */
  def apply[T](iterable: Iterable[T]): Index[T] = {
    val index = new Index[T] {};
    // read through all elements now -- don't lazily defer evaluation
    for (element <- iterable) {
      index.index(element);
    }
    return index;
  }

}
