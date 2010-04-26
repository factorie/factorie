/* Copyright (C) 2010-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie

import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.reflect.Manifest



/** Classes that mix in the trait "Catalog" have available a IndexedSeq list of all instances ever created. 
    Note that the Catalog's references are strong; they will never be garbage collected.
    @author Andrew McCallum
    @since 0.9
*/
trait Catalog {
  Catalog += this
  def catalogIndex = Catalog.indexOf(this)
  def catalogSiblings[This<:Catalog] = Catalog.get[This](this.getClass)
}

object Catalog {
  private val cache = new HashMap[Class[_],ArrayBuffer[Catalog]] {
    override def default(k:Class[_]) = new ArrayBuffer[Catalog]
  }
  /** Add elt to the catalog for class A. */
  def +=[A<:Catalog](elt:A): Unit = cache(elt.getClass) += elt
  /** Return the sequence of catalog entries of class A. */
  def apply[A<:Catalog](implicit m:Manifest[A]): Seq[A] = cache(m.erasure).asInstanceOf[Seq[A]]
  /** Return the index'th catalog entry of class A. */
  def apply[A<:Catalog](index:Int)(implicit m:Manifest[A]): A = cache(m.erasure).apply(index).asInstanceOf[A]
  /** Return the sequence of catalog entries of class A. */
  def get[A<:Catalog](c:Class[_]): Seq[A] = cache(c).asInstanceOf[Seq[A]]
  /** Return the index'th catalog entry of class A. */
  def get[A<:Catalog](c:Class[_], index:Int): A = cache(c).apply(index).asInstanceOf[A]
  /** Return the index */
  def indexOf[A<:Catalog](elt:A): Int = cache(elt.getClass).indexOf(elt)
  //def indexOf[A<:Catalog](elt:A, c:Class[_]): Int = cache(c).indexOf(elt)
  /** Return the number of catalog entries of class A. */
  def size[A<:Catalog](implicit m:Manifest[A]): Int = cache(m.erasure).size
  /** Return the number of catalog entries of class A. */
  def size(c:Class[_]): Int = cache(c).size
  /** Make c2 also act as a key for looking up the catalog for class c1.
      Note that you will get a casting error if you then try Catalog.apply[C2](3);
      you must instead use Catalog.get[C1](classOf[C2], 3).
      This somewhat odd functionality helps us save memory in MixtureComponentRef. */
  def alias(c1:Class[_], c2:Class[_]): Unit = if (cache.contains(c1)) require(cache(c1) == cache(c2)) else cache(c1) = cache(c2)
}

// TODO How does this interact with serialization?
