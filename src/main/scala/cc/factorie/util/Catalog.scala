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

import scala.collection.mutable.{ArrayBuffer, HashMap}


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
