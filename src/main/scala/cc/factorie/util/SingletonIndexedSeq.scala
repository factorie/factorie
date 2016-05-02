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

/** An immutable indexed sequence containing a single element, with efficient implementations of head, last, foreach, iterator and map.
    @author Andrew McCallum */
case class SingletonIndexedSeq[A](elt:A) extends IndexedSeq[A] {
  def apply(i:Int): A = if (i == 0) elt else throw throw new IndexOutOfBoundsException(i.toString)
  def length: Int = 1
  override def head: A = elt
  override def last: A = elt
  override def foreach[U](f: (A)=>U): Unit = f(elt)
  override def iterator: Iterator[A] = Iterator.single(elt)
  def map[B](f: (A) => B): SingletonIndexedSeq[B] = new SingletonIndexedSeq(f(elt))
  // TODO Figure out how to override this properly. 
  //override def map[B, That](f: A => B)(implicit bf: scala.collection.generic.CanBuildFrom[IndexedSeq[A],B,That]): That = new SingletonIndexedSeq(f(elt))
}

//import scala.collection.generic.SeqFactory
//import scala.collection.generic.CanBuildFrom
//import scala.collection.mutable.ArrayBuffer
//object SingletonIndexedSeq extends SeqFactory[SingletonIndexedSeq] {
//  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, SingletonIndexedSeq[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
//  //def newBuilder[A]: Builder[A, ArrayBuffer[A]] = new ArrayBuffer[A]
//}
