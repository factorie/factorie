///* Copyright (C) 2008-2010 University of Massachusetts Amherst,
//   Department of Computer Science.
//   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
//   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//    http://www.apache.org/licenses/LICENSE-2.0
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License. */
//
//package cc.factorie.la
//import cc.factorie._
//
///** Trait for a Vector that has all zeros, except one position containing a 1.0.
//    @author Andrew McCallum */
//trait SingletonBinaryVec extends Vector {
//  def singleIndex: Int
//  def activeDomainSize = 1
//  def activeDomain: Iterable[Int] = Seq(singleIndex)
//  override def forActiveDomain(f: (Int)=>Unit): Unit = f(singleIndex)
//  def apply(index:Int): Double = if (index == singleIndex) 1.0 else 0.0
//  def dot(v:Vector) = v(singleIndex)
//  def activeElements = Iterator.single((singleIndex, 1.0))
//
//  override def flatOuter(v:Vector):Vector = v match {
//     case that:SparseBinaryVector =>
//      //new SparseBinaryVector(that, this) // TODO !!! Shouldn't this be ordered: (this, that) because first argument is spread further through memory, second argument has last array offset ????
//      new SparseBinaryVector(this, that) // TODO !!! Shouldn't this be ordered: (this, that) because first argument is spread further through memory, second argument has last array offset ????
//    case that:SingletonBinaryVec => new SingletonBinaryVector(this.size * that.size, this.singleIndex * that.size + that.singleIndex)
//    case that:SparseIndexedVector => that flatOuter this
//    case that:SparseHashVector => {
//      val offset = this.singleIndex * that.length
//      val result = new SparseHashVector(this.length * that.length)
//      that.activeDomain.foreach(i => result(offset + i) = that(i))
//      result
//    }
//  }
//
//  override def flatOuter(v1:Vector, v2:Vector):Vector = (v1,v2) match {
//    case (v1:SparseBinaryVector ,v2:SparseBinaryVector)    => new SparseBinaryVector(v1, v2, this)
//    case (v1:SparseBinaryVector ,v2:SingletonBinaryVec) => new SparseBinaryVector(v1, v2, this)
//    case (v1:SingletonBinaryVec ,v2:SparseBinaryVector)    => new SparseBinaryVector(v2, this, v1)
//    case (v1:SingletonBinaryVec ,v2:SingletonBinaryVec) =>
//      new SingletonBinaryVector(this.size * v1.size * v2.size,
//                                (this.singleIndex * v1.size + v1.singleIndex) * v2.size + v2.singleIndex)
//  }
//}
//
///** A Vector that has all zeros, except one position containing a 1.0.
//    @author Andrew McCallum */
//class SingletonBinaryVector(val theLength:Int, val singleIndex:Int) extends SingletonBinaryVec {
//  assert(singleIndex < theLength, "index %d should be less than length %d".format(singleIndex, theLength))
//  def length = theLength
//}
