
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

package cc.factorie.la

// This code was originally in Template.scala - the implementation of flatOuter has changed, 
// but this version is now used as a reference to catch any errors in the new version
object OriginalFlatOuter {
  def flatOuter(vector1: Vector, vector2: Vector) : Vector = vector1 match {
    case v1: SingletonBinaryVector => vector2 match {
      case v2: SingletonBinaryVector =>
        new SingletonBinaryVector(v1.size * v2.size, v1.singleIndex * v2.size + v2.singleIndex)
      case v2: SparseBinaryVector =>
        new SparseBinaryVector(v1.size * v2.size,
                               {
                                 val arr = new Array[Int](v2.activeDomain.size);
                                 var i = 0;
                                 for (i2 <- v2.activeDomain) {
                                   arr(i) = v1.singleIndex * v2.size + i2;
                                   i += 1;
                                 };
                                 arr
                               })
    }
    case v1: SparseBinaryVector => vector2 match {
      case v2: SingletonBinaryVector =>
        new SparseBinaryVector(v1.size * v2.size,
                               {
                                 val arr = new Array[Int](v1.activeDomain.size);
                                 var i = 0;
                                 for (i1 <- v1.activeDomain) {
                                   arr(i) = i1 * v2.size + v2.singleIndex;
                                   i += 1;
                                 };
                                 arr
                               })
      case v2: SparseBinaryVector =>
        new SparseBinaryVector(v1.size * v2.size,
                               {
                                 val arr = new Array[Int](v1.activeDomain.size * v2.activeDomain.size);
                                 var i = 0;
                                 for (i1 <- v1.activeDomain; i2 <- v2.activeDomain) {
                                   arr(i) = i1 * v2.size + i2;
                                   i += 1;
                                 };
                                 arr
                               })
    }
  }
}
