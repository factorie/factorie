/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.util
import scalala.Scalala._
import scalala.tensor.Vector
import scalala.tensor.Tensor
import scalala.collection.MergeableSet
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}
import scalala.collection.{MergeableSet, IntSpanSet}
                             
/** Helper class making it easier to view as Seq[Double] as a scalala.tensor.Vector. */
trait SeqAsVector extends Vector {
  def apply(i:Int): Double
  def length: Int
  //def size: Int = length
  def update(i:Int, value:Double) = throw new Error("Not implemented")
  def create[J](d:MergeableSet[J]):Tensor[J] = throw new Error("Not implemented")
  val activeDomain: MergeableSet[Int] = IntSpanSet(0, size);
}
