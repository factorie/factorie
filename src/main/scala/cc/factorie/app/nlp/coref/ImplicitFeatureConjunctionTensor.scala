/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
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
package cc.factorie.app.nlp.coref

import cc.factorie.la._
import cc.factorie.util.{DoubleSeq, SparseDoubleSeq}

/**
 * User: apassos
 * Date: 6/27/13
 * Time: 12:21 PM
 */

/**
 * HashConjunctionFeatureTensor is a tensor which implicitly represents all
 * conjunctions of features in its baseFeatures member. It never instantiates
 * all the conjunctions in memory, and it uses hashing for efficiency.
 * @param dim1 - the size of the hash domain for the conjunction features
 * @param baseFeatures - the sparse binary tensor from which conjunctions are being computed
 */
class ImplicitFeatureConjunctionTensor(val dim1: Int, val baseFeatures: SparseBinaryTensor, domain: ImplicitDomain) extends Tensor1 with ReadOnlyTensor with SparseDoubleSeq {
  def activeDomainSize = baseFeatures.activeDomainSize*baseFeatures.activeDomainSize
  def isDense = false
  private val _dim1 = dim1
  private val _a = domain.a
  private val _b = domain.b
  private val _p0 = domain.prime0

  @inline private def prodIndex(i: Int, j: Int): Int = ((i * dim1 + j) * _a + _b)%_p0

  @inline private def index(i: Int, j: Int) = {
    val res = prodIndex(i, j) % _dim1
    if (res < 0) _dim1 + res else res
  }
  @inline private def sign(i: Int, j: Int): Int =
    1 - 2 * (prodIndex(i, j) & 1)

  def dot(ds: DoubleSeq) = ds match {
    case t: DenseTensor =>
      val len = baseFeatures.activeDomainSize
      val indices = baseFeatures._indices
      val arr = t.asArray
      var i = 0
      var dot = 0.0
      while (i < len) {
        var j = 0
        val ii = indices(i)
        while (j < i) {
          val ij = indices(j)
          dot += arr(index(ii, ij)) * sign(ii, ij)
          j += 1
        }
        i += 1
      }
      dot
    case t: Tensor =>
      val len = baseFeatures.activeDomainSize
      val indices = baseFeatures._indices
      var i = 0
      var dot = 0.0
      while (i < len) {
        var j = 0
        val ii = indices(i)
        while (j < i) {
          val ij = indices(j)
          dot += t(index(ii, ij)) * sign(ii, ij)
          j += 1
        }
        i += 1
      }
      dot
  }
  def activeDomain = throw new Error("Can't efficiently enumerate the active domain")
  def apply(i: Int) = throw new Error("Can't efficiently access a value in a given position")

  /**
   * Note: this foreachActiveElement might call the same index twice.
   */
  override def foreachActiveElement(f: (Int, Double) => Unit) {
    val len = baseFeatures.activeDomainSize
    val indices = baseFeatures._indices
    var i = 0
    while (i < len) {
      var j = 0
      val ii = indices(i)
      while (j < i) {
        val ij = indices(j)
        f(index(ii, ij), sign(ii, ij))
        j += 1
      }
      i += 1
    }
  }
}

class ImplicitDomain(baseSize: Int) {
  private lazy val dimSize = baseSize
  lazy val prime0 = PrimeUtils.getRandomPrime(2*dimSize, 10*dimSize, new java.util.Random(0))
  lazy val a = new java.util.Random(0).nextInt(prime0)
  lazy val b = new java.util.Random(1).nextInt(prime0)
}

object PrimeUtils {
  def getRandomPrime(start: Int, end: Int, rand: java.util.Random): Int = {
    // println("start: " + start + " end: " + end )
    while (true) {
      val candidate = (start + rand.nextInt(end - start)) | 1
      if (isPrime(candidate)) return candidate
    }
    sys.error("impossible")
  }
  def isPrime(n: Int): Boolean = {
    require(n > 0)
    val upto = math.sqrt(n).asInstanceOf[Int] + 1
    var i = 2
    while (i <= upto) {
      if (n % i == 0) return false
      i += 1
    }
    true
  }
}
