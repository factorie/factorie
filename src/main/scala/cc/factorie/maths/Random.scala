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

package cc.factorie.maths

import java.util.BitSet
import scala.util.Random

// Random number helpers 
trait RandomDistributions {
  implicit val implicitRandom:Random = cc.factorie.random
 
  /** Return random integer from Poission with parameter lambda.  
   * The mean of this distribution is lambda.  The variance is lambda. */
  def nextPoisson(lambda:Double)(implicit r:Random) : Double = {
    var v = -1
    val l=math.exp(-lambda)
    var p=1.0;
    while (p>=l) { p *= nextUniform(r); v += 1 }
    v
  }

  /** Return nextPoisson(1). */
  def nextPoisson(implicit r:Random) : Double = nextPoisson(1)(r)

  /** Return a random boolean, equally likely to be true or false. */
  def nextBoolean(implicit r:Random) = r.nextBoolean
  // TODO  Is this good enough?  Code used to be:
  // return (next(32) & 1 << 15) != 0;


  /** Return a random boolean, with probability p of being true. */
  def nextBoolean(p:Double)(implicit r:Random) = nextUniform(r) < p

  /** Return a random BitSet with "size" bits, each having probability p of being true. */
  def nextBitSet (size:Int, p:Double)(implicit r:Random) : BitSet = {
    val bs = new BitSet (size)
    for (i <- 0 until size) if (nextBoolean(p)(r)) bs.set(i)
    bs
  }

  /** Return a random double in the range 0 to 1, inclusive, uniformly sampled from that range. 
   * The mean of this distribution is 0.5.  The variance is 1/12. */
  @inline final def nextUniform(implicit r:Random) : Double = r.nextDouble
  // Is this good enough?  Code used to be:
  // val l = ((r.self.next(26)).asInstanceOf[Long] << 27) + next(27);
  // l / (1L << 53).asInstanceOf[Double]

  /** Return a random double in the range a to b, inclusive, uniformly sampled from that range.
   * The mean of this distribution is (b-a)/2.  The variance is (b-a)^2/12 */
  //def nextUniform(a:Double, b:Double)(implicit r:Random) : Double = a + (b-a)*nextUniform(r)

  /** Draw a single sample from multinomial "a".  Assumes that the elements of "a" already sum to 1.0. */
  def nextDiscrete (a:Array[Double])(implicit r:Random): Int = {
    var b = 0.0; val s = nextUniform(r); var i = 0
    while (b <= s && i < a.length) { assert (a(i) >= 0.0); b += a(i); i += 1 }
    assert(i > 0)
    i - 1
  }

  /** Draw a single sample from multinomial "a".  Assumes that the elements of "a" already sum to 1.0. */
  def nextDiscrete (a:IndexedSeq[Double])(implicit r:Random): Int = {
    var b = 0.0; val s = nextUniform(r); var i = 0
    while (b <= s && i < a.length) { b += a(i); i += 1 }
    assert(i > 0)
    i - 1
  }

  /** draw a single sample from (unnormalized) multinomial "a", with normalizing factor "sum". */
  def nextDiscrete (a:Array[Double], sum:Double)(implicit r:Random): Int = {
    assert(sum > 0.0, "sum = "+sum)
    var b = 0.0; val s = nextUniform(r) * sum; var i = 0
    while (b <= s && i < a.length) { assert(a(i) >= 0.0); b += a(i); i += 1 }
    assert(i > 0)
    i - 1
  }

  /*
   Directly adapted from:
   Michael Wichura,The Percentage Points of the Normal Distribution, 
   Applied Statistics, Volume 37, Number 3, pages 477-484, 1988.
   Algorithm AS 241,
   */
  

  // TODO Not thread-safe!
  private var nextGaussianValue = 0.0
  private var haveNextGaussianValue = false

  /** Return a random double drawn from a Gaussian distribution with mean 0 and variance 1. */
  def nextGaussian(implicit r:Random) : Double = {
    if (!haveNextGaussianValue) {
      val v1 = nextUniform(r); val v2 = nextUniform(r)
      val x1 = math.sqrt(-2*math.log(v1))*math.cos(2*math.Pi*v2);
      val x2 = math.sqrt(-2*math.log(v1))*math.sin(2*math.Pi*v2);
      nextGaussianValue = x2;
      haveNextGaussianValue = true;
      x1
    } else {
      haveNextGaussianValue = false;
      nextGaussian(r);
    }
  }

  /** Return a random double drawn from a Gaussian distribution with mean m and variance s2. */
  def nextGaussian(mean:Double, s2:Double)(implicit r:Random) :Double = nextGaussian(r) * math.sqrt(s2)+mean

  // generate Gamma(1,1)
  // E(X)=1 ; Var(X)=1
  /** Return a random double drawn from a Gamma distribution with mean 1.0 and variance 1.0. */
  def nextGamma(implicit r:Random) : Double = nextGamma(1,1,0)(r)

  /** Return a random double drawn from a Gamma distribution with mean alpha and variance 1.0. */
  def nextGamma(alpha:Double)(implicit r:Random) : Double = nextGamma(alpha,1,0)(r)

  /** Return a random double drawn from a Gamma distribution with mean alpha*beta and variance alpha*beta^2. */
  def nextGamma(alpha:Double, beta:Double)(implicit r:Random) : Double = nextGamma(alpha,beta,0)(r)

  /** Return a random double drawn from a Gamma distribution with mean alpha*beta+lamba and variance alpha*beta^2. */
  def nextGamma(alpha:Double, beta:Double, lambda:Double)(implicit r:Random) : Double = {
    var gamma = 0.0
    if (alpha <= 0.0 || beta <= 0.0) throw new IllegalArgumentException ("alpha and beta must be strictly positive.");
    if (alpha < 1.0) {
      var p = 0.0
      var flag = false;
      val b = 1+alpha*math.exp(-1)
      while (!flag) {
        p = b*nextUniform(r)
        if (p>1) {
          gamma = -math.log((b-p)/alpha);
          if (nextUniform(r) <= math.pow(gamma,alpha-1)) flag = true
        } else {
          gamma = math.pow(p,1/alpha);
          if (nextUniform(r) <= math.exp(-gamma)) flag = true
        }
      }
    } else if (alpha == 1) {
      gamma = -math.log (nextUniform(r))
    } else {
      var y = -math.log (nextUniform(r))
      while (nextUniform(r) > math.pow (y * math.exp (1 - y), alpha - 1))
        y = -math.log (nextUniform(r))
      gamma = alpha * y
    }
    beta * gamma + lambda;
  }

  /** Return a random double drawn from an Exponential distribution with mean 1 and variance 1. */
  def nextExp(implicit r:Random) = nextGamma(1,1,0)(r)

  /** Return a random double drawn from an Exponential distribution with mean beta and variance beta^2. */
  def nextExp(beta:Double)(implicit r:Random) = nextGamma(1,beta,0)(r)

  /** Return a random double drawn from an Exponential distribution with mean beta+lambda and variance beta^2. */
  def nextExp(beta:Double, lambda:Double)(implicit r:Random) = nextGamma(1,beta,lambda)(r)

  /** Return a random double drawn from an Chi-squarted distribution with mean 1 and variance 2. 
   * Equivalent to nextChiSq(1) */
  def nextChiSq(implicit r:Random) : Double = nextGamma(0.5,2,0)(r)

  /** Return a random double drawn from an Chi-squared distribution with mean df and variance 2*df.  */
  def nextChiSq(df:Int)(implicit r:Random) = nextGamma(0.5*df, 2, 0)(r)

  /** Return a random double drawn from an Chi-squared distribution with mean df+lambda and variance 2*df.  */
  def nextChiSq(df:Int, lambda:Double)(implicit r:Random) = nextGamma(0.5*df,2,lambda)(r)

  /** Return a random double drawn from a Beta distribution with mean a/(a+b) and variance ab/((a+b+1)(a+b)^2).  */
  def nextBeta(alpha:Double, beta:Double)(implicit r:Random) : Double = {
    if (alpha <= 0 || beta <= 0) throw new IllegalArgumentException ("alpha and beta must be strictly positive.")
    if (alpha == 1 && beta == 1) {
      return nextUniform(r)
    } else if (alpha >= 1 && beta >= 1) {
      val A = alpha - 1
      val B = beta - 1
      val C = A + B
      val L = C * math.log (C)
      val mu = A / C
      val sigma = 0.5 / math.sqrt (C)
      var y = nextGaussian(r)
      var x = sigma * y + mu
      while (x < 0 || x > 1) { y = nextGaussian(r); x = sigma * y + mu }
      var u = nextUniform(r)
      while (math.log (u) >= A * math.log (x / A) + B * math.log ((1 - x) / B) + L + 0.5 * y * y) {
        y = nextGaussian(r)
        x = sigma * y + mu
        while (x < 0 || x > 1) { y = nextGaussian(r); x = sigma * y + mu }
        u = nextUniform(r)
      }
      return x
    } else {
      var v1 = math.pow (nextUniform(r), 1 / alpha)
      var v2 = math.pow (nextUniform(r), 1 / beta)
      while (v1 + v2 > 1) {
        v1 = math.pow (nextUniform(r), 1 / alpha)
        v2 = math.pow (nextUniform(r), 1 / beta)
      }
      return v1 / (v1 + v2)
    }
  }
}
