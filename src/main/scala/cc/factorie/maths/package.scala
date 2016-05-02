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

package cc.factorie
import java.util.BitSet

import cc.factorie.util.DoubleSeq

import scala.util.Random

package object maths {
  
  trait ArrayOps {
    type A = Array[Double]
    def oneNorm(s:A): Double = { var result = 0.0; var i = 0; while (i < s.length) { result += math.abs(s(i)); i += 1 }; result }
    def twoNorm(s:A): Double = { var result = 0.0; var i = 0; while (i < s.length) { result += s(i) * s(i); i += 1 }; math.sqrt(result) }
    def twoNormSquared(s:A): Double = { var result = 0.0; var i = 0; while (i < s.length) { result += s(i) * s(i); i += 1 }; result }
    def infinityNorm(s:A): Double = { var result = s(0); var i = 0; while (i < s.length) { if (math.abs(s(i)) > result) result = math.abs(s(i)); i += 1 }; result }
    def +=(s:A, d:Double): Unit = { var i = 0; while (i < s.length) { s(i) = s(i) + d; i += 1 } }
    def -=(s:A, d:Double): Unit = { var i = 0; while (i < s.length) { s(i) = s(i) - d; i += 1 } }
    def *=(s:A, d:Double): Unit = { var i = 0; while (i < s.length) { s(i) = s(i) * d; i += 1 } }
    def /=(s:A, d:Double): Unit = { var i = 0; while (i < s.length) { s(i) = s(i) / d; i += 1 } }
    def incr(s:A, t:A): Unit = { require(s.length == t.length); var i = 0; while (i < s.length) { s(i) += t(i); i += 1 } }
    def incr(s:A, t:A, factor:Double): Unit = { require(s.length == t.length); var i = 0; while (i < s.length) { s(i) += t(i) * factor; i += 1 } }
    def different(s:A, t:A, threshold:Double): Boolean = { require(s.length == t.length); var i = 0; while (i < s.length) { if (math.abs(s(i) - t(i)) > threshold) return true; i += 1 }; false }
    def dot(s:A, t:A): Double = { assert(s.length == t.length); var result = 0.0; var i = 0; while (i < s.length) { result += s(i) * t(i); i += 1 }; result }
    /** Divide each element of the array by the sum of the elements. */
    def normalize(s:A): Double = { val sum = oneNorm(s); var i = 0; while (i < s.length) { s(i) /= sum; i += 1 }; sum }
    def oneNormalize(s:A): Double = normalize(s)
    def twoNormalize(s:A): Double = { val norm = twoNorm(s); var i = 0; while (i < s.length) { s(i) /= norm; i += 1 }; norm }
    def twoSquaredNormalize(s:A): Double = { val norm = twoNormSquared(s); var i = 0; while (i < s.length) { s(i) /= norm; i += 1 }; norm }
    def contains(s:A, d:Double): Boolean = { var i = 0; while (i < s.length) { if (s(i) == d) return true; i += 1 }; false }
    def maxIndex(a:Array[Double]): Int = { var j = 0; for (i <- 0 until a.length) if (a(j) < a(i)) j = i; j }
    def isNaN(s:A): Boolean = contains(s, Double.NaN)
    def substitute(s:A, oldValue:Double, newValue:Double): Unit = { var i = 0; while (i < s.length) { if (s(i) == oldValue) s(i) = newValue; i += 1 } }
    def copy(s:A): Array[Double] = { val result = new Array[Double](s.length); set(result, s); result }
    def set(s:A, t:A): Unit = { require(s.length == t.length); System.arraycopy(t, 0, s, 0, s.length) }
    def elementwiseSum(as: Array[Array[Double]]): Array[Double] = {
      val result = Array.fill(as(0).size)(0.0)
      var i = 0
      while (i < as.size) {
        val a = as(i)
        var j = 0
        while (j < a.size) {
          result(j) += a(j)
          j += 1
        }
        i += 1
      }
      result
    }
    /** Exponentiate the elements of the array, and then normalize them to sum to one. */
    def expNormalize(a:Array[Double]): Double = {
      var max = Double.MinValue
      var i = 0
      while (i < a.length) { if (max < a(i)) max = a(i); i += 1 }
      var sum = 0.0
      i = 0
      while (i < a.length) {
        a(i) = math.exp(a(i) - max)
        sum += a(i)
        i += 1
      }
      i = 0
      while (i < a.length) { a(i) /= sum; i += 1 }
      sum
    }

    /** expNormalize, then put back into log-space. */
    def normalizeLogProb(a:Array[Double]): Double = {
      // normalizeLogProb: [log(a), log(b), log(c)] --> [log(a/Z), log(b/Z), log(c/Z)] where Z = a+b+c
      // expNormalize: [log(a), log(b), log(c)] --> [a/Z, b/Z, c/Z] where Z=a+b+c
      val n = expNormalize(a)
      for (i <- 0 until a.length) a(i) = math.log(a(i))
      n
    }

  }

  object ArrayOps extends ArrayOps

// Binomial

   /** Computes p(x;n,p) where x~B(n,p)  */
  // Copied as the "classic" method from Catherine Loader.
  //  Fast and Accurate Computation of Binomial Probabilities.
  //   2001.  (This is not the fast and accurate version.)
  def logBinom(x: Int, n: Int, p: Double): Double = {
    logFactorial (n) - logFactorial (x) - logFactorial (n - x)
      + (x*math.log (p)) + ((n-x)*math.log (1-p))
   }

  /** Vastly inefficient O(x) method to compute cdf of B(n,p)  */
  def pbinom(x: Int, n: Int, p: Double): Double = {
    var sum: Double = Double.NegativeInfinity
    for (i <- 0 to x) sum = sumLogProb(sum, logBinom(i, n, p))
    math.exp(sum)
  }

// Close

  /** Numbers that are closer than this are considered equal */
  def almostEquals(d1: Double, d2: Double, epsilon: Double = 0.000001): Boolean = math.abs(d1 - d2) < epsilon

// Combinatorics

  def numCombinations(n: Int, r: Int) = math.exp(logFactorial(n)-logFactorial(r)-logFactorial(n-r))
  def numPermutations(n: Int, r: Int) = math.exp(logFactorial(n)-logFactorial(r))

// Factorial

  object FactorialCache {
    val size = 13 // 12! = 479 001 600, 13! = 6 227 020 800, java.Integer.MAX_INT = (2^31) - 1 = 2 147 483 647
    private val cache = new Array[Int](size)
    cache(0) = 1; for (i <- 1 until size) cache(i) = i * cache(i-1)
    def factorial(n: Int): Int = cache(n)
  }

  def factorial(n: Int): Double = if (n < FactorialCache.size) FactorialCache.factorial(n) else math.exp(logGamma(n+1.0))
  def logFactorial(n: Int): Double = logGamma(n+1.0)

  // Digamma

  // This implementation is from the Apache Commons math library, version 3.2
  object Digamma {
    // Euler-Mascheroni constant
    val GAMMA = 0.577215664901532860606512090082
    val S_LIMIT = 1e-5
    val C_LIMIT = 49

    def digamma(x: Double): Double =
      if (x > 0 && x <= S_LIMIT)
        -GAMMA - 1.0 / x
      else if (x >= C_LIMIT) {
        val inv = 1.0 / (x * x)
        math.log(x) - 0.5 / x - inv * ((1.0 / 12) + inv * (1.0 / 120 - inv / 252))
      } else
        digamma(x + 1) - 1 / x
  }

  def digamma(x: Double): Double = Digamma.digamma(x)

  // LogGamma

  // todo: use fast "log"
  // This implementation is from the Apache Commons math library, version 3.2
  object LogGamma {
    val INV_GAMMA1P_M1_A0 = .611609510448141581788E-08
    val INV_GAMMA1P_M1_A1 = .624730830116465516210E-08
    val INV_GAMMA1P_M1_B1 = .203610414066806987300E+00
    val INV_GAMMA1P_M1_B2 = .266205348428949217746E-01
    val INV_GAMMA1P_M1_B3 = .493944979382446875238E-03
    val INV_GAMMA1P_M1_B4 = -.851419432440314906588E-05
    val INV_GAMMA1P_M1_B5 = -.643045481779353022248E-05
    val INV_GAMMA1P_M1_B6 = .992641840672773722196E-06
    val INV_GAMMA1P_M1_B7 = -.607761895722825260739E-07
    val INV_GAMMA1P_M1_B8 = .195755836614639731882E-09
    val INV_GAMMA1P_M1_P0 = .6116095104481415817861E-08
    val INV_GAMMA1P_M1_P1 = .6871674113067198736152E-08
    val INV_GAMMA1P_M1_P2 = .6820161668496170657918E-09
    val INV_GAMMA1P_M1_P3 = .4686843322948848031080E-10
    val INV_GAMMA1P_M1_P4 = .1572833027710446286995E-11
    val INV_GAMMA1P_M1_P5 = -.1249441572276366213222E-12
    val INV_GAMMA1P_M1_P6 = .4343529937408594255178E-14
    val INV_GAMMA1P_M1_Q1 = .3056961078365221025009E+00
    val INV_GAMMA1P_M1_Q2 = .5464213086042296536016E-01
    val INV_GAMMA1P_M1_Q3 = .4956830093825887312020E-02
    val INV_GAMMA1P_M1_Q4 = .2692369466186361192876E-03
    val INV_GAMMA1P_M1_C = -.422784335098467139393487909917598E+00
    val INV_GAMMA1P_M1_C0 = .577215664901532860606512090082402E+00
    val INV_GAMMA1P_M1_C1 = -.655878071520253881077019515145390E+00
    val INV_GAMMA1P_M1_C2 = -.420026350340952355290039348754298E-01
    val INV_GAMMA1P_M1_C3 = .166538611382291489501700795102105E+00
    val INV_GAMMA1P_M1_C4 = -.421977345555443367482083012891874E-01
    val INV_GAMMA1P_M1_C5 = -.962197152787697356211492167234820E-02
    val INV_GAMMA1P_M1_C6 = .721894324666309954239501034044657E-02
    val INV_GAMMA1P_M1_C7 = -.116516759185906511211397108401839E-02
    val INV_GAMMA1P_M1_C8 = -.215241674114950972815729963053648E-03
    val INV_GAMMA1P_M1_C9 = .128050282388116186153198626328164E-03
    val INV_GAMMA1P_M1_C10 = -.201348547807882386556893914210218E-04
    val INV_GAMMA1P_M1_C11 = -.125049348214267065734535947383309E-05
    val INV_GAMMA1P_M1_C12 = .113302723198169588237412962033074E-05
    val INV_GAMMA1P_M1_C13 = -.205633841697760710345015413002057E-06

    val HALF_LOG_2_PI = 0.5 * math.log(2.0 * math.Pi)

    val LANCZOS_G = 607.0 / 128.0
    val LANCZOS = Array[Double](
        0.99999999999999709182,
        57.156235665862923517,
        -59.597960355475491248,
        14.136097974741747174,
        -0.49191381609762019978,
        .33994649984811888699e-4,
        .46523628927048575665e-4,
        -.98374475304879564677e-4,
        .15808870322491248884e-3,
        -.21026444172410488319e-3,
        .21743961811521264320e-3,
        -.16431810653676389022e-3,
        .84418223983852743293e-4,
        -.26190838401581408670e-4,
        .36899182659531622704e-5)

    def lanczos(x: Double): Double = {
      var sum = 0.0
      for (i <- LANCZOS.length - 1 until 0 by -1)
        sum = sum + (LANCZOS(i) / (x + i))
      sum + LANCZOS(0)
    }
    def logGamma1p(x: Double): Double ={
      if (x < -0.5)
        sys.error("Number is too small: %f < -0.5" format x)
      if (x > 1.5)
        sys.error("Number is too small: %f > 1.5" format x)
      -math.log1p(invGamma1pm1(x))
    }
    def logGamma(x: Double): Double =
      if (x.isNaN || (x <= 0.0))
        Double.NaN
      else if (x < 0.5)
        logGamma1p(x) - math.log(x)
      else if (x <= 2.5)
        logGamma1p((x - 0.5) - 0.5)
      else if (x <= 8.0) {
        val n = math.floor(x - 1.5).asInstanceOf[Int]
        var prod = 1.0
        for (i <- 1 to n)
          prod *= x - i
        logGamma1p(x - (n + 1)) + math.log(prod)
      } else {
        val sum = lanczos(x)
        val tmp = x + LANCZOS_G + .5
        ((x + .5) * math.log(tmp)) - tmp +
        HALF_LOG_2_PI + math.log(sum / x)
      }

    def invGamma1pm1(x: Double): Double = {
      if (x < -0.5)
        sys.error("Number is too small: %f < -0.5" format x)
      if (x > 1.5)
        sys.error("Number is too small: %f > 1.5" format x)

      val t = if (x <= 0.5) x else (x - 0.5) - 0.5
      if (t < 0.0) {
        val a = INV_GAMMA1P_M1_A0 + t * INV_GAMMA1P_M1_A1
        var b = INV_GAMMA1P_M1_B8
        b = INV_GAMMA1P_M1_B7 + t * b
        b = INV_GAMMA1P_M1_B6 + t * b
        b = INV_GAMMA1P_M1_B5 + t * b
        b = INV_GAMMA1P_M1_B4 + t * b
        b = INV_GAMMA1P_M1_B3 + t * b
        b = INV_GAMMA1P_M1_B2 + t * b
        b = INV_GAMMA1P_M1_B1 + t * b
        b = 1.0 + t * b

        var c = INV_GAMMA1P_M1_C13 + t * (a / b)
        c = INV_GAMMA1P_M1_C12 + t * c
        c = INV_GAMMA1P_M1_C11 + t * c
        c = INV_GAMMA1P_M1_C10 + t * c
        c = INV_GAMMA1P_M1_C9 + t * c
        c = INV_GAMMA1P_M1_C8 + t * c
        c = INV_GAMMA1P_M1_C7 + t * c
        c = INV_GAMMA1P_M1_C6 + t * c
        c = INV_GAMMA1P_M1_C5 + t * c
        c = INV_GAMMA1P_M1_C4 + t * c
        c = INV_GAMMA1P_M1_C3 + t * c
        c = INV_GAMMA1P_M1_C2 + t * c
        c = INV_GAMMA1P_M1_C1 + t * c
        c = INV_GAMMA1P_M1_C + t * c
        if (x > 0.5)
          t * c / x
        else
          x * ((c + 0.5) + 0.5)
      } else {
        var p = INV_GAMMA1P_M1_P6
        p = INV_GAMMA1P_M1_P5 + t * p
        p = INV_GAMMA1P_M1_P4 + t * p
        p = INV_GAMMA1P_M1_P3 + t * p
        p = INV_GAMMA1P_M1_P2 + t * p
        p = INV_GAMMA1P_M1_P1 + t * p
        p = INV_GAMMA1P_M1_P0 + t * p

        var q = INV_GAMMA1P_M1_Q4
        q = INV_GAMMA1P_M1_Q3 + t * q
        q = INV_GAMMA1P_M1_Q2 + t * q
        q = INV_GAMMA1P_M1_Q1 + t * q
        q = 1.0 + t * q

        var c = INV_GAMMA1P_M1_C13 + (p / q) * t
        c = INV_GAMMA1P_M1_C12 + t * c
        c = INV_GAMMA1P_M1_C11 + t * c
        c = INV_GAMMA1P_M1_C10 + t * c
        c = INV_GAMMA1P_M1_C9 + t * c
        c = INV_GAMMA1P_M1_C8 + t * c
        c = INV_GAMMA1P_M1_C7 + t * c
        c = INV_GAMMA1P_M1_C6 + t * c
        c = INV_GAMMA1P_M1_C5 + t * c
        c = INV_GAMMA1P_M1_C4 + t * c
        c = INV_GAMMA1P_M1_C3 + t * c
        c = INV_GAMMA1P_M1_C2 + t * c
        c = INV_GAMMA1P_M1_C1 + t * c
        c = INV_GAMMA1P_M1_C0 + t * c

        if (x > 0.5)
          (t / x) * ((c - 0.5) - 0.5)
        else
          x * c
      }
    }
  }

  def logGamma(xa: Double): Double = LogGamma.logGamma(xa)
  def logBeta(a: Double, b: Double) = logGamma(a)+logGamma(b)-logGamma(a+b)
  def beta(a: Double, b: Double) = math.exp(logBeta(a,b))
  def gamma(x: Double) = math.exp(logGamma(x))

// Log Prob
  
  val log2 = math.log(2)

  /** Return the entropy of the normalized distribution p.
      The log is w.r.t. base 2.  */
  def entropy(p: Array[Double]): Double = {
    var result = 0.0
    var i = p.length - 1
    var pv = 0.0
    while (i >= 0) {
      pv = p(i)
      require(pv >= 0.0, pv)
      require(pv <= 1.000001, pv)
      if (pv > 0.0)
        result -= pv * math.log(pv)
      i -= 1
    }
    result / log2
  }

  /**
   * Returns the KL divergence, K(p1 || p2).
   * The log is w.r.t. base 2. <p>
   * *Note*: If any value in <tt>p2</tt> is <tt>0.0</tt> then the KL-divergence
   * is <tt>infinite</tt>. 
   */
  def klDivergence(p1: Array[Double], p2: Array[Double]) = {
    assert(p1.length == p2.length)
    var klDiv = 0.0
    for (i <- 0 until p1.length) {
      if (p1(i) != 0.0)
        klDiv += p1(i) * math.log(p1(i) / p2(i))
    }
    klDiv / log2
  }

  /** Returns the Jensen-Shannon divergence. */
  def jensenShannonDivergence(p1: Array[Double], p2: Array[Double]) = {
    assert(p1.length == p2.length)
    val average = new Array[Double](p1.length)
    for (i <- 0 until p1.length) average(i) += (p1(i) + p2(i)) / 2.0
    (klDivergence(p1, average) + klDivergence(p2, average)) / 2.0
  }

  /**
   *  Returns the sum of two doubles expressed in log space,
   *   that is,
   * <pre>
   *    sumLogProb = log (e^a + e^b)
   *               = log e^a(1 + e^(b-a))
   *               = a + log (1 + e^(b-a))
   * </pre>
   *
   * By exponentiating <tt>b-a</tt>, we obtain better numerical precision than
   *  we would if we calculated <tt>e^a</tt> or <tt>e^b</tt> directly.
   */
  def sumLogProb(a: Double, b: Double) =   {
    if (a.isNegInfinity)
      b
    else if (b.isNegInfinity)
      a
    else if (b < a)
      a + math.log1p(math.exp(b-a))
    else
      b + math.log1p(math.exp(a-b))
  }

  def sumLogProbs(vals: Array[Double]): Double = {
    val LOGTOLERANCE = 30.0

    val len = vals.length
    var max = vals(0)
    var maxIdx = 0
    var i = 1
    while (i < len) {
      val v = vals(i)
      if (v > max) {
        max = v
        maxIdx = i
      }
      i += 1
    }
    if (max == Double.NegativeInfinity) return max
    var anyAdded = false
    var intermediate = 0.0
    val cutoff = max - LOGTOLERANCE
    i = 0
    while (i < len) {
      if (vals(i) >= cutoff && i != maxIdx && !vals(i).isInfinite) {
        anyAdded = true
        intermediate += math.exp(vals(i) - max)
      }
      i += 1
    }
    if (anyAdded)
      max + math.log1p(intermediate)
    else
      max
  }

  def sumLogProbs(vals: DoubleSeq): Double = {
    val LOGTOLERANCE = 30.0

    val len = vals.length
    var max = vals(0)
    var maxIdx = 0
    var i = 1
    while (i < len) {
      val v = vals(i)
      if (v > max) {
        max = v
        maxIdx = i
      }
      i += 1
    }
    if (max == Double.NegativeInfinity) return max
    var anyAdded = false
    var intermediate = 0.0
    val cutoff = max - LOGTOLERANCE
    i = 0
    while (i < len) {
      if (vals(i) >= cutoff && i != maxIdx && !vals(i).isInfinite) {
        anyAdded = true
        intermediate += math.exp(vals(i) - max)
      }
      i += 1
    }
    if (anyAdded)
      max + math.log1p(intermediate)
    else
      max
  }

  def subtractLogProb(a: Double, b: Double) =
    if (b.isNegInfinity) a else a + math.log (1 - math.exp(b-a))

// Poly

  def poly(coeff: Array[Double], x: Double): Double = {
    var result: Double = 0
    var i: Int = coeff.length - 1
    while (i >= 0) {
      result = result * x + coeff(i)
      i -= 1
    }
    result
  }

// Probit

  object Probit {
    val a = Array(
      3.3871328727963666080,
      133.14166789178437745,
      1971.5909503065514427,
      13731.693765509461125,
      45921.953931549871457,
      67265.770927008700853,
      33430.575583588128105,
      2509.0809287301226727
    )
    val b = Array(
      1.0,
      42.313330701600911252,
      687.18700749205790830,
      5394.1960214247511077,
      21213.794301586595867,
      39307.895800092710610,
      28729.085735721942674,
      5226.4952788528545610
    )
    val c = Array(
      1.42343711074968357734,
      4.63033784615654529590,
      5.76949722146069140550,
      3.64784832476320460504,
      1.27045825245236838258,
      0.241780725177450611770,
      0.0227238449892691845833,
      0.00077454501427834140764
    )
    val const1 = 0.180625
    val const2 = 1.6
    val d = Array(
      1.0E+00,
      2.05319162663775882187E+00,
      1.67638483018380384940E+00,
      6.89767334985100004550E-01,
      1.48103976427480074590E-01,
      1.51986665636164571966E-02,
      5.47593808499534494600E-04,
      1.05075007164441684324E-09
    )
    val e = Array(
      6.65790464350110377720E+00,
      5.46378491116411436990E+00,
      1.78482653991729133580E+00,
      2.96560571828504891230E-01,
      2.65321895265761230930E-02,
      1.24266094738807843860E-03,
      2.71155556874348757815E-05,
      2.01033439929228813265E-07
    )
    val f = Array(
      1.0E+00,
      5.99832206555887937690E-01,
      1.36929880922735805310E-01,
      1.48753612908506148525E-02,
      7.86869131145613259100E-04,
      1.84631831751005468180E-05,
      1.42151175831644588870E-07,
      2.04426310338993978564E-15
    )
    val split1 = 0.425
    val split2 = 5.0
  }

  def probit(p: Double): Double = {
    import Probit._
    if (p <= 0)
      return Double.NegativeInfinity
    if (p >= 1)
      return Double.PositiveInfinity
    val q = p - 0.5
    var r: Double = 0
    var g: Double = 0
    if (math.abs(q) <= split1) {
      //System.out.println("CASE 1" );
      r = const1 - q * q
      g = q * poly(a, r) / poly(b, r)
    } else {
      //System.out.println("CASE 2" );
      if (q < 0) r = p
      else r = 1 - p
      if (r <= 0) g = -1
      else {
        //System.out.println("  (b)");
        r = math.sqrt(-math.log(r))
        if (r <= split2) {
          //System.out.println("   (i)");
          r = r - const2
          g = poly(c, r) / poly(d, r)
        } else {
          //System.out.println("   (ii)");
          r = r - split2
          g = poly(e, r) / poly(f, r)
        }
        //r=r-split2
        if (q < 0) g = -g
      }
    }
    g
  }

  def sigmoid(beta: Double) = 1.0/(1.0+math.exp(-beta))
  def sigmoid_rev(sig: Double) = logit(sig)
  def logit(p: Double) = math.log (p / (1 - p))

  // Random Distributions

//  implicit val implicitRandom:Random = cc.factorie.random
 
  /** Return random integer from Poission with parameter lambda.  
   * The mean of this distribution is lambda.  The variance is lambda. */
  def nextPoisson(lambda: Double)(implicit r: Random): Double = {
    var v = -1
    val l=math.exp(-lambda)
    var p=1.0
    while (p>=l) { p *= nextUniform(r); v += 1 }
    v
  }

  /** Return nextPoisson(1). */
  def nextPoisson(implicit r: Random): Double = nextPoisson(1)(r)

  /** Return a random boolean, equally likely to be true or false. */
  def nextBoolean(implicit r: Random) = r.nextBoolean()
  // TODO  Is this good enough?  Code used to be:
  // return (next(32) & 1 << 15) != 0;

  /** Return a random boolean, with probability p of being true. */
  def nextBoolean(p: Double)(implicit r: Random) = nextUniform(r) < p

  /** Return a random BitSet with "size" bits, each having probability p of being true. */
  def nextBitSet(size: Int, p: Double)(implicit r: Random): BitSet = {
    val bs = new BitSet(size)
    for (i <- 0 until size) if (nextBoolean(p)(r)) bs.set(i)
    bs
  }

  /** Return a random double in the range 0 to 1, inclusive, uniformly sampled from that range. 
   * The mean of this distribution is 0.5.  The variance is 1/12. */
  @inline final def nextUniform(implicit r: Random): Double = r.nextDouble()
  // Is this good enough?  Code used to be:
  // val l = ((r.self.next(26)).asInstanceOf[Long] << 27) + next(27);
  // l / (1L << 53).asInstanceOf[Double]

  /** Return a random double in the range a to b, inclusive, uniformly sampled from that range.
   * The mean of this distribution is (b-a)/2.  The variance is (b-a)^2/12 */
  //def nextUniform(a:Double, b:Double)(implicit r:Random) : Double = a + (b-a)*nextUniform(r)

  /** Draw a single sample from multinomial "a".  Assumes that the elements of "a" already sum to 1.0. */
  def nextDiscrete(a: Array[Double])(implicit r: Random): Int = {
    var b = 0.0; val s = nextUniform(r); var i = 0
    while (b <= s && i < a.length) { assert (a(i) >= 0.0); b += a(i); i += 1 }
    assert(i > 0)
    i - 1
  }

  /** draw a single sample from (unnormalized) multinomial "a", with normalizing factor "sum". */
  def nextDiscrete(a: Array[Double], sum: Double)(implicit r: Random): Int = {
    assert(sum > 0.0, "sum = "+sum)
    var b = 0.0; val s = nextUniform(r) * sum; var i = 0
    while (b <= s && i < a.length) { assert(a(i) >= 0.0); b += a(i); i += 1 }
    assert(i > 0)
    i - 1
  }

  /** Return a random double drawn from a Gaussian distribution with mean 0 and variance 1. */
  def nextGaussian(implicit r: Random): Double = r.nextGaussian()

  /** Return a random double drawn from a Gaussian distribution with mean m and variance s2. */
  def nextGaussian(mean: Double, s2: Double)(implicit r: Random): Double = nextGaussian(r) * math.sqrt(s2)+mean

  // generate Gamma(1,1)
  // E(X)=1 ; Var(X)=1
  /** Return a random double drawn from a Gamma distribution with mean 1.0 and variance 1.0. */
  def nextGamma(implicit r: Random): Double = nextGamma(1,1,0)(r)

  /** Return a random double drawn from a Gamma distribution with mean alpha and variance 1.0. */
  def nextGamma(alpha: Double)(implicit r: Random): Double = nextGamma(alpha,1,0)(r)

  /** Return a random double drawn from a Gamma distribution with mean alpha*beta and variance alpha*beta^2. */
  def nextGamma(alpha: Double, beta: Double)(implicit r: Random): Double = nextGamma(alpha,beta,0)(r)

  /** Return a random double drawn from a Gamma distribution with mean alpha*beta+lamba and variance alpha*beta^2. */
  def nextGamma(alpha: Double, beta: Double, lambda: Double)(implicit r: Random): Double = {
    var gamma = 0.0
    if (alpha <= 0.0 || beta <= 0.0) throw new IllegalArgumentException ("alpha and beta must be strictly positive.")
    if (alpha < 1.0) {
      var p = 0.0
      var flag = false
      val b = 1+alpha*math.exp(-1)
      while (!flag) {
        p = b*nextUniform(r)
        if (p>1) {
          gamma = -math.log((b-p)/alpha)
          if (nextUniform(r) <= math.pow(gamma,alpha-1)) flag = true
        } else {
          gamma = math.pow(p,1/alpha)
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
    beta * gamma + lambda
  }

  /** Return a random double drawn from an Exponential distribution with mean 1 and variance 1. */
  def nextExp(implicit r: Random) = nextGamma(1,1,0)(r)

  /** Return a random double drawn from an Exponential distribution with mean beta and variance beta^2. */
  def nextExp(beta: Double)(implicit r: Random) = nextGamma(1,beta,0)(r)

  /** Return a random double drawn from an Exponential distribution with mean beta+lambda and variance beta^2. */
  def nextExp(beta: Double, lambda: Double)(implicit r: Random) = nextGamma(1,beta,lambda)(r)

  /** Return a random double drawn from an Chi-squarted distribution with mean 1 and variance 2. 
   * Equivalent to nextChiSq(1) */
  def nextChiSq(implicit r: Random): Double = nextGamma(0.5,2,0)(r)

  /** Return a random double drawn from an Chi-squared distribution with mean df and variance 2*df.  */
  def nextChiSq(df: Int)(implicit r: Random) = nextGamma(0.5*df, 2, 0)(r)

  /** Return a random double drawn from an Chi-squared distribution with mean df+lambda and variance 2*df.  */
  def nextChiSq(df: Int, lambda: Double)(implicit r: Random) = nextGamma(0.5*df,2,lambda)(r)

  /** Return a random double drawn from a Beta distribution with mean a/(a+b) and variance ab/((a+b+1)(a+b)^2).  */
  def nextBeta(alpha: Double, beta: Double)(implicit r: Random): Double = {
    if (alpha <= 0 || beta <= 0) throw new IllegalArgumentException ("alpha and beta must be strictly positive.")
    if (alpha == 1 && beta == 1) {
      nextUniform(r)
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
      x
    } else {
      var v1 = math.pow (nextUniform(r), 1 / alpha)
      var v2 = math.pow (nextUniform(r), 1 / beta)
      while (v1 + v2 > 1) {
        v1 = math.pow (nextUniform(r), 1 / alpha)
        v2 = math.pow (nextUniform(r), 1 / beta)
      }
      v1 / (v1 + v2)
    }
  }

// Sample statistics
  
  def sampleMean(ds: DoubleSeq): Double = ds.sum / ds.length
  def sampleVariance(ds: DoubleSeq): Double = sampleVariance(ds, sampleMean(ds))
  /** Returns the unbiased sample variance.  See http://en.wikipedia.org/wiki/Variance#Population_variance_and_sample_variance */
  def sampleVariance(ds: DoubleSeq, mean: Double): Double = {
    val len = ds.length; var i = 0
    var v = 0.0
    while (i < len) {
      val diff = mean - ds(i)
      v += diff * diff
      i += 1
    }
    //math.sqrt(v / (len - 1))
    v / (len - 1)
  }

// Trig

  def cosh(a: Double) = if (a < 0) 0.5 * (math.exp(-a) + math.exp(a)) else 0.5 * (math.exp(a) + math.exp(-a))
  def tanh(a: Double) = (math.exp(a) - math.exp(-a)) / (math.exp(a) + math.exp(-a))
}
