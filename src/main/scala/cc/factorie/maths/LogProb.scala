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

//object LogProb extends LogProb
trait LogProb {

  val log2 = math.log(2);


  /**
   * Returns the KL divergence, K(p1 || p2).
   * The log is w.r.t. base 2. <p>
   * *Note*: If any value in <tt>p2</tt> is <tt>0.0</tt> then the KL-divergence
   * is <tt>infinite</tt>. 
   */
  def klDivergence(p1:Seq[Double], p2:Seq[Double]) = {
    assert(p1.length == p2.length);
    var klDiv = 0.0;
    for (i <- 0 until p1.length) {
      if (p1(i) != 0.0)
        klDiv += p1(i) * math.log(p1(i) / p2(i))
    }
    klDiv / log2
  }

  /** Returns the Jensen-Shannon divergence. */
  def jensenShannonDivergence(p1:Seq[Double], p2:Seq[Double]) = {
    assert(p1.length == p2.length);
    val average = new Array[Double](p1.length);
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
   * <P>
   * Note: This function is just like 
   *  {@link cc.mallet.fst.Transducer#sumNegLogProb sumNegLogProb}
   *   in <TT>Transducer</TT>,
   *   except that the logs aren't negated.
   */
  def sumLogProb (a:Double, b:Double) =   {
    if (a == Double.NegativeInfinity) 
      b
    else if (b == Double.NegativeInfinity)
      a
    else if (b < a)
      a + math.log (1 + math.exp(b-a))
    else
      b + math.log (1 + math.exp(a-b))
  }

  // Below adapted from Stanford NLP package, SloppyMath.java
  /**
   * Sums an array of numbers log(x1)...log(xn).  This saves some of
   *  the unnecessary calls to Math.log in the two-argument version.
   * <p>
   * Note that this implementation IGNORES elements of the input
   *  array that are more than LOGTOLERANCE (currently 30.0) less
   *  than the maximum element.
   * <p>
   * Cursory testing makes me wonder if this is actually much faster than
   *  repeated use of the 2-argument version, however -cas.
   * @param vals An array log(x1), log(x2), ..., log(xn)
   * @return log(x1+x2+...+xn)
   */
  def sumLogProbs (vals:Seq[Double])
  {
    val LOGTOLERANCE = 30.0;
    val len = vals.length;
    var max = Double.NegativeInfinity;
    var maxidx = 0;
    for (i <- 0 until len) if (vals(i) > max) { max = vals(i); maxidx = i; }
    var anyAdded = false;
    var intermediate = 0.0;
    val cutoff = max - LOGTOLERANCE;
    for (i <- 0 until maxidx) if (vals(i) >= cutoff) { anyAdded = true; intermediate += math.exp(vals(i) - max) }
    for (i <- maxidx + 1 until len) if (vals(i) >= cutoff) { anyAdded = true; intermediate += math.exp(vals(i) - max) }
    if (anyAdded) max + math.log(1.0 + intermediate) else max
  }

  /**
   *  Returns the difference of two doubles expressed in log space,
   *   that is,
   * <pre>
   *    sumLogProb = log (e^a - e^b)
   *               = log e^a(1 - e^(b-a))
   *               = a + log (1 - e^(b-a))
   * </pre>
   *
   * By exponentiating <tt>b-a</tt>, we obtain better numerical precision than
   *  we would if we calculated <tt>e^a</tt> or <tt>e^b</tt> directly.
   * <p>
   * Returns <tt>NaN</tt> if b > a (so that log(e^a - e^b) is undefined).
   */
  def subtractLogProb (a:Double, b:Double) = 
    if (b == Double.NegativeInfinity) a else a + math.log (1 - math.exp(b-a))

}
