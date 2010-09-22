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
  val const1 = 0.180625;
  val const2 = 1.6;
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
  val split1 = 0.425;
  val split2 = 5.0;
}


trait Probit {
  def probit(p: Double): Double = {
    import Probit._
    if (p <= 0)
      return Double.NegativeInfinity
    if (p >= 1)
      return Double.PositiveInfinity
    val q = p - 0.5;
    var r: Double = 0;
    var g: Double = 0;
    if (math.abs(q) <= split1) {
      //System.out.println("CASE 1" );
      r = const1 - q * q;
      g = q * poly(a, r) / poly(b, r);
    } else {
      //System.out.println("CASE 2" );
      if (q < 0) r = p
      else r = 1 - p
      if (r <= 0) g = -1;
      else {
        //System.out.println("  (b)");
        r = math.sqrt(-math.log(r));
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

  def sigmod(beta:Double) = 1.0/(1.0+math.exp(-beta))
  def sigmod_rev(sig:Double) = math.log(sig/(1-sig))
  def logit(p:Double) = math.log (p / (1 - p))

}
