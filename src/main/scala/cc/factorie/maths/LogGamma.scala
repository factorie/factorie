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


object LogGamma {
  val d1 = -5.772156649015328605195174e-1;
  val p1 = Array( 
    4.945235359296727046734888e0, 2.018112620856775083915565e2, 
    2.290838373831346393026739e3, 1.131967205903380828685045e4, 
    2.855724635671635335736389e4, 3.848496228443793359990269e4, 
    2.637748787624195437963534e4, 7.225813979700288197698961e3 
  )
  val q1 = Array(
    6.748212550303777196073036e1, 1.113332393857199323513008e3, 
    7.738757056935398733233834e3, 2.763987074403340708898585e4, 
    5.499310206226157329794414e4, 6.161122180066002127833352e4, 
    3.635127591501940507276287e4, 8.785536302431013170870835e3
  )
  val d2 = 4.227843350984671393993777e-1
  val p2 = Array(
    4.974607845568932035012064e0, 5.424138599891070494101986e2, 
    1.550693864978364947665077e4, 1.847932904445632425417223e5, 
    1.088204769468828767498470e6, 3.338152967987029735917223e6, 
    5.106661678927352456275255e6, 3.074109054850539556250927e6
  )
  val q2 = Array(
    1.830328399370592604055942e2, 7.765049321445005871323047e3, 
    1.331903827966074194402448e5, 1.136705821321969608938755e6, 
    5.267964117437946917577538e6, 1.346701454311101692290052e7, 
    1.782736530353274213975932e7, 9.533095591844353613395747e6
  )
  val d4 = 1.791759469228055000094023e0;
  val p4 = Array(
    1.474502166059939948905062e4, 2.426813369486704502836312e6, 
    1.214755574045093227939592e8, 2.663432449630976949898078e9, 
    2.940378956634553899906876e10, 1.702665737765398868392998e11, 
    4.926125793377430887588120e11, 5.606251856223951465078242e11
  )
  val q4 = Array(
    2.690530175870899333379843e3, 6.393885654300092398984238e5, 
    4.135599930241388052042842e7, 1.120872109616147941376570e9, 
    1.488613728678813811542398e10, 1.016803586272438228077304e11, 
    3.417476345507377132798597e11, 4.463158187419713286462081e11
  )
  val c = Array(
    -1.910444077728e-03, 8.4171387781295e-04, 
    -5.952379913043012e-04, 7.93650793500350248e-04, 
    -2.777777777777681622553e-03, 8.333333333333333331554247e-02, 
    5.7083835261e-03
  )
}


trait LogGamma { 
  // From libbow, dirichlet.c
  // Written by Tom Minka <minka@stat.cmu.edu>
  def logGamma (xa:Double) : Double = {
    import LogGamma._
    var x = xa
    var result = 0.0; var y = 0.0; var xnum = 0.0; var xden = 0.0
    var i = 0
    val a = 0.6796875;

    if((x <= 0.5) || ((x > a) && (x <= 1.5))) {
      if(x <= 0.5) {
        result = -math.log(x);
        /*  Test whether X < machine epsilon. */
        if(x+1 == 1) {
          return result;
        }
      }
        else {
          result = 0;
          x = (x - 0.5) - 0.5;
        }
      xnum = 0;
      xden = 1;
      for(i <- 0 until 8) {
        xnum = xnum * x + p1(i);
        xden = xden * x + q1(i);
      }
      result += x*(d1 + x*(xnum/xden));
    }
        else if((x <= a) || ((x > 1.5) && (x <= 4))) {
          if(x <= a) {
            result = -math.log(x);
            x = (x - 0.5) - 0.5;
          }
          else {
            result = 0;
            x -= 2;
          }
          xnum = 0;
          xden = 1;
          for(i <- 0 until 8) {
            xnum = xnum * x + p2(i);
            xden = xden * x + q2(i);
          }
          result += x*(d2 + x*(xnum/xden));
        }
          else if(x <= 12) {
            x -= 4;
            xnum = 0;
            xden = -1;
            for(i <- 0 until 8) {
              xnum = xnum * x + p4(i);
              xden = xden * x + q4(i);
            }
            result = d4 + x*(xnum/xden);
          }
    /*  X > 12  */
          else {
            y = math.log(x);
            result = x*(y - 1) - y*0.5 + .9189385332046727417803297;
            x = 1/x;
            y = x*x;
            xnum = c(6);
            for(i <- 0 until 6) xnum = xnum * y + c(i);
            xnum *= x;
            result += xnum;
          }
    return result;
  }
  
  def logBeta(a:Double , b:Double) = logGamma(a)+logGamma(b)-logGamma(a+b)
  def beta(a:Double, b:Double) = math.exp(logBeta(a,b))
  def gamma (x:Double) = math.exp(logGamma(x))
}
