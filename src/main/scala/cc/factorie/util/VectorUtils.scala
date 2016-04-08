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

/**
 * @author John Sullivan
 */
object VectorUtils{
  implicit def doubleArrayToRichTensor(x:Array[Double]):ArrayVector = new ArrayVector(x)
  def randomArray(k:Int,random:scala.util.Random):Array[Double] = {
    val result = new Array[Double](k)
    var i = 0;while(i<result.length){result(i) = random.nextGaussian;i+=1}
    result
  }
  def zero(k:Int) = new Array[Double](k)
  def rep(v:Double,k:Int) = {val r=new Array[Double](k);var i=0;while(i<r.length){r(i)=v;i+=1};r}
}

class ArrayVector(val x:Array[Double]) extends AnyVal {
  import VectorUtils._//doubleArrayToRichTensor
  def copy():Array[Double] = {val r=new Array[Double](x.length);var i=0;while(i<x.length){r(i)=(i);i+=1};r}
  //basic arithmetic operations
  def + (y:Array[Double]):Array[Double] = {assert(x.length==y.length);val r=new Array[Double](x.length);var i=0;while(i<x.length){r(i)=x(i)+y(i);i+=1};r}
  def + (y:Double):Array[Double] = {val r=rep(y,x.length);var i=0;while(i<x.length){r(i)=r(i)+x(i);i+=1};r}
  def - (y:Double):Array[Double] = {val r=rep(y,x.length);var i=0;while(i<x.length){r(i)=r(i)-x(i);i+=1};r}
  def - (y:Array[Double]):Array[Double] = {assert(x.length==y.length);val r=new Array[Double](x.length);var i=0;while(i<x.length){r(i)=x(i)-y(i);i+=1};r}
  def * (y:Array[Double]):Array[Double] = {assert(x.length==y.length);val r=new Array[Double](x.length);var i=0;while(i<x.length){r(i)=x(i)*y(i);i+=1};r}
  def / (y:Array[Double]):Array[Double] = {assert(x.length==y.length);val r=new Array[Double](x.length);var i=0;while(i<x.length){r(i)=x(i)/y(i);i+=1};r}
  def * (y:Double):Array[Double] = {val r=new Array[Double](x.length);var i=0;while(i<x.length){r(i)=x(i)*y;i+=1};r}
  def / (y:Double):Array[Double] = {val r=new Array[Double](x.length);var i=0;while(i<x.length){r(i)=x(i)/y;i+=1};r}
  //modifiers
  def *= (y:Double){var i=0;while(i<x.length){x(i) *= y;i+=1}}
  def /= (y:Double){var i=0;while(i<x.length){x(i) /= y;i+=1}}
  def *= (y:Array[Double]){assert(x.length==y.length);var i=0;while(i<x.length){x(i) *= y(i);i+=1}}
  def /= (y:Array[Double]){assert(x.length==y.length);var i=0;while(i<x.length){x(i) /= y(i);i+=1}}
  def += (y:Array[Double],scalar:Double){assert(x.length==y.length);var i=0;while(i<x.length){x(i) += y(i)*scalar;i+=1}}
  def += (y:Array[Double]){assert(x.length==y.length);var i=0;while(i<x.length){x(i) += y(i);i+=1}}
  def -= (y:Array[Double]){this += (y,-1.0)}
  def -= (y:Array[Double],scalar:Double){this += (y,-scalar)}
  //norms
  def oneNorm():Double = {var r=0.0;var i=0;while(i<x.length){r+=scala.math.abs(x(i));i+=1};r}
  def twoNorm():Double = {var r=0.0;var i=0;while(i<x.length){r+=x(i)*x(i);i+=1};math.sqrt(r)}
  //distances/similarities
  def dot(y:Array[Double]):Double = {assert(x.length==y.length);var r=0.0;var i=0;while(i<x.length){r+=x(i)*y(i);i+=1};r}
  def cosineSimilarity(that:Array[Double]):Double = {val thisDotThat=this dot that;if(thisDotThat==0.0)0.0 else thisDotThat/(this.twoNorm*that.twoNorm())}
  def cosineSimilarityWithParent(parent:Array[Double]):Double = {
    var thisTwoNormSquared = twoNorm();thisTwoNormSquared*=thisTwoNormSquared
    var thatTwoNormSquared = parent.twoNorm();thatTwoNormSquared*=thatTwoNormSquared
    val thisDotThat = this dot parent
    if(math.abs(thisDotThat-thisTwoNormSquared)<=0.000001) 0.0 else (thisDotThat - thisTwoNormSquared)/(math.sqrt(thisTwoNormSquared)*math.sqrt(thatTwoNormSquared+thisTwoNormSquared-2.0*thisDotThat))
  }
  def hasNaN = {var r=false;var i=0;while(i<x.length && !r){if(x != x)r=true;i+=1};r}//(x.exists(e=> !(e == e)))
  def twoDistance(that:Array[Double]):Double = (this - that).twoNorm()
  def totalVariation(that:Array[Double]):Double = 0.5*(this - that).oneNorm()
  //noise
  def makeCorruptObservation(noiseSource:Array[Double]=>Unit):Array[Double] = {val r=copy();noiseSource(r);r}
  def corrupt(sigma:Double,random:scala.util.Random){var i=0;while(i<x.length){x(i)+=random.nextGaussian()*sigma;i+=1}}
  def corrupt(sigma:Array[Double],random:scala.util.Random){var i=0;while(i<x.length){x(i)+=random.nextGaussian()*sigma(i);i+=1}}
  //other
  //def pAsGauss(sigma:Double) =
  def compression(setSize:Int) = entropyForLogValues * setSize.toDouble
  def mean:Double ={var result=0.0;var i=0;while(i<x.length){result+=x(i);i+=1};result}
  //def stddev:Double = (x - mean).twoDistance
  //def variance:Double = {val s=stddev;s*s}
  def variance = {var r=0.0;val m = mean;var i=0;while(i<0){r+=(x(i)-m)*(x(i)-m);i+=1};r}
  def stddev = math.sqrt(variance)
  def entropyForLogValues:Double ={
    var h = 0.0
    val z = cc.factorie.maths.sumLogProbs(x)
    var i=0
    while(i<x.length){
      val logp = x(i)-z
      h += math.exp(logp)*logp
      i+=1
    }
    h
  }
  def normalizedEntropyForLogValues:Double = -entropyForLogValues/math.log(x.length)
  //def entropy:Double = {var result = 0.0;val z = oneNorm;var i=0;while(i<x.length){result += x(i)*math.log(x(i)/z);i+=1;result/z}}
}

