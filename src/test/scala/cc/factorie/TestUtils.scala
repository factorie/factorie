
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

package cc.factorie

trait TestUtils {
  import scala.collection.Seq

  /**
   * Return all combinations of n elements from
   * given sequence w/o replacement
   */
  def chooseN[T](seq:Seq[T], n:Int):Seq[Seq[T]] = { 
    // Helpers: 
    //   [(a, 1), (b, 2), ...] -> [a, b, ..]
    def removeZipIndex[T](ss:Seq[(T, Int)]) = ss.unzip._1
    //   [(a, 0), (b, 3), (c, 23)] -> 23
    def lastElemZipIndex[T](s:Seq[(T, Int)]) = s.last._2
    //   [(a, 0), (b, 1)] -> [a, b, c, d] -> [c, d]
    def remainingSeq[T](s:Seq[(T, Int)], seq:Seq[(T, Int)]) = seq.view(lastElemZipIndex(s)+1, seq.length)

    def choose[T](n:Int, seq:Seq[(T, Int)]):Seq[Seq[(T, Int)]] = n match {
      case 0 => Nil
      case 1 => seq.map(_ :: Nil)
      case i:Int => choose(i-1, seq) flatMap { 
        ll => remainingSeq(ll, seq).map { e => ll :+ e }}
    }
    choose(n, seq.zipWithIndex) map removeZipIndex _
  }
}
