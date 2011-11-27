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

package cc.factorie.generative
import cc.factorie._

trait GenerativeFactor extends Factor {
  type ChildType <: Variable
  type StatisticsType <: Statistics
  def statistics: StatisticsType
  def child: ChildType
  def parents: Seq[Variable]
  def pr(s:StatisticsType): Double
  def pr: Double = pr(statistics)
  def logpr(s:StatisticsType): Double = math.log(pr(s))
  def logpr: Double = logpr(statistics)
  def sampledValue(s:StatisticsType): Any
  def sampledValue: Any = sampledValue(statistics)
  // TODO Consider removing these methods because we'd have specialized code in the inference recipes.
  /** Update sufficient statistics in collapsed parents, using current value of child, with weight.  Return false on failure. */
  // TODO Consider passing a second argument which is the value of the child to use in the update
  def updateCollapsedParents(weight:Double): Boolean = throw new Error(factorName+": Collapsing parent not implemented.")
  def updateCollapsedChild(): Boolean = throw new Error(factorName+": Collapsing child not implemented.")
  def resetCollapsedChild(): Boolean = throw new Error(factorName+": Resetting child not implemented.")
}

trait RealGeneratingFactor extends GenerativeFactor {
  def sampleDouble: Double
  def pr(x:Double): Double
  def logpr(x:Double): Double
}

trait IntGeneratingFactor extends GenerativeFactor {
  def sampleInt: Int
  def pr(x:Int): Double
  def logpr(x:Int): Double
}

trait GenerativeFactorWithStatistics1[C<:Variable] extends GenerativeFactor with FactorWithStatistics1[C] {
  type ChildType = C
  def child = _1
  def parents: Seq[Variable] = Nil
  def score(s:Statistics) = logpr(s.asInstanceOf[StatisticsType]) // Can't define score earlier because inner class Factor.Statistics not defined until here
}

trait GenerativeFactorWithStatistics2[C<:Variable,P1<:Variable] extends GenerativeFactor with FactorWithStatistics2[C,P1] {
  type ChildType = C
  def child = _1
  def parents = Seq(_2)
  // TODO Consider this:
  //def parents = _2 match { case vars:Vars[Parameter] => vars; case _ => Seq(_2) }
  def score(s:Statistics) = logpr(s.asInstanceOf[StatisticsType])
}

trait GenerativeFactorWithStatistics3[C<:Variable,P1<:Variable,P2<:Variable] extends GenerativeFactor with FactorWithStatistics3[C,P1,P2] {
  type ChildType = C
  def child = _1
  def parents = Seq(_2, _3)
  def score(s:Statistics) = logpr(s.asInstanceOf[StatisticsType])
}

trait GenerativeFactorWithStatistics4[C<:Variable,P1<:Variable,P2<:Variable,P3<:Variable] extends GenerativeFactor with FactorWithStatistics4[C,P1,P2,P3] {
  type ChildType = C
  def child = _1
  def parents = Seq(_2, _3, _4)
  def score(s:Statistics) = logpr(s.asInstanceOf[StatisticsType])
}

trait GenerativeFamily1[Child<:Variable] {
  type C = Child
  trait Factor extends GenerativeFactorWithStatistics1[C] 
  def newFactor(c:C): Factor
  def apply() = new Function1[C,Factor] {
    def apply(c:C) = newFactor(c)
  }
}

trait GenerativeFamily2[Child<:Variable,Parent1<:Variable] {
  type C = Child
  type P1 = Parent1
  trait Factor extends GenerativeFactorWithStatistics2[C,P1] 
  def newFactor(c:C, p1:P1): Factor
  def apply(p1:P1) = new Function1[C,Factor] {
    def apply(c:C) = newFactor(c, p1)
  }
}

trait GenerativeFamily3[Child<:Variable,Parent1<:Variable,Parent2<:Variable] {
  type C = Child
  type P1 = Parent1
  type P2 = Parent2
  trait Factor extends GenerativeFactorWithStatistics3[C,P1,P2] 
  def newFactor(c:C, p1:P1, p2:P2): Factor
  def apply(p1:P1,p2:P2) = new Function1[C,Factor] {
    def apply(c:C) = newFactor(c, p1, p2)
  }
}

trait GenerativeFamily4[Child<:Variable,Parent1<:Variable,Parent2<:Variable,Parent3<:Variable] {
  type C = Child
  type P1 = Parent1
  type P2 = Parent2
  type P3 = Parent3
  trait Factor extends GenerativeFactorWithStatistics4[C,P1,P2,P3] 
  def newFactor(c:C, p1:P1, p2:P2, p3:P3): Factor
  def apply(p1:P1,p2:P2,p3:P3) = new Function1[C,Factor] {
    def apply(c:C) = newFactor(c, p1, p2, p3)
  }
}

