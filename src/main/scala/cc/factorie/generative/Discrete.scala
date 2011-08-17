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
import cc.factorie.la._
import scala.collection.mutable.{HashSet,ArrayBuffer}

trait GeneratedDiscreteVar extends DiscreteVar with GeneratedVar 
trait MutableGeneratedDiscreteVar extends GeneratedDiscreteVar with MutableGeneratedVar with MutableDiscreteVar
abstract class Discrete(initialInt: Int = 0) extends DiscreteVariable(initialInt) with MutableGeneratedDiscreteVar

trait GeneratedCategoricalVar[A] extends GeneratedDiscreteVar with CategoricalVar[A]
trait MutableGeneratedCategoricalVar[A] extends MutableGeneratedDiscreteVar with GeneratedCategoricalVar[A] with MutableCategoricalVar[A]
abstract class Categorical[A](initialValue:A) extends CategoricalVariable(initialValue) with MutableGeneratedCategoricalVar[A] 


trait DiscreteGeneratingFamily extends GenerativeFamily {
  type FamilyType <: DiscreteGeneratingFamily
  def prValue(s:StatisticsType, value:Int): Double
  def prValue(s:cc.factorie.Statistics, value:Int): Double = prValue(s.asInstanceOf[StatisticsType], value)
  def prValue(f:FactorType, value:Int): Double
  //def discreteChild(f:FactorType): GeneratedDiscreteVar
  // Nice idea if this following would work, but "extends super.Factor" is not dynamically bound by mixin order! :-(
  /*trait Factor extends super.Factor {
    def discreteGeneratingFamily = DiscreteGeneratingFamily.this
    //override def _1: GeneratedDiscreteVar
    def child = _1
    def prValue(s:StatisticsType, value:Int): Double = DiscreteGeneratingFamily.this.prValue(s, value)
    def prValue(value:Int): Double = DiscreteGeneratingFamily.this.prValue(this.asInstanceOf[FactorType], value)
  }*/
  //type StatisticsType <: Statistics
  /*trait Statistics extends super.Statistics {
    def prValue(value:Int): Double = DiscreteGeneratingFamily.this.prValue(this.asInstanceOf[StatisticsType], value)
  }*/
}

object Discrete extends DiscreteGeneratingFamily with GenerativeFamilyWithStatistics2[GeneratedDiscreteVar,Proportions] /*with DiscreteGeneratingFamily*/ {
  //override type StatisticsType = Stat
  //type FamilyType = Discrete
  def pr(s:StatisticsType) = s._2.apply(s._1.intValue)
  def prValue(s:StatisticsType, intValue:Int): Double = s._2.apply(intValue)
  def prValue(f:Factor, intValue:Int): Double = f._2.apply(intValue)
  def sampledValue(s:StatisticsType): DiscreteValue = s._1.domain.getValue(s._2.sampleInt)
  //def discreteChild(f:Factor) = f._1 
  //def maxIntValue(s:StatisticsType): Int = s._2.maxInt
  override def updateCollapsedParents(f:Factor, weight:Double): Boolean = {
    f._2 match {
      case p:DenseCountsProportions => { p.increment(f._1.intValue, weight)(null); true }
      case _ => false
    }
  }
  // TODO Arrange to call this in Factor construction.
  def factorHook(factor:Factor): Unit =
    if (factor._1.domain.size != factor._2.size) throw new Error("Discrete child domain size different from parent Proportions size.")
}

/*class Binomial(p:RealVarParameter, trials:Int) extends OrdinalVariable with GeneratedVariable {
  this := 0
}*/

/*
trait GenerativeStatisticsB extends Statistics {
  type ChildType <: GeneratedVar
  def pr: Double
  def logpr: Double = math.log(pr)
  def sampledChildValue: ChildType#Value
  def score = logpr
}

trait GenerativeFactorB extends Factor {
  type ChildType <: GeneratedVar
  def statistics: GenerativeStatisticsB
  def pr = statistics.pr
  def logpr = statistics.logpr
}

trait Statistics2B[A,B] extends Statistics {
  def _1:A
  def _2:B
}

trait Factor2B[N1<:Variable,N2<:Variable] extends Factor {
  //type StatisticsType <: GenerativeStatisticsB
  type NeighborType1 = N1
  type NeighborType2 = N2
  def _1: N1
  def _2: N2
  def numVariables = 2
  override def variables = IndexedSeq(_1, _2)
  def variable(i:Int) = i match { case 0 => _1; case 1 => _2; case _ => throw new IndexOutOfBoundsException(i.toString) }
  //type Values
  //def newValues(v1:N1#Value, v2:N2#Value): this.type#Values
}

trait Factor2c[N1<:Variable,N2<:Variable] extends Factor2B[N1,N2] {
  //def statistics(a:N1#Value = _1.value, b:N2#Value = _2.value): Statistics
  def statistics(a:N1#Value, b:N2#Value): Statistics
  def statistics = statistics(_1.value, _2.value)
}

trait FactorWithValues2c[This<:FactorWithValues2c[This,N1,N2],N1<:Variable,N2<:Variable] extends Factor2B[N1,N2] {
  self: This =>
  def statistics(v:Values): This#Statistics
  //def score(s:This#Statistics): Double
  def score(s:cc.factorie.Statistics): Double
  //def score(s:Statistics): Double
  //def score(s:this.type#Statistics): Double
  //def score(s:this.Statistics): Double
  def statistics: This#Statistics = values.statistics
  def values: Values = values(_1.value, _2.value)
  def values(a:N1#Value = _1.value, b:N2#Value = _2.value): Values = Values(a,b)
  case class Values(val _1:N1#Value, val _2:N2#Value) extends cc.factorie.Values with Statistics {
    override def statistics: This#Statistics = self.statistics(this)
  }
  trait Statistics extends cc.factorie.Statistics {
    def factor: This = self
    def score = self.score(this)
  }
}

class Factor2WithStatistics1(val _1:DiscreteVar, val _2:DiscreteVar) extends FactorWithValues2c[Factor2WithStatistics1,DiscreteVar,DiscreteVar] {
  def statistics(v:Values) = Statistics(3)
  //def score(s:Statistics) = 1.0
  //def score(s:this.type#Statistics) = 1.0
  def score(s:cc.factorie.Statistics): Double = 0.1
  def score(s:Factor2WithStatistics1.this.Statistics) = 1.0
  //def score(s:Statistics) = 1.0
  //def score(s:this.Statistics) = 3.0
  //def score(s:Factor2WithStatistics1#Statistics): Double = s.s
  def score(s:FactorWithValues2c[Factor2WithStatistics1,DiscreteVar,DiscreteVar]#Statistics): Double = 0.0
  def copy(s:cc.factorie.util.Substitutions): Factor = throw new Error
  case class Statistics(s:Double) extends super.Statistics
}

trait FactorWithStatistics2c[This<:FactorWithStatistics2c[This,N1,N2],N1<:Variable,N2<:Variable] extends FactorWithValues2c[This,N1,N2] {
  self: This =>
  def statistics(v:Values) = v
}

trait FactorWithStatistics2b[N1<:Variable,N2<:Variable] extends Factor2B[N1,N2] {
  self =>
  def pr(s:Statistics): Double
  def values = values(_1.value, _2.value)
  def values(a:N1#Value, b:N2#Value) = Statistics(a,b)
  def statistics = values
  protected def statistics(v:Values) = v
  protected def score(s:Statistics): Double
  type StatisticsType <: Statistics
  case class Statistics(_1:N1#Value, _2:N2#Value) extends cc.factorie.Values with Statistics2B[N1#Value,N2#Value] {
    def factor = null
    //def pr = self.pr(this)
    def score = self.score(this)
  }
}

trait FactorWithStatistics2[N1<:Variable,N2<:Variable] extends Factor2B[N1,N2] {
  type StatisticsType <: Statistics2B[N1#Value,N2#Value]
  type Values = this.type#Statistics
  def values = newValues(_1.value, _2.value)
  def newValues(v1:N1#Value, v2:N2#Value) = newStatistics(v1, v2)
  def statistics(v:Values) = v
  def newStatistics(v1:N1#Value, v2:N2#Value): this.type#Statistics
  abstract class Statistics(_1:N1#Value, _2:N2#Value) extends cc.factorie.Values with Statistics2B[N1#Value,N2#Value] {
    //def statistics = this
    def factor = null
    // Needs def score: Double
  }
}
trait Values {
  def statistics: cc.factorie.Statistics
}
trait Bactor {
  //type StatisticsType <: cc.factorie.Statistics
  def values: Values
  def statistics: Statistics //Type 
}
abstract case class Bactor2[N1<:Variable](_1:N1) extends Bactor {
  self =>
  type StatisticsType <: cc.factorie.Statistics
  final case class Values(_1:N1#Value) extends cc.factorie.generative.Values {
    def statistics: StatisticsType = self.statistics(this)
  }
  def values = Values(_1.value)
  def statistics: StatisticsType = statistics(values)
  def statistics(v:Values = values): StatisticsType
}
class BactorWithStatistics2[N1<:Variable](a:N1) extends Bactor2(a) {
  self =>
  type StatisticsType <: Statistics
  case class Statistics(_1:N1#Value) extends cc.factorie.Statistics {
    def score: Double = self.score(this) 
  }
  override def statistics(v:Values) = Statistics(v._1).asInstanceOf[StatisticsType]
  //def statistics: StatisticsType = statistics(values)
  def score(s:Statistics) = 0.0 // Not StatisticsType! because it needs to be concrete
}

object BooFactor extends BactorWithStatistics2(new BooleanVariable(true)) {
  type StatisticsType = BooStatistics
  class BooStatistics(a:BooleanValue) extends super.Statistics(a) {
    def boo(i:Int): Int = if (a.booleanValue) i else 0
  }
  override def statistics(v:Values) = new BooStatistics(v._1)
  //override def statistics = statistics(values)
}
object BooTest {
  val v = new BooFactor.Values(true)
  val s = BooFactor.statistics()
  val s2 = BooFactor.statistics(v)
  val s3 = v.statistics
  s3.boo(3)
  println(s)
}

trait Bamily {
  self =>
  type FamilyType = this.type
  type StatisticsType <: Statistics
  trait MyBactor extends Bactor {
    type StatisticsType = FamilyType#StatisticsType
    def family: FamilyType = self
    //def statistics: Statistics //FamilyType#StatisticsType
  }
  trait Statistics extends cc.factorie.Statistics {
    def family: FamilyType = self
  }
  //type StatisticsType <: Statistics
  //type Stat = FamilyType#StatisticsType
}
trait Bamily2[N1<:Variable] extends Bamily {
  self =>
  class Bactor(_1:N1) extends Bactor2[N1](_1) with super.MyBactor {
    def statistics(v:Values): StatisticsType = self.statistics(v)
    //def statistics: StatisticsType = statistics(values)
  }
  type Values = Bactor#Values
  def statistics(v:Values): StatisticsType
}
trait Statistics2[V1] extends Bamily {
  self =>
  type StatisticsType = Statistics
  case class Statistics(_1:V1) extends super.Statistics {
    def score = self.score(this)
  }
  def score(s:Statistics): Double
}
trait BamilyWithStatistics2[N1<:Variable] extends Bamily2[N1] with Statistics2[N1#Value]{
  def statistics(v:Bactor#Values) = Statistics(v._1)
}
trait MixtureBamily extends Bamily {
  //this: This =>
  //type FamilyType <: MixtureBamily
  //def prChoosing(s:FamilyType#StatisticsType, i:Int): Double
  def prChoosing(s:Statistics, i:Int): Double
  //def prBaz(f:MyBactor) = new { def pr(i:Int) = 0.0 }
  // You want to implement extra functionality 
  // which requires a Statistics or Factor argument
  // and you want to do this in an abstract Family trait
  // from which multiple concrete Families will inherit...
  // No way to have a type match on the passed in Statistics or Factor
}
object BamilyTest {
  def foo(f:MixtureBamily#MyBactor): Unit = {
    //f.family.prChoosing(f.statistics, 0)
  }
  
  object Bam extends BamilyWithStatistics2[BooleanVar] {
    def score(s:Statistics): Double = s._1.intValue
    def frob(f:Bactor): Unit = {}
    def bab(s:Statistics): Unit = {}
  }
  val f = new Bam.Bactor(new BooleanVariable(true))
  f.family.frob(f)
  val s = f.statistics()
  f.family.bab(s)
  val s2 = f.statistics(f.values)
  val s3 = Bam.statistics(f.values)
  println(s.score)
  println(Bam.score(s3))
  println(Bam.score(s2))
  println(Bam.score(s))
}

object Discrete3 {
  case class Factor(child:GeneratedDiscreteVar, proportions:Proportions) extends Factor2B[GeneratedDiscreteVar,Proportions] with GenerativeFactorB {
    type ChildType = GeneratedDiscreteVar
    final def _1 = child
    final def _2 = proportions
    type Values = Factor#Statistics
    def values: Factor#Statistics = statistics
    def statistics(v:Values) = v
    def statistics = new Statistics(_1.value, _2.value)
    def sampledChildValue = child.domain.getValue(proportions.value.sampleInt)
    def copy(s:cc.factorie.util.Substitutions): Factor = throw new Error
    case class Statistics(_1:DiscreteValue, _2:ProportionsValue) extends GenerativeStatisticsB {
      def sampledChildValue = throw new Error
      def pr = _2(_1.intValue)
    }
  }
}


object Discrete2 {
  case class Factor(child:GeneratedDiscreteVar, proportions:Proportions) extends FactorWithStatistics2[GeneratedDiscreteVar,Proportions] with GenerativeFactorB {
    type ChildType = GeneratedDiscreteVar
    final def _1 = child
    final def _2 = proportions
    type StatisticsType = Statistics
    //def statistics(v:Values) = new Statistics(v._1, v._2)
    override def newStatistics(v1:NeighborType1#Value, v2:NeighborType2#Value): Statistics = Statistics(v1, v2)
    def statistics: StatisticsType = newStatistics(_1.value, _2.value)
    def sampledChildValue = child.domain.getValue(proportions.value.sampleInt)
    def copy(s:cc.factorie.util.Substitutions): Factor = throw new Error
    case class Statistics(_1:DiscreteValue, _2:ProportionsValue) extends super.Statistics(_1, _2) with GenerativeStatisticsB {
      def sampledChildValue = throw new Error
      def pr = _2(_1.intValue)
    }
  }
}
*/

// The binary special case, for convenience
// TODO Rename this Boolean, inherit from BooleanVariable, and move it to a new file

/** The outcome of a coin flip, with boolean value.  */
class Flip(value:Boolean = false) extends BooleanVariable(value) with MutableGeneratedDiscreteVar 

/** A coin, with Multinomial distribution over outcomes, which are Flips. */
class Coin(p:Double) extends DenseProportions(Seq(1.0-p, p)) {
  def this() = this(0.5)
  assert (p >= 0.0 && p <= 1.0)
  def flip: Flip = { new Flip :~ Discrete(this) }
  def flip(n:Int) : Seq[Flip] = for (i <- 0 until n) yield flip
}
object Coin { 
  def apply(p:Double) = new Coin(p)
}
