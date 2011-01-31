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

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer, FlatHashTable}
import scala.util.{Random,Sorting}
import java.io.{File,PrintStream,FileOutputStream,PrintWriter,FileReader,FileWriter,BufferedReader}
import cc.factorie.la._
import cc.factorie.util.Substitutions

object Template {
  var enableCachedStatistics: Boolean = true
}

// Factor Templates, which create factors in a factor graph on-the-fly as necessary.
// A factor template specifies
// (1) a description of the arbitrary relationship among its variable neighbors
// (2) a sufficient statistics function that maps those neighbors to the statistics necessary to return a real-valued score
// (3) an aggregator for multiple statistics of the same template
// (4) a function mapping those aggregated statistics to a real-valued score
// (5) optionally, the parameters used in the function to calculate that score;
//     (alternatively the score may be calculated using parameter stored externally to the Template,
//      or in some fixed way without learned parameters).

/** A single factor in a factor graph.  In other words, a factor
    template packaged with a set of variables neighboring the
    factor.  
    @author Andrew McCallum */
trait Factor extends Ordered[Factor] {
  def template: Template
  def numVariables: Int 
  def variable(index: Int): Variable
  def statistics: Statistics
  /** Optionally return pre-calculated Statistics.  By default not actually cached, but may be overridden in subclasses. */
  def cachedStatistics: Statistics
  def score: Double = statistics.score
  def variables: IndexedSeq[Variable] = { val result = new ArrayBuffer[Variable](numVariables); for (i <- 0 until numVariables) result += variable(i); result }
  def randomVariable(implicit random:Random = cc.factorie.random): Variable = variable(random.nextInt(numVariables))
  def copy(s:Substitutions): Factor
  // Implement Ordered, such that worst (lowest) scores are considered "high"
  def compare(that: Factor) = {val d = that.score - this.score; if (d > 0.0) 1 else if (d < 0.0) -1 else 0}
  // Implement equality based on class assignability and Variable contents equality
  //override def canEqual(other: Any) = (null != other) && other.isInstanceOf[Factor]; // TODO Consider putting this back in
  override def equals(other: Any): Boolean = other match {
    case other:Factor => 
      (this eq other) || ((this.template eq other.template)
                          && forallIndex(numVariables)(i =>
        (this.variable(i) eq other.variable(i)) ||
                (this.variable(i).isInstanceOf[Vars[_]] && this.variable(i) == other.variable(i))))
    case _ => false
  }
  var _hashCode = -1
  override def hashCode: Int = {
    if (_hashCode == -1) {
      _hashCode = getClass.hashCode
      forIndex(numVariables)(i => { val v = variable(i); _hashCode += 31*i + (if (v eq null) 0 else v.hashCode) })
    }
    _hashCode
  }
  def factorName = template.factorName
  override def toString: String = variables.mkString(factorName+"(", ",", ")")
}

/** A container for all the values of the variables neighboring a factor.
    These are necessary to construct a Statistics object. */
trait Values /* extends Product with Ordered[Values] */ {
  def template: Template
  def statistics: Statistics
  def score: Double = statistics.score
  //def productArity: Int
  //def canEqual(other:Any) = other match { case other:Values => }
}

/** A summary of all the statistics of a Factor */
trait Statistics {
  def template: Template
  def score: Double
}

/** A container for sufficient statistics of a Factor.  Note that one Factor can result in more than one of these Stat objects. */
trait Stat extends Statistics

/** A collection of Stat objects along with a method of producting a compatibility score from them. */
trait Stats extends Statistics {
  def stats: Iterable[Stat]
}



/** The template for many factors.  Manages its connections to neighboring variables.
    @Andrew McCallum
*/
trait Template { thisTemplate =>
  type TemplateType <: Template // like a self-type
  type FactorType <: Factor
  type StatType <: Stat
  type ValuesType <: Values
  type StatisticsType <: Statistics
  protected var _neighborDomains: ArrayBuffer[Domain[_]] = null
  protected def _newNeighborDomains = new ArrayBuffer[Domain[_]]
  def neighborDomains: Seq[Domain[_]] = 
    if (_neighborDomains eq null)
      throw new IllegalStateException("You must override neighborDomains if you want to access them before creating any Factor objects.")
    else
      _neighborDomains
  /** The method responsible for mapping a Statistic object to a real-valued score.  
      Called by the Statistic.score method; implemented here so that it can be easily overriden in user-defined subclasses of Template. */
  def score(s:StatType): Double
  trait Factor extends cc.factorie.Factor { 
    override def template: TemplateType = Template.this.asInstanceOf[TemplateType];
    override def statistics: StatisticsType
    override def cachedStatistics: StatisticsType
    def forSettingsOf(vs:Seq[Variable])(f: =>Unit): Unit = thisTemplate.forSettingsOf(this.asInstanceOf[FactorType], vs)(f)
    //def stats: Iterable[StatType]
  }
  /** Used by the trickery that obtains Manifests for Statistics*[] traits.  
      See template2initialized in Package.scala. */
  def defaultFactorName = "Factor"
  var factorName = defaultFactorName
  /** Assign this Template a name which will be used later when its factors are printed. */
  def setName(n:String): this.type = { factorName = n; this }
  /** Assign this Template a name which will be used later when its factors are printed. */
  def %(n:String): this.type = setName(n) // because % is the comment character in shell languages such as /bin/sh and Makefiles.
  /**A version of factors that takes the Diff object instead of just the variable */
  def factors(d: Diff): Iterable[FactorType] = if (d.variable == null) Nil else factors(d.variable)
  def factors(v: Variable): Iterable[FactorType]
  def factors(difflist: DiffList): Iterable[FactorType] = {
    //var result = new LinkedHashSet[Factor]()
    var result = new HashSet[FactorType]()
    for (diff <- difflist; factor <- factors(diff)) { if (factor eq null) throw new Error("unroll returned null Factor") else result += factor } 
    //difflist.foreach(diff => result ++= factors(diff))
    result.toList // TODO is this necessary?
  }
  def factors(variables:Iterable[Variable]): Iterable[FactorType] = {
    if (variables.size == 1) return factors(variables.head) // Efficiently avoids the HashSet.
    //var result = new LinkedHashSet[FactorType]()
    var result = new HashSet[FactorType]()
    for (v <- variables; factor <- factors(v)) { if (factor eq null) throw new Error("unroll returned null Factor") else result += factor }
    result.toSeq // TODO is this necessary?
  }
  /** Called in implementations of factors(Variable) to give the variable a chance 
      to specify additional dependent variables on which factors(Variable) should also be called. */
  def unrollCascade(v:Variable): Iterable[Variable] = v.unrollCascade
  // Values
  trait Values extends cc.factorie.Values {
    override def template: TemplateType = Template.this.asInstanceOf[TemplateType]
  }
  // Statistics
  trait Statistics extends cc.factorie.Statistics {
    override def template: TemplateType = Template.this.asInstanceOf[TemplateType];
  }
  trait Stat extends cc.factorie.Stat with Statistics {
    override def template: TemplateType = Template.this.asInstanceOf[TemplateType];
    // TODO Make this non-lazy later, when _statisticsDomains can be initialized earlier
    // Warning: if score gets called too late, might the values of the variables have been changed to something else already?
    lazy val score = Template.this.score(this.asInstanceOf[StatType]) // TODO can we find a way to get rid of this cast?  Yes, use a self-type Stat[This]
  }
  class Stats(val stats:Iterable[StatType]) extends cc.factorie.Stats with Statistics /*with Iterable[StatType]*/ {
    //def iterator = stats.iterator
    override def template: TemplateType = Template.this.asInstanceOf[TemplateType];
    val score = stats.foldLeft(0.0)(_ + Template.this.score(_))
  }
  def statistics(values:ValuesType): StatisticsType
  // TODO Is this next method actually used?
  def statistics(ss:Iterable[StatType]): StatisticsType = (new Stats(ss)).asInstanceOf[StatisticsType] // TODO How can we get rid of this cast? // TODO Is this method still used?
  /** May be overridden in subclasses to actually cache. */
  def cachedStatistics(values:ValuesType): StatisticsType = statistics(values)
  def clearCachedStatistics: Unit = {}
  //def statistics(v:Variable): StatisticsType = new Stats(factors(v).map(_.stats).flatten)
  /** To allow users' "def statistics(v1)" to return an Iterator[Stat] */
  implicit def iterableStatToStatistics[S<:StatType](ss:Iterable[S]): StatisticsType = statistics(ss)
  //def statistic(ss:Iterable[StatType]): StatisticType = new Statistic(ss).asInstanceOf[StatisticType] // TODO is there some way to avoid this cast?
  // Managing settings iteration
  // TODO Replace this with message calculation code that does the settings iteration internally
  def hasSettingsIterator: Boolean = false
  def forSettings(factor:FactorType)(f: =>Unit): Unit = throw new Error("Not supported.")
  def forSettingsOf(factor:FactorType, vs:Seq[Variable])(f: =>Unit): Unit = throw new Error("Not supported.")
  def sparsifySettingsFor(vs:Iterable[Variable]): Unit = throw new Error("Not supported.")
  // New version of settings iteration
  def forSettingStats(factor:FactorType, vs:Seq[Variable])(f: (StatisticsType)=>Unit): Unit = throw new Error("Not supported.") // TODO Also pass variable values?
  /** The filename into which to save this factor.  If factorName is not the default, use it, otherwise use the class name. */
  protected def filename: String = if (factorName != defaultFactorName) factorName else this.getClass.getName
  def save(dirname:String): Unit = {}
  def load(dirname:String): Unit = {}
}

/** A Template whose sufficient statistics are represented as a set of DiscretesValues
    (which inherit from cc.factorie.la.Vector, and also have a DiscretesDomain).
    @author Andrew McCallum
*/
trait VectorTemplate extends Template {
  //def vectorLength: Int
  //protected var _vectorLength1 = -1
  //def vectorLength1: Int = if (_vectorLength < 0) throw new Error("Not yet set.") else _vectorLength1
  protected var _statisticsDomains: ArrayBuffer[DiscretesDomain] = null
  protected def _newStatisticsDomains = new ArrayBuffer[DiscretesDomain]
  def statisticsDomains: Seq[DiscretesDomain] = 
    if (_statisticsDomains eq null)
      throw new IllegalStateException("You must override statisticsDomains if you want to access them before creating any Factor and Stat objects.")
    else
      _statisticsDomains
  def freezeDomains: Unit = statisticsDomains.foreach(_.freeze)
  lazy val statisticsVectorLength: Int = statisticsDomains.multiplyInts(_.dimensionSize)
  type StatisticsType <: Statistics
  trait Statistics extends super.Statistics {
    def vector: Vector
  }
  type StatType <: Stat
  trait Stat extends super.Stat with Statistics {
    def vector: Vector
  }
  class Stats(ss:Iterable[_<:StatType]) extends super.Stats(ss) with Statistics {
    val vector: Vector = {
      val iter = ss.iterator
      if (iter.hasNext) {
        val first: Vector = iter.next.vector
        if (!iter.hasNext) // There is only one there
          first
        else {
          var vec = new SparseVector(first.size) // TODO if 'first' is SparseBinaryVector this should be Sparse also
          vec += first
          while (iter.hasNext) vec += iter.next.vector
          vec
        } 
      } else { // an empty iterator over Suff's.  Just return a (sparse) vector of zero's.
        new SparseVector(statisticsVectorLength)
      }
    }
  }
  override def statistics(ss:Iterable[StatType]): StatisticsType = (new Stats(ss)).asInstanceOf[StatisticsType] // TODO How can we get rid of this cast?
  //override def statistics(v:Variable): StatisticsType = new Stats(factors(v).map(_.stats).flatten)
  //override def statistic(ss:Iterable[StatType]): StatisticType = new Statistic(ss)
  // TODO implement this!
  private def unflattenOuter(weightIndex:Int, dimensions:Int*): Array[Int] = new Array[Int](2) 
} // end of VectorTemplate


/** A VectorTemplate that also has a vector of weights, and calculates score by a dot-product between statistics.vector and weights. 
    @author Andrew McCallum */
trait DotTemplate extends VectorTemplate {
  type TemplateType <: DotTemplate
  lazy val weights: Vector = { freezeDomains; new DenseVector(statisticsVectorLength) } // Dense by default, may be override in sub-traits
  def score(s:StatType) = weights match {
    case w:DenseVector => { w dot s.vector }
    //case w:SparseHashVector => w dot s.vector // TODO Uncomment this.  It was only commented because latest version of scalala didn't seem to have this class any more
    case w:SparseVector => w dot s.vector
  }
  override def save(dirname:String): Unit = {
    val f = new File(dirname+"/"+filename) // TODO Make this work on MSWindows also
    if (f.exists) return // Already exists, don't write it again
    for (d <- statisticsDomains) d.save(dirname)
    val s = new PrintWriter(new FileWriter(f))
    // TODO Do we also need to save the weights.default?
    for (weight <- weights.activeElements; if (weight._2 != 0.0)) { // before June 21 2010, used too be weights.iterator -akm
      s.print(weight._1)
      s.print(" ")
      s.println(weight._2)
    }
    s.close
  }
  override def load(dirname:String): Unit = {
    //println("Loading "+this.getClass.getName+" from directory "+dirname)
    for (d <- statisticsDomains) { /* println(" Loading Domain["+d+"]"); */ d.load(dirname) }
    // TODO Why would statisticsVectorLength be 0 or negative?
    if (statisticsVectorLength <= 0 || weights.activeElements.exists({case(i,v) => v != 0})) return // Already have non-zero weights, must already be read.
    val f = new File(dirname+"/"+filename)
    val s = new BufferedReader(new FileReader(f))
    var line = ""
    while ({ line = s.readLine; line != null }) {
      val fields = line.split(" +")
      assert(fields.length == 2)
      val index = Integer.parseInt(fields(0))
      val value = java.lang.Double.parseDouble(fields(1))
      weights(index) = value
    }
    s.close
  }
}

/** A DotTemplate that stores its parameters in a Scalala SparseVector instead of a DenseVector 
    @author Andrew McCallum */
trait SparseWeights extends DotTemplate {
  override lazy val weights: Vector = { new SparseVector(statisticsVectorLength) } // Dense by default, here overridden to be sparse
}

/** A DotTemplate that stores its parameters in a Scalala SparseHashVector instead of a DenseVector 
    @author Sameer Singh */
trait SparseHashWeights extends DotTemplate {
  override lazy val weights: Vector = { freezeDomains; new SparseHashVector(statisticsVectorLength) } // Dense by default, override to be sparseHashed
}


abstract class Template1[N1<:Variable](implicit nm1: Manifest[N1]) extends Template {
  type Neighbor1Type = N1
  val nc1 = nm1.erasure // "Neighbor class" // TODO Give this a more explanatory name
  val nc1a = { val ta = nm1.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(nc1)) { assert(ta.length == 1); ta.head.erasure } else null }
  lazy val nd1: Domain[N1#Value] = null // Can be overriden if necessary // TODO Consider removing this.
  // Factors
  def factors(v:Variable): Iterable[FactorType] = {
    // TODO Given the surprise about how slow Manifest <:< was, I wonder how slow this is when there are lots of traits!
    // When I substituted "isAssignable" for HashMap caching in GenericSampler I got 42.8 versus 44.4 seconds ~ 3.7%  Perhaps worth considering?
    var ret = new ListBuffer[FactorType]
    if (nc1.isAssignableFrom(v.getClass)) ret ++= unroll1(v.asInstanceOf[N1])
    if ((nc1a ne null) && nc1a.isAssignableFrom(v.getClass)) ret ++= unroll1s(v.asInstanceOf[N1#ContainedVariableType])
    // TODO It would be so easy for the user to define Variable.unrollCascade to cause infinite recursion.  Can we make better checks for this?
    val cascadeVariables = unrollCascade(v); if (cascadeVariables.size > 0) ret ++= cascadeVariables.flatMap(factors(_))
    ret
  }
  def unroll1(v:N1): Iterable[FactorType] = new Factor(v)
  def unroll1s(v:N1#ContainedVariableType): Iterable[FactorType] = throw new Error("You must override unroll1s.")
  type FactorType = Factor
  final case class Factor(_1:N1) extends super.Factor {
    if (_neighborDomains eq null) {
      _neighborDomains = _newNeighborDomains
      _neighborDomains += _1.domain
    }
    def numVariables = 1
    def variable(i:Int) = i match { case 0 => _1; case _ => throw new IndexOutOfBoundsException(i.toString) }
    override lazy val variables: IndexedSeq[Variable] = IndexedSeq(_1)
    def values: ValuesType = new Values(_1.value)
    def statistics: StatisticsType = Template1.this.statistics(values)
    override def cachedStatistics: StatisticsType = Template1.this.cachedStatistics(values)
    // Note.  If someone subclasses Factor, then you might not get that subclass!
    def copy(s:Substitutions) = Factor(s.sub(_1))
  } 
  // Values
  type ValuesType = Values
  final case class Values(_1:N1#Value) extends super.Values {
    def statistics = Template1.this.statistics(this)
  }
  // Statistics
  def statistics(values:Values): StatisticsType
  //def stats(v:Variable): Iterable[StatisticsType] = factors(v).map(_.statistics) // TODO Do we need to consider a flatMap here?
  private var cachedStatisticsArray: Array[StatisticsType] = null
  override def cachedStatistics(vals:Values): StatisticsType = if (Template.enableCachedStatistics) {
    vals._1 match {
    case v:DiscreteValue => {
      if (cachedStatisticsArray eq null) cachedStatisticsArray = new Array[Statistics](v.domain.size).asInstanceOf[Array[StatisticsType]]
      val i = v.intValue
      if (cachedStatisticsArray(i) eq null) cachedStatisticsArray(i) = statistics(vals)
      cachedStatisticsArray(i)
    }
    case _ => statistics(vals)
  }} else statistics(vals)
  /** You must clear cache the cache if DotTemplate.weights change! */
  override def clearCachedStatistics: Unit =  cachedStatisticsArray = null
}
trait Statistics1[S1] extends Template {
  final case class Stat(_1:S1) extends super.Stat
  type StatType = Stat
  type StatisticsType = Statistics
}
trait VectorStatistics1[S1<:DiscretesValue] extends VectorTemplate {
  type StatType = Stat
  type StatisticsType = Statistics
  // Use Scala's "pre-initialized fields" syntax because super.Stat needs vector to initialize score
  final case class Stat(_1:S1) extends { val vector: Vector = _1 } with super.Stat { 
    if (_statisticsDomains eq null) {
      _statisticsDomains = _newStatisticsDomains
      _statisticsDomains += _1.domain
    }
  }
}
trait DotStatistics1[S1<:DiscretesValue] extends VectorStatistics1[S1] with DotTemplate {
  def setWeight(entry:S1, w:Double) = entry match {
    case d:DiscreteValue => weights(d.intValue) = w
    case ds:DiscretesValue => ds.activeDomain.foreach(i => weights(i) = w)
  }
}
abstract class TemplateWithStatistics1[N1<:Variable](implicit nm1:Manifest[N1]) extends Template1[N1] with Statistics1[N1#Value] {
//abstract class TemplateWithStatistics1[N1<:Variable](implicit nm1:Manifest[N1]) extends Template1[N1]()(nm1) with Statistics1[N1]
  def statistics(vals:Values): StatType = Stat(vals._1)
}
abstract class TemplateWithVectorStatistics1[N1<:DiscretesVar](implicit nm1:Manifest[N1]) extends Template1[N1] with VectorStatistics1[N1#Value] {
//abstract class TemplateWithVectorStatistics1[N1<:VectorVar](implicit nm1:Manifest[N1]) extends Template1[N1]()(nm1) with VectorStatistics1[N1]
  def statistics(vals:Values): StatType = Stat(vals._1)
  //init //(nm1)
}
class TemplateWithDotStatistics1[N1<:DiscretesVar](implicit nm1:Manifest[N1]) extends Template1[N1] with DotStatistics1[N1#Value] {
//class TemplateWithDotStatistics1[N1<:VectorVar](implicit nm1:Manifest[N1]) extends Template1[N1]()(nm1) with DotStatistics1[N1] 
  def statistics(vals:Values): StatType = Stat(vals._1)
  //init //(nm1)
}

/*
trait DiscreteFactorSettings1 extends Template {
  this: VectorTemplate { type Neighbor1Type <: DiscreteVar; type FactorType <: { def _1:DiscreteVariable } } =>
  val ndd1: DiscreteDomain = throw new Error // TODO nd1.asInstanceOf[DiscreteDomain[DiscreteVar]]
  val nds1 = ndd1.size
  // Managing settings iteration
  override def hasSettingsIterator: Boolean = true
  override def forSettings(factor:FactorType)(f: =>Unit): Unit = 
    if (settingsSparsified && (sparseSettingsValues ne null))
      forIndex(sparseSettingsValues.length)(i => { factor._1.set(sparseSettingsValues(i))(null); f })
    else
      forIndex(nds1)(i => { factor._1.set(i)(null); f })
  override def forSettingsOf(factor:FactorType, vs:Seq[Variable])(f: =>Unit): Unit = { require(vs.size == 1); require(factor._1 == vs.head) }
  def forSettingsExcept(factor:FactorType, v:Variable)(f: =>Unit): Unit = require(factor._1 == v)

  private var settingsSparsified = false
  private var sparseSettingsValues: Array[Int] = null
  override def sparsifySettingsFor(vs:Iterable[Variable]): Unit = {
    val sparseInts = new HashSet[Int]
    // Only works for DiscreteVar
    vs.foreach(_ match { case v:DiscreteVar => sparseInts += v.intValue })
    sparseSettingsValues = sparseInts.toArray
    settingsSparsified = true 
  }
}
*/

abstract class Template2[N1<:Variable,N2<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2]) extends Template {
  type Neighbor1Type = N1
  type Neighbor2Type = N2
  val nc1 = nm1.erasure
  val nc2 = nm2.erasure
  val nc1a = { val ta = nm1.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(nc1)) { assert(ta.length == 1); ta.head.erasure } else null }
  val nc2a = { val ta = nm2.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(nc2)) { assert(ta.length == 1); ta.head.erasure } else null }
  lazy val nd1: Domain[N1#Value] = throw new Error // TODO Domain.get[Variable](nc1).asInstanceOf[Domain[Variable]]
  lazy val nd2: Domain[N2#Value] = throw new Error // TODO Domain.get[Variable](nc2).asInstanceOf[Domain[Variable]]
  override def factors(v: Variable): Iterable[FactorType] = {
    var ret = new ListBuffer[FactorType]
    if (nc1.isAssignableFrom(v.getClass)) ret ++= unroll1(v.asInstanceOf[N1]) 
    if (nc2.isAssignableFrom(v.getClass)) ret ++= unroll2(v.asInstanceOf[N2]) 
    if ((nc1a ne null) && nc1a.isAssignableFrom(v.getClass)) ret ++= unroll1s(v.asInstanceOf[N1#ContainedVariableType])
    if ((nc2a ne null) && nc2a.isAssignableFrom(v.getClass)) ret ++= unroll2s(v.asInstanceOf[N2#ContainedVariableType])
    val cascadeVariables = unrollCascade(v); if (cascadeVariables.size > 0) ret ++= cascadeVariables.flatMap(factors(_))
    ret
  }
  def unroll1(v:N1): Iterable[FactorType] 
  def unroll2(v:N2): Iterable[FactorType]
  def unroll1s(v:N1#ContainedVariableType): Iterable[FactorType] = throw new Error("You must override unroll1s.")
  def unroll2s(v:N2#ContainedVariableType): Iterable[FactorType] = throw new Error("You must override unroll2s.")
  type FactorType = Factor
  final case class Factor(_1:N1, _2:N2) extends super.Factor {
    if (_neighborDomains eq null) {
      _neighborDomains = _newNeighborDomains
      _neighborDomains += _1.domain
      _neighborDomains += _2.domain
    }
    def numVariables = 2
    def variable(i:Int) = i match { case 0 => _1; case 1 => _2; case _ => throw new IndexOutOfBoundsException(i.toString) }
    def values: ValuesType = new Values(_1.value, _2.value)
    def statistics: StatisticsType = Template2.this.statistics(values)
    override def cachedStatistics: StatisticsType = Template2.this.cachedStatistics(values)
    def copy(s:Substitutions) = Factor(s.sub(_1), s.sub(_2))
  }
  // Values
  type ValuesType = Values
  final case class Values(_1:N1#Value, _2:N2#Value) extends super.Values {
    def statistics = Template2.this.statistics(this)
  }
  // Statistics
  def statistics(values:Values): StatisticsType
  //def stats(v:Variable) = factors(v).flatMap(_statistics(_))
  private var cachedStatisticsArray: Array[StatisticsType] = null
  private var cachedStatisticsHash: HashMap[Product,StatisticsType] = null
  /** It is callers responsibility to clearCachedStatistics if weights or other relevant state changes. */
  override def cachedStatistics(values:Values): StatisticsType = if (Template.enableCachedStatistics) values._1 match {
    case v1:DiscreteValue => { 
      values._2 match {
        case v2:DiscreteValue => {
          //println("Template2.cachedStatistics")
          if (cachedStatisticsArray eq null) cachedStatisticsArray = new Array[Statistics](v1.domain.size * v2.domain.size).asInstanceOf[Array[StatisticsType]]
          val i = v1.intValue * nd2.asInstanceOf[DiscretesDomain].dimensionSize + v2.intValue
          if (cachedStatisticsArray(i) eq null) cachedStatisticsArray(i) = statistics(values)
          cachedStatisticsArray(i)
        }
        case v2:DiscretesValue if (true /*v2.isConstant*/) => {
          //println("Template2.cachedStatistics")
          if (cachedStatisticsHash eq null) cachedStatisticsHash = new HashMap[Product,StatisticsType] { override protected def initialSize = 512 }
          val i = ((v1.intValue,v2))
          cachedStatisticsHash.getOrElseUpdate(i, statistics(values))
        }
        case _ => statistics(values)
      }
    }
    case v1:DiscretesValue if (true /*v1.isConstant*/) => { 
      values._2 match {
        case v2:DiscreteValue => {
          if (cachedStatisticsHash eq null) cachedStatisticsHash = new HashMap[Product,StatisticsType]
          val i = ((v2.intValue,v1))
          cachedStatisticsHash.getOrElseUpdate(i, statistics(values))
        }
        case _ => statistics(values)
      }
    }
    case _ => statistics(values)
  } else statistics(values)
  override def clearCachedStatistics: Unit =  { cachedStatisticsArray = null; cachedStatisticsHash = null }
}
trait Statistics2[S1,S2] extends Template {
  final case class Stat(_1:S1, _2:S2) extends super.Stat
  type StatType = Stat
  type StatisticsType = Statistics
}
trait VectorStatistics2[S1<:DiscretesValue,S2<:DiscretesValue] extends VectorTemplate {
  type StatType = Stat
  type StatisticsType = Statistics
  final case class Stat(_1:S1, _2:S2) extends { val vector: Vector = _1 flatOuter _2 } with super.Stat { 
    if (_statisticsDomains eq null) { 
      _statisticsDomains = _newStatisticsDomains
      _statisticsDomains += _1.domain
      _statisticsDomains += _2.domain
    }
  }
}
trait DotStatistics2[S1<:DiscretesValue,S2<:DiscretesValue] extends VectorStatistics2[S1,S2] with DotTemplate
abstract class TemplateWithStatistics2[N1<:Variable,N2<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2]) extends Template2[N1,N2] with Statistics2[N1#Value,N2#Value] {
  def statistics(values:Values): StatType = Stat(values._1, values._2)
}
abstract class TemplateWithVectorStatistics2[N1<:DiscretesVar,N2<:DiscretesVar](implicit nm1:Manifest[N1], nm2:Manifest[N2]) extends Template2[N1,N2] with VectorStatistics2[N1#Value,N2#Value] {
  def statistics(values:Values): StatType = Stat(values._1, values._2)
  //init(nm1, nm2)
}
abstract class TemplateWithDotStatistics2[N1<:DiscretesVar,N2<:DiscretesVar](implicit nm1:Manifest[N1], nm2:Manifest[N2]) extends Template2[N1,N2] with DotStatistics2[N1#Value,N2#Value] {
  def statistics(values:Values): StatType = Stat(values._1, values._2)
  //init(nm1, nm2)
}

/*
trait DiscreteFactorSettings2 extends Template {
  this: VectorTemplate {
    type TemplateType <: DotTemplate
    type Neighbor1Type <: DiscreteVar
    type Neighbor2Type <: DiscreteVar
    type FactorType <: { def _1:DiscreteVariable ; def _2:DiscreteVariable }
  } =>
  lazy val ndd1: DiscreteDomain = throw new Error // TODO nd1.asInstanceOf[DiscreteDomain[DiscreteVar]]
  lazy val ndsize1 = ndd1.size
  lazy val ndd2: DiscreteDomain = throw new Error // TODO nd2.asInstanceOf[DiscreteDomain[DiscreteVar]]
  lazy val ndsize2 = ndd2.size
  // Managing settings iteration
  override def hasSettingsIterator: Boolean = true
  override def forSettings(factor:FactorType)(f: =>Unit): Unit = {
    if (settingsSparsified) {
      forIndex(sparseSettings1.length)(i => {
        factor._1.set(i)(null)
        forIndex(sparseSettings1(i).length)(j => {
          factor._2.set(j)(null)
          f
        })
      })
    } else {
      var i = 0
      while (i < ndsize1) {
        factor._1.set(i)(null)
        var j = 0
        while (j < ndsize2) {
          factor._2.set(j)(null)
          f
          j += 1
        }
      }
    }
  }
  // Call function f for each valid (possibly sparsified) variable value setting 
  // of the neighboring variables specified in 'vs'. 
  override def forSettingsOf(factor:FactorType, vs:Seq[Variable])(f: =>Unit): Unit = {
    if (vs.size == 1) {
      val v = vs.head
      if (factor._1 eq v) {
        // vary v1, keep v2 constant
        val v = factor._1 // Get it with the correct type
        if (settingsSparsified) {
          val sparseSettings = sparseSettings2(factor._2.intValue)
          forIndex(sparseSettings.length)(i => { v.set(sparseSettings(i))(null); f })
        } else forIndex(ndsize1)(i => { v.set(i)(null); f })
      } else if (factor._2 eq v) {
        // vary v2, keep v1 constant
        val v = factor._2 // Get it with the correct type
        if (settingsSparsified) {
          val sparseSettings = sparseSettings1(factor._1.intValue)
          forIndex(sparseSettings.length)(i => { v.set(sparseSettings(i))(null); f })
        } else forIndex(ndsize2)(i => { v.set(i)(null); f })
      }
    } else if (vs.size == 2) {
      throw new Error("Not yet implemented.")
    } else throw new Error("Asked to vary settings of too many variables.")
  }

  private var settingsSparsified = false
  // Redundant storage of valid v1,v2 value pairs
  private var sparseSettings1: Array[Array[Int]] = null // first index=v1, second index=v2
  private var sparseSettings2: Array[Array[Int]] = null // first index=v2, second index=v1
  // Initialize sparseSettings1 and sparseSettings2 to cover all values in factors touching the variables in 'vs'.
  override def sparsifySettingsFor(vs:Iterable[Variable]): Unit = {
    println("Template sparsifySettingsFor ndsize1="+ndsize1+" ndsize2="+ndsize2)
    assert (ndsize1 > 0, "sparsifySettingsFor before Domain size properly set.")
    assert (ndsize2 > 0, "sparsifySettingsFor before Domain size properly set.")
    val sparse1 = new HashMap[Int,scala.collection.mutable.Set[Int]]
    val sparse2 = new HashMap[Int,scala.collection.mutable.Set[Int]]
    vs.foreach(v => {
      this.factors(v).foreach(f => {
        sparse1.getOrElseUpdate(f._1.intValue, new HashSet[Int]) += f._2.intValue
        sparse2.getOrElseUpdate(f._2.intValue, new HashSet[Int]) += f._1.intValue
      })
    })
    sparseSettings1 = new Array[Array[Int]](ndsize1)
    sparseSettings2 = new Array[Array[Int]](ndsize2)
    forIndex(sparseSettings1.length)(i => sparseSettings1(i) = sparse1.getOrElse(i, new HashSet[Int]).toArray)
    forIndex(sparseSettings2.length)(i => sparseSettings2(i) = sparse2.getOrElse(i, new HashSet[Int]).toArray)
    settingsSparsified = true 
  }
}
*/


abstract class Template3[N1<:Variable,N2<:Variable,N3<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3]) extends Template {
  type Neighbor1Type = N1
  type Neighbor2Type = N2
  type Neighbor3Type = N3
  val nc1 = nm1.erasure
  val nc2 = nm2.erasure
  val nc3 = nm3.erasure
  val nc1a = { val ta = nm1.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(nc1)) { assert(ta.length == 1); ta.head.erasure } else null }
  val nc2a = { val ta = nm2.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(nc2)) { assert(ta.length == 1); ta.head.erasure } else null }
  val nc3a = { val ta = nm3.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(nc3)) { assert(ta.length == 1); ta.head.erasure } else null }
  override def factors(v: Variable): Iterable[FactorType] = {
    var ret = new ListBuffer[FactorType]
    if (nc1.isAssignableFrom(v.getClass)) ret ++= unroll1(v.asInstanceOf[N1])
    if (nc2.isAssignableFrom(v.getClass)) ret ++= unroll2(v.asInstanceOf[N2])
    if (nc3.isAssignableFrom(v.getClass)) ret ++= unroll3(v.asInstanceOf[N3])
    if ((nc1a ne null) && nc1a.isAssignableFrom(v.getClass)) ret ++= unroll1s(v.asInstanceOf[N1#ContainedVariableType])
    if ((nc2a ne null) && nc2a.isAssignableFrom(v.getClass)) ret ++= unroll2s(v.asInstanceOf[N2#ContainedVariableType])
    if ((nc3a ne null) && nc3a.isAssignableFrom(v.getClass)) ret ++= unroll3s(v.asInstanceOf[N3#ContainedVariableType])
    val cascadeVariables = unrollCascade(v); if (cascadeVariables.size > 0) ret ++= cascadeVariables.flatMap(factors(_))
    ret
  }
  def unroll1(v:N1): Iterable[FactorType]
  def unroll2(v:N2): Iterable[FactorType]
  def unroll3(v:N3): Iterable[FactorType]
  def unroll1s(v:N1#ContainedVariableType): Iterable[FactorType] = throw new Error("You must override unroll1s.")
  def unroll2s(v:N2#ContainedVariableType): Iterable[FactorType] = throw new Error("You must override unroll2s.")
  def unroll3s(v:N3#ContainedVariableType): Iterable[FactorType] = throw new Error("You must override unroll3s.")
  type FactorType = Factor
  final case class Factor(_1:N1, _2:N2, _3:N3) extends super.Factor {
    if (_neighborDomains eq null) {
      _neighborDomains = _newNeighborDomains
      _neighborDomains += _1.domain
      _neighborDomains += _2.domain
      _neighborDomains += _3.domain
    }
    def numVariables = 3
    def variable(i:Int) = i match { case 0 => _1; case 1 => _2; case 2 => _3; case _ => throw new IndexOutOfBoundsException(i.toString) }
    def values: ValuesType = new Values(_1.value, _2.value, _3.value)
    def statistics: StatisticsType = Template3.this.statistics(values)
    override def cachedStatistics: StatisticsType = Template3.this.cachedStatistics(values)
    def copy(s:Substitutions) = Factor(s.sub(_1), s.sub(_2), s.sub(_3))
  } 
  // Values
  type ValuesType = Values
  final case class Values(_1:N1#Value, _2:N2#Value, _3:N3#Value) extends super.Values {
    def statistics = Template3.this.statistics(this)
  }
  // Statistics
  def statistics(values:Values): StatisticsType
  private var cachedStatisticsArray: Array[StatisticsType] = null
  private var cachedStatisticsHash: HashMap[Product,StatisticsType] = null
  /** It is callers responsibility to clearCachedStatistics if weights or other relevant state changes. */
  override def cachedStatistics(values:Values): StatisticsType = if (Template.enableCachedStatistics) {
    //println("Template3.cachedStatistics")
    if (values._1.isInstanceOf[DiscreteValue] && values._2.isInstanceOf[DiscreteValue] && values._3.isInstanceOf[DiscretesValue] /*&& f._3.isConstant*/ ) {
      val v1 = values._1.asInstanceOf[DiscreteValue]
      val v2 = values._2.asInstanceOf[DiscreteValue]
      val v3 = values._3.asInstanceOf[DiscretesValue]
      if (cachedStatisticsHash eq null) cachedStatisticsHash = new HashMap[Product,StatisticsType]
      val i = ((v1.intValue, v2.intValue, v3))
      //print(" "+((v1.intValue, v2.intValue))); if (cachedStatisticsHash.contains(i)) println("*") else println(".")
      cachedStatisticsHash.getOrElseUpdate(i, statistics(values))
    } else {
      statistics(values)
    }
  } else statistics(values)
  override def clearCachedStatistics: Unit =  { cachedStatisticsArray = null; cachedStatisticsHash = null }
}
trait Statistics3[S1,S2,S3] extends Template {
  final case class Stat(_1:S1, _2:S2, _3:S3) extends super.Stat
  type StatType = Stat
  type StatisticsType = Statistics
}
trait VectorStatistics3[S1<:DiscretesValue,S2<:DiscretesValue,S3<:DiscretesValue] extends VectorTemplate {
  final case class Stat(_1:S1, _2:S2, _3:S3) extends { val vector: Vector = _1 flatOuter (_2 flatOuter _3) } with super.Stat {
    if (_statisticsDomains eq null) {
      _statisticsDomains = _newStatisticsDomains
      _statisticsDomains += _1.domain
      _statisticsDomains += _2.domain
      _statisticsDomains += _3.domain
    }
  }
  type StatType = Stat
  type StatisticsType = Statistics
}
trait DotStatistics3[S1<:DiscretesValue,S2<:DiscretesValue,S3<:DiscretesValue] extends VectorStatistics3[S1,S2,S3] with DotTemplate
abstract class TemplateWithStatistics3[N1<:Variable,N2<:Variable,N3<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3]) extends Template3[N1,N2,N3] with Statistics3[N1#Value,N2#Value,N3#Value] {
  def statistics(values:Values): StatType = Stat(values._1, values._2, values._3)
}
abstract class TemplateWithVectorStatistics3[N1<:DiscretesVar,N2<:DiscretesVar,N3<:DiscretesVar](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3]) extends Template3[N1,N2,N3] with VectorStatistics3[N1#Value,N2#Value,N3#Value]  {
  def statistics(values:Values): StatType = Stat(values._1, values._2, values._3)
  //init(nm1, nm2, nm3)
}
abstract class TemplateWithDotStatistics3[N1<:DiscretesVar,N2<:DiscretesVar,N3<:DiscretesVar](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3]) extends Template3[N1,N2,N3] with DotStatistics3[N1#Value,N2#Value,N3#Value]  {
  def statistics(values:Values): StatType = Stat(values._1, values._2, values._3)
  //init(nm1, nm2, nm3)
}

abstract class Template4[N1<:Variable,N2<:Variable,N3<:Variable,N4<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3], nm4:Manifest[N4]) extends Template {
  type Neighbor1Type = N1
  type Neighbor2Type = N2
  type Neighbor3Type = N3
  type Neighbor4Type = N4
  val nc1 = nm1.erasure
  val nc2 = nm2.erasure
  val nc3 = nm3.erasure
  val nc4 = nm4.erasure
  val nc1a = { val ta = nm1.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(nc1)) { assert(ta.length == 1); ta.head.erasure } else null }
  val nc2a = { val ta = nm2.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(nc2)) { assert(ta.length == 1); ta.head.erasure } else null }
  val nc3a = { val ta = nm3.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(nc3)) { assert(ta.length == 1); ta.head.erasure } else null }
  val nc4a = { val ta = nm4.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(nc4)) { assert(ta.length == 1); ta.head.erasure } else null }
  override def factors(v: Variable): Iterable[FactorType] = {
    var ret = new ListBuffer[FactorType]
    if (nc1.isAssignableFrom(v.getClass)) ret ++= unroll1(v.asInstanceOf[N1])
    if (nc2.isAssignableFrom(v.getClass)) ret ++= unroll2(v.asInstanceOf[N2])
    if (nc3.isAssignableFrom(v.getClass)) ret ++= unroll3(v.asInstanceOf[N3])
    if (nc4.isAssignableFrom(v.getClass)) ret ++= unroll4(v.asInstanceOf[N4])
    if ((nc1a ne null) && nc1a.isAssignableFrom(v.getClass)) ret ++= unroll1s(v.asInstanceOf[N1#ContainedVariableType])
    if ((nc2a ne null) && nc2a.isAssignableFrom(v.getClass)) ret ++= unroll2s(v.asInstanceOf[N2#ContainedVariableType])
    if ((nc3a ne null) && nc3a.isAssignableFrom(v.getClass)) ret ++= unroll3s(v.asInstanceOf[N3#ContainedVariableType])
    if ((nc4a ne null) && nc4a.isAssignableFrom(v.getClass)) ret ++= unroll4s(v.asInstanceOf[N4#ContainedVariableType])
    val cascadeVariables = unrollCascade(v); if (cascadeVariables.size > 0) ret ++= cascadeVariables.flatMap(factors(_))
    ret
  }
  def unroll1(v:N1): Iterable[FactorType]
  def unroll2(v:N2): Iterable[FactorType]
  def unroll3(v:N3): Iterable[FactorType]
  def unroll4(v:N4): Iterable[FactorType]
  def unroll1s(v:N1#ContainedVariableType): Iterable[FactorType] = throw new Error("You must override unroll1s.")
  def unroll2s(v:N2#ContainedVariableType): Iterable[FactorType] = throw new Error("You must override unroll2s.")
  def unroll3s(v:N3#ContainedVariableType): Iterable[FactorType] = throw new Error("You must override unroll3s.")
  def unroll4s(v:N4#ContainedVariableType): Iterable[FactorType] = throw new Error("You must override unroll4s.")
  type FactorType = Factor
  final case class Factor(_1:N1, _2:N2, _3:N3, _4:N4) extends super.Factor {
    if (_neighborDomains eq null) {
      _neighborDomains = _newNeighborDomains
      _neighborDomains += _1.domain
      _neighborDomains += _2.domain
      _neighborDomains += _3.domain
      _neighborDomains += _4.domain
    }
    def numVariables = 4
    def variable(i:Int) = i match { case 0 => _1; case 1 => _2; case 2 => _3; case 3 => _4; case _ => throw new IndexOutOfBoundsException(i.toString) }
    def values: ValuesType = new Values(_1.value, _2.value, _3.value, _4.value)
    def statistics: StatisticsType = Template4.this.statistics(values)
    def cachedStatistics: StatisticsType = Template4.this.statistics(values)
    // TODO override def cachedStatistics: StatisticsType = Template4.this.cachedStatistics(_1.value, _2.value, _3.value, _4.value)
    def copy(s:Substitutions) = Factor(s.sub(_1), s.sub(_2), s.sub(_3), s.sub(_4))
  } 
  // Values
  type ValuesType = Values
  final case class Values(_1:N1#Value, _2:N2#Value, _3:N3#Value, _4:N4#Value) extends super.Values {
    def statistics = Template4.this.statistics(this)
  }
  // Statistics
  def statistics(values:Values): StatisticsType
  //def stats(v:Variable) = factors(v).flatMap(_statistics(_))
}
trait Statistics4[S1,S2,S3,S4] extends Template {
  final case class Stat(_1:S1, _2:S2, _3:S3, _4:S4) extends super.Stat 
  type StatType = Stat
  type StatisticsType = Statistics
}
trait VectorStatistics4[S1<:DiscretesValue,S2<:DiscretesValue,S3<:DiscretesValue,S4<:DiscretesValue] extends VectorTemplate {
  final case class Stat(_1:S1, _2:S2, _3:S3, _4:S4) extends  { val vector: Vector = _1 flatOuter (_2 flatOuter (_3 flatOuter _4)) } with super.Stat {
    if (_statisticsDomains eq null) {
      _statisticsDomains = _newStatisticsDomains
      _statisticsDomains += _1.domain
      _statisticsDomains += _2.domain
      _statisticsDomains += _3.domain
      _statisticsDomains += _4.domain
    }
  }
  type StatType = Stat
  type StatisticsType = Statistics
}
trait DotStatistics4[S1<:DiscretesValue,S2<:DiscretesValue,S3<:DiscretesValue,S4<:DiscretesValue] extends VectorStatistics4[S1,S2,S3,S4] with DotTemplate
abstract class TemplateWithStatistics4[N1<:Variable,N2<:Variable,N3<:Variable,N4<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3], nm4:Manifest[N4]) extends Template4[N1,N2,N3,N4] with Statistics4[N1#Value,N2#Value,N3#Value,N4#Value] {
  // Returning StatType instead of StatisticsType here to declare that we are returning a singleton Stat
  def statistics(values:Values) = Stat(values._1, values._2, values._3, values._4)
}
abstract class TemplateWithVectorStatistics4[N1<:DiscretesVar,N2<:DiscretesVar,N3<:DiscretesVar,N4<:DiscretesVar](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3], nm4:Manifest[N4]) extends Template4[N1,N2,N3,N4] with VectorStatistics4[N1#Value,N2#Value,N3#Value,N4#Value]  {
  def statistics(values:Values): StatType = Stat(values._1, values._2, values._3, values._4)
  //init(nm1, nm2, nm3, nm4)
}
abstract class TemplateWithDotStatistics4[N1<:DiscretesVar,N2<:DiscretesVar,N3<:DiscretesVar,N4<:DiscretesVar](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3], nm4:Manifest[N4]) extends Template4[N1,N2,N3,N4] with DotStatistics4[N1#Value,N2#Value,N3#Value,N4#Value]  {
  def statistics(values:Values): StatType = Stat(values._1, values._2, values._3, values._4)
  //init(nm1, nm2, nm3, nm4)
}


// Shortcuts for templates whose statistics are a subset of their neighbors, coming from the end of the neighbor list.
// AKM: Let me know if you think it would be more sensible to have them come from the beginning instead.

abstract class Template2WithStatistics1[N1<:Variable:Manifest,N2<:Variable:Manifest] extends Template2[N1,N2] with Statistics1[N2#Value] {
  def statistics(v:Values): StatType = Stat(v._2)
}
abstract class Template3WithStatistics1[N1<:Variable:Manifest,N2<:Variable:Manifest,N3<:Variable:Manifest] extends Template3[N1,N2,N3] with Statistics1[N3#Value] {
  def statistics(v:Values): StatType = Stat(v._3)
}
abstract class Template3WithStatistics2[N1<:Variable:Manifest,N2<:Variable:Manifest,N3<:Variable:Manifest] extends Template3[N1,N2,N3] with Statistics2[N2#Value,N3#Value] {
  def statistics(v:Values): StatType = Stat(v._2, v._3)
}
