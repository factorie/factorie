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
import scala.util.Random
import scala.math
import scala.util.Sorting
import java.io.{File,PrintStream,FileOutputStream,PrintWriter,FileReader,FileWriter,BufferedReader}
import cc.factorie.la._

// Factor Templates, which create factors in a factor graph on-the-fly as necessary.
// A factor template specifies
// (1) a description of the arbitrary relationship among its variable neighbors
// (2) a sufficient statistics function that maps those neighbors to the statistics necessary to return a real-valued score
// (3) an aggregator for multiple statistics of the same template
// (4) a function mapping those aggregated statistics to a real-valued score
// (5) optionally, the parameters used in the function to calculate that score;
//     (alternatively the score may be calculated in some fixed way without learned parameters) 

/** A single factor in a factor graph.  In other words, a factor
    template packaged with a set of variables neighboring the
    factor.  
    @author Andrew McCallum */
trait Factor extends Ordered[Factor] {
  def template: Template
  def numVariables: Int 
  def variable(index: Int): Variable
  def statistics: Statistics
  def score: Double = statistics.score
  def variables: Iterable[Variable] = { val result = new ArrayBuffer[Variable](numVariables); for (i <- 0 until numVariables) result += variable(i); result }
  def randomVariable(implicit random:Random): Variable = variable(random.nextInt(numVariables))
  // Implement Ordered, such that worst (lowest) scores are considered "high"
  def compare(that: Factor) = {val d = that.score - this.score; if (d > 0.0) 1 else if (d < 0.0) -1 else 0}
  // Implement equality based on class assignability and Variable contents equality
  //override def canEqual(other: Any) = (null != other) && other.isInstanceOf[Factor];
  override def equals(other: Any): Boolean = other match {
    case other:Factor => 
      (this eq other) || ((this.template eq other.template)
                          && forallIndex(numVariables)(i => this.variable(i) eq other.variable(i)))
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

/** A summary of all the statistics of a Factor */
trait Statistics {
  def template: Template
  def score: Double
}

/** A container for sufficient statistics of a Factor.  Note that one Factor can result in more than one of these Stat objects. */
trait Stat extends Statistics

/** A collection of Stat objects along with a method of producting a compatibility score from them. */
trait Stats extends Statistics with Iterable[Stat]



/** The template for many factors.  Manages its connections to neighboring variables.
    @Andrew McCallum
*/
trait Template {
  type TemplateType <: Template // like a self-type
  type FactorType <: Factor
  type StatType <: Stat
  type StatisticsType <: Statistics
  trait Statistics extends cc.factorie.Statistics {
    override def template: TemplateType = Template.this.asInstanceOf[TemplateType];
  }
  trait Factor extends cc.factorie.Factor { 
    override def template: TemplateType = Template.this.asInstanceOf[TemplateType];
    override def statistics: StatisticsType
    //def stats: Iterable[StatType]
    //def statistic: StatisticType = Template.this.statistic(this.statistics) // TODO verify that this gets override definitions of statistics()
  }
  trait Stat extends cc.factorie.Stat with Statistics {
    override def template: TemplateType = Template.this.asInstanceOf[TemplateType];
    val score = Template.this.score(this.asInstanceOf[StatType]) // TODO can we find a way to get rid of this cast?  Yes, use a self-type Stat[This]
  }
  class Stats(val stats:Iterable[StatType]) extends cc.factorie.Stats with Statistics with Iterable[StatType] {
    def iterator = stats.iterator
    override def template: TemplateType = Template.this.asInstanceOf[TemplateType];
    val score = stats.foldLeft(0.0)(_ + Template.this.score(_))
  }
  def score(s:StatType): Double
  def statistics(ss:Iterable[StatType]): StatisticsType = new Stats(ss)
  //def statistics(v:Variable): StatisticsType = new Stats(factors(v).map(_.stats).flatten)
  /** To allow users' "def statistics(v1)" to return an Iterator[Stat] */
  implicit def iterableStatToStatistics[S<:StatType](ss:Iterable[S]): StatisticsType = statistics(ss)
  //def statistic(ss:Iterable[StatType]): StatisticType = new Statistic(ss).asInstanceOf[StatisticType] // TODO is there some way to avoid this cast?
  /** Used by the trickery that obtains Manifests for Statistics*[] traits.  
      See template2initialized in Package.scala. */
  var isInitialized = true
  def defaultFactorName = "Factor"
  var factorName = defaultFactorName
  /** Assign this Template a name which will be used later when its factors are printed. */
  def setName(n:String): this.type = { factorName = n; this }
  /** Assign this Template a name which will be used later when its factors are printed. */
  def %(n:String): this.type = setName(n) // because % is the comment character in shell languages such as /bin/sh and Makefiles.
  /**A version of factors that takes the Diff object instead of just the variable */
  def factors(d: Diff): Iterable[Factor] = if (d.variable == null) Nil else factors(d.variable)
  def factors(v: Variable): Iterable[Factor]
  def factors(difflist: DiffList): Iterable[Factor] = {
    //var result = new LinkedHashSet[Factor]()
    var result = new HashSet[Factor]()
    for (diff <- difflist; factor <- factors(diff)) { if (factor eq null) throw new Error("unroll returned null Factor") else result += factor } 
    //difflist.foreach(diff => result ++= factors(diff))
    result.toList // TODO is this necessary?
  }
  def factors(variables:Iterable[Variable]): Iterable[Factor] = { // TODO Why is this List, and not Iterable?  Do the GibbsSampling cases depend on this?
    if (variables.size == 1) return factors(variables.head) // Efficiently avoids the HashSet.
    //var result = new LinkedHashSet[Factor]()
    var result = new HashSet[Factor]()
    for (v <- variables; factor <- factors(v)) { if (factor eq null) throw new Error("unroll returned null Factor") else result += factor }
    result.toList // TODO is this necessary?
  }
  /** Called in implementations of factors(Variable) to give the variable a chance 
      to specify additional dependent variables on which factors(Variable) should also be called. */
  def unrollCascade(v:Variable): Iterable[Variable] = v.unrollCascade
  /** The filename into which to save this factor.  If factorName is not the default, use it, otherwise use the class name. */
  protected def filename: String = if (factorName != defaultFactorName) factorName else this.getClass.getName
  def save(dirname:String): Unit = {}
  def load(dirname:String): Unit = {}
}

/** A Template whose sufficient statistics are represented as a Scalala Vector. 
    @author Andrew McCallum
*/
trait VectorTemplate extends Template {
  val statClasses = new ArrayBuffer[Class[VectorVar]] {
    var frozen: Boolean = false
    def freeze = frozen = true
    // Override to make sure that if the ArrayBuffer tries to grow when frozen, we throw an error
    override def ensureSize(s:Int) = if (frozen) throw new IllegalStateException("Template already .init'ed.") else super.ensureSize(s)
  }
  def statDomains: Seq[VectorDomain[_]] = {
    if (statClasses.isEmpty) throw new IllegalStateException("You must call .init on this Template before use.")
    statClasses.map(Domain.get[VectorVar](_))
  } 
  def freezeDomains: Unit = {
    if (statClasses.isEmpty) throw new IllegalStateException("You must call .init on this Template before use.")
    statClasses.foreach(Domain.get[VectorVar](_).freeze)
  }
  // TODO Consider changing name to statSize?
  lazy val statsize: Int = {
    if (statClasses.isEmpty) throw new IllegalStateException("You must call .init on this Template before use.")
    val ss = statClasses.multiplyInts(Domain.get[VectorVar](_).maxVectorSize)
    //println("Template "+this.getClass.getName+"["+statClasses.mkString(",")+"] statsize="+ss+" = "+statClasses.map(Domain.get[VectorVar](_).maxVectorSize).mkString("*"))
    ss
  } 
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
        new SparseVector(statsize)
      }
    }
  }
  override def statistics(ss:Iterable[StatType]): StatisticsType = new Stats(ss)
  //override def statistics(v:Variable): StatisticsType = new Stats(factors(v).map(_.stats).flatten)
  //override def statistic(ss:Iterable[StatType]): StatisticType = new Statistic(ss)
  // TODO implement this!
  private def unflattenOuter(weightIndex:Int, dimensions:Int*): Array[Int] = new Array[Int](2) 
} // end of VectorTemplate


/** A VectorTemplate that also has a vector of weights, and calculates score by a dot-product between statistics.vector and weights. 
    @author Andrew McCallum */
trait DotTemplate extends VectorTemplate {
  type TemplateType <: DotTemplate
  lazy val weights: Vector = { freezeDomains; new DenseVector(statsize) } // Dense by default, may be override in sub-traits
  def score(s:StatType) = weights match {
    case w:DenseVector => { assert(isInitialized == true); w dot s.vector }
    //case w:SparseHashVector => w dot s.vector // TODO Uncomment this.  It was only commented because latest version of scalala didn't seem to have this class any more
    case w:SparseVector => w dot s.vector
  }
  override def save(dirname:String): Unit = {
    val f = new File(dirname+"/"+filename) // TODO Make this work on MSWindows also
    if (f.exists) return // Already exists, don't write it again
    for (d <- statDomains) d.save(dirname)
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
    for (d <- statDomains) { /* println(" Loading Domain["+d+"]"); */ d.load(dirname) }
    // TODO Why would statsize be 0 or negative?
    if (statsize <= 0 || weights.activeElements.exists({case(i,v) => v != 0})) return // Already have non-zero weights, must already be read.
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
  override lazy val weights: Vector = { new SparseVector(statsize) } // Dense by default, here overridden to be sparse
}

/** A DotTemplate that stores its parameters in a Scalala SparseHashVector instead of a DenseVector 
    @author Sameer Singh */
trait SparseHashWeights extends DotTemplate {
  override lazy val weights: Vector = { freezeDomains; new SparseHashVector(statsize) } // Dense by default, override to be sparseHashed
}


abstract class Template1[N1<:Variable](implicit nm1: Manifest[N1]) extends Template {
  val nc1 = nm1.erasure // "Neighbor class"
  val nc1a = { val ta = nm1.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(nc1)) { assert(ta.length == 1); ta.head.erasure } else null }
  // TODO create methods like this for all Templates and put abstract version in Template
  def hasNeighbor[NN](implicit m:Manifest[NN]) = nc1.isAssignableFrom(m.erasure)
  def factors(v:Variable): Iterable[Factor] = {
    // TODO Given the surprise about how slow Manifest <:< was, I wonder how slow this is when there are lots of traits!
    // When I substituted "isAssignable" for HashMap caching in GenericSampler I got 42.8 versus 44.4 seconds ~ 3.7%  Perhaps worth considering?
    var ret = new ListBuffer[Factor]
    if (nc1.isAssignableFrom(v.getClass)) ret ++= unroll1(v.asInstanceOf[N1])
    if ((nc1a ne null) && nc1a.isAssignableFrom(v.getClass)) ret ++= unroll1s(v.asInstanceOf[N1#ContainedVariableType])
    // TODO It would be so easy for the user to define Variable.unrollCascade to cause infinite recursion.  Can we make better checks for this?
    val cascadeVariables = unrollCascade(v); if (cascadeVariables.size > 0) ret ++= cascadeVariables.flatMap(factors(_))
    ret
  }
  def unroll1(v:N1): Iterable[Factor] = new Factor(v)
  def unroll1s(v:N1#ContainedVariableType): Iterable[Factor] = throw new Error("You must override unroll1s.")
  @inline final def _statistics(f:Factor): StatisticsType = statistics(f._1)
  def statistics(v1:N1): StatisticsType
  def stats(v:Variable): Iterable[StatisticsType] = factors(v).map(_statistics(_))
  case class Factor(_1:N1) extends super.Factor {
    def numVariables = 1
    def variable(i:Int) = i match { case 0 => _1; case _ => throw new IndexOutOfBoundsException(i.toString) }
    def statistics: StatisticsType = _statistics(this)
    //def stats: Iterable[StatType] = statistics match { case ss:Iterable[Stats] => ss; case s:Stat => Seq(s) }
  } 
}
trait Statistics1[S1] extends Template {
  case class Stat(_1:S1) extends super.Stat
  type StatType = Stat
  type StatisticsType = Statistics
  //def init(implicit m1:Manifest[S1]): this.type = this
}
trait VectorStatistics1[S1<:VectorVar] extends VectorTemplate {
  type StatType = Stat
  type StatisticsType = Statistics
  // Use Scala's "pre-initialized fields" syntax because super.Stat needs vector to initialize score
  case class Stat(_1:S1) extends { val vector: Vector = _1.vector } with super.Stat
  //case class Stat(_1:S1) extends super.Stat { lazy val vector: Vector = _1.vector }
  isInitialized = false
  def init(implicit m1:Manifest[S1]): this.type = {
    if (!isInitialized) {
      statClasses += m1.erasure.asInstanceOf[Class[VectorVar]]
      statClasses.freeze
      isInitialized = true
    }
    this
  }
}
trait DotStatistics1[S1<:VectorVar] extends VectorStatistics1[S1] with DotTemplate {
  def setWeight(entry:S1, w:Double) = entry match {
    case d:DiscreteVar => weights(d.intValue) = w
    case ds:DiscreteVars => ds.vector.activeDomain.foreach(i => weights(i) = w)
  }
}
abstract class TemplateWithStatistics1[N1<:Variable](implicit nm1:Manifest[N1]) extends Template1[N1]()(nm1) with Statistics1[N1] {
  def statistics(v1:N1): StatisticsType = Stat(v1)
}
abstract class TemplateWithVectorStatistics1[N1<:VectorVar](implicit nm1:Manifest[N1]) extends Template1[N1]()(nm1) with VectorStatistics1[N1]  {
  def statistics(v1:N1): StatisticsType = Stat(v1)
  init //(nm1)
}
class TemplateWithDotStatistics1[N1<:VectorVar](implicit nm1:Manifest[N1]) extends Template1[N1]()(nm1) with DotStatistics1[N1] {
  def statistics(v1:N1): StatisticsType = Stat(v1)
  init //(nm1)
}

abstract class Template2[N1<:Variable,N2<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2]) extends Template {
  val nc1 = nm1.erasure
  val nc2 = nm2.erasure
  val nc1a = { val ta = nm1.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(nc1)) { assert(ta.length == 1); ta.head.erasure } else null }
  val nc2a = { val ta = nm2.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(nc2)) { assert(ta.length == 1); ta.head.erasure } else null }
  override def factors(v: Variable): Iterable[Factor] = {
    var ret = new ListBuffer[Factor]
    if (nc1.isAssignableFrom(v.getClass)) ret ++= unroll1(v.asInstanceOf[N1]) 
    if (nc2.isAssignableFrom(v.getClass)) ret ++= unroll2(v.asInstanceOf[N2]) 
    if ((nc1a ne null) && nc1a.isAssignableFrom(v.getClass)) ret ++= unroll1s(v.asInstanceOf[N1#ContainedVariableType])
    if ((nc2a ne null) && nc2a.isAssignableFrom(v.getClass)) ret ++= unroll2s(v.asInstanceOf[N2#ContainedVariableType])
    val cascadeVariables = unrollCascade(v); if (cascadeVariables.size > 0) ret ++= cascadeVariables.flatMap(factors(_))
    ret
  }
  def unroll1(v:N1): Iterable[Factor] 
  def unroll2(v:N2): Iterable[Factor]
  def unroll1s(v:N1#ContainedVariableType): Iterable[Factor] = throw new Error("You must override unroll1s.")
  def unroll2s(v:N2#ContainedVariableType): Iterable[Factor] = throw new Error("You must override unroll2s.")
  @inline final def _statistics(f:Factor): StatisticsType = statistics(f._1, f._2)
  def statistics(v1:N1, v2:N2): StatisticsType
  //def stats(v:Variable) = factors(v).flatMap(_statistics(_))
  case class Factor(_1:N1, _2:N2) extends super.Factor {
    def numVariables = 2
    def variable(i:Int) = i match { case 0 => _1; case 1 => _2; case _ => throw new IndexOutOfBoundsException(i.toString) }
    def statistics: StatisticsType = _statistics(this)
  } 
}
trait Statistics2[S1,S2] extends Template {
  case class Stat(_1:S1, _2:S2) extends super.Stat
  type StatType = Stat
  type StatisticsType = Statistics
}
trait VectorStatistics2[S1<:VectorVar,S2<:VectorVar] extends VectorTemplate {
  //case class Stat(_1:S1, _2:S2) extends super.Stat { lazy val vector: Vector = _1.vector flatOuter _2.vector } 
  case class Stat(_1:S1, _2:S2) extends { val vector: Vector = _1.vector flatOuter _2.vector } with super.Stat
  type StatType = Stat
  type StatisticsType = Statistics
  isInitialized = false
  def init(implicit m1:Manifest[S1], m2:Manifest[S2]): this.type = {
    if (!isInitialized) {
      statClasses ++= List(m1.erasure.asInstanceOf[Class[VectorVar]], m2.erasure.asInstanceOf[Class[VectorVar]])
      statClasses.freeze
      isInitialized = true
    }
    this 
  }  
}
trait DotStatistics2[S1<:VectorVar,S2<:VectorVar] extends VectorStatistics2[S1,S2] with DotTemplate
abstract class TemplateWithStatistics2[N1<:Variable,N2<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2]) extends Template2[N1,N2]()(nm1,nm2) with Statistics2[N1,N2] {
  def statistics(v1:N1, v2:N2): StatisticsType = Stat(v1, v2)
}
abstract class TemplateWithVectorStatistics2[N1<:VectorVar,N2<:VectorVar](implicit nm1:Manifest[N1], nm2:Manifest[N2]) extends Template2[N1,N2]()(nm1,nm2) with VectorStatistics2[N1,N2]  {
  def statistics(v1:N1,v2:N2): StatisticsType = Stat(v1,v2)
  init(nm1, nm2)
}
abstract class TemplateWithDotStatistics2[N1<:VectorVar,N2<:VectorVar](implicit nm1:Manifest[N1], nm2:Manifest[N2]) extends Template2[N1,N2]()(nm1,nm2) with DotStatistics2[N1,N2]  {
  def statistics(v1:N1,v2:N2): StatisticsType = Stat(v1,v2)
  init(nm1, nm2)
}

abstract class Template3[N1<:Variable,N2<:Variable,N3<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3]) extends Template {
  val nc1 = nm1.erasure
  val nc2 = nm2.erasure
  val nc3 = nm3.erasure
  val nc1a = { val ta = nm1.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(nc1)) { assert(ta.length == 1); ta.head.erasure } else null }
  val nc2a = { val ta = nm2.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(nc2)) { assert(ta.length == 1); ta.head.erasure } else null }
  val nc3a = { val ta = nm3.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(nc3)) { assert(ta.length == 1); ta.head.erasure } else null }
  override def factors(v: Variable): Iterable[Factor] = {
    var ret = new ListBuffer[Factor]
    if (nc1.isAssignableFrom(v.getClass)) ret ++= unroll1(v.asInstanceOf[N1])
    if (nc2.isAssignableFrom(v.getClass)) ret ++= unroll2(v.asInstanceOf[N2])
    if (nc3.isAssignableFrom(v.getClass)) ret ++= unroll3(v.asInstanceOf[N3])
    if ((nc1a ne null) && nc1a.isAssignableFrom(v.getClass)) ret ++= unroll1s(v.asInstanceOf[N1#ContainedVariableType])
    if ((nc2a ne null) && nc2a.isAssignableFrom(v.getClass)) ret ++= unroll2s(v.asInstanceOf[N2#ContainedVariableType])
    if ((nc3a ne null) && nc3a.isAssignableFrom(v.getClass)) ret ++= unroll3s(v.asInstanceOf[N3#ContainedVariableType])
    val cascadeVariables = unrollCascade(v); if (cascadeVariables.size > 0) ret ++= cascadeVariables.flatMap(factors(_))
    ret
  }
  def unroll1(v:N1): Iterable[Factor]
  def unroll2(v:N2): Iterable[Factor]
  def unroll3(v:N3): Iterable[Factor]
  def unroll1s(v:N1#ContainedVariableType): Iterable[Factor] = throw new Error("You must override unroll1s.")
  def unroll2s(v:N2#ContainedVariableType): Iterable[Factor] = throw new Error("You must override unroll2s.")
  def unroll3s(v:N3#ContainedVariableType): Iterable[Factor] = throw new Error("You must override unroll3s.")
  @inline final def _statistics(f:Factor): StatisticsType = statistics(f._1, f._2, f._3)
  def statistics(v1:N1, v2:N2, v3:N3): StatisticsType
  //def stats(v:Variable) = factors(v).flatMap(_statistics(_))
  case class Factor(_1:N1, _2:N2, _3:N3) extends super.Factor {
    def numVariables = 3
    def variable(i:Int) = i match { case 0 => _1; case 1 => _2; case 2 => _3; case _ => throw new IndexOutOfBoundsException(i.toString) }
    def statistics: StatisticsType = _statistics(this)
  } 
}
trait Statistics3[S1,S2,S3] extends Template {
  case class Stat(_1:S1, _2:S2, _3:S3) extends super.Stat
  type StatType = Stat
  type StatisticsType = Statistics
}
trait VectorStatistics3[S1<:VectorVar,S2<:VectorVar,S3<:VectorVar] extends VectorTemplate {
  //case class Stat(_1:S1, _2:S2, _3:S3) extends super.Stat { lazy val vector: Vector = _1.vector flatOuter (_2.vector flatOuter _3.vector) } 
  case class Stat(_1:S1, _2:S2, _3:S3) extends { val vector: Vector = _1.vector flatOuter (_2.vector flatOuter _3.vector) } with super.Stat 
  type StatType = Stat
  type StatisticsType = Statistics
  isInitialized = false
  def init(implicit m1:Manifest[S1], m2:Manifest[S2], m3:Manifest[S3]): this.type = {
    if (!isInitialized) {
      statClasses ++= List(m1.erasure.asInstanceOf[Class[VectorVar]], m2.erasure.asInstanceOf[Class[VectorVar]], m3.erasure.asInstanceOf[Class[VectorVar]])
      statClasses.freeze
      isInitialized = true
    }
    this
  }  
}
trait DotStatistics3[S1<:VectorVar,S2<:VectorVar,S3<:VectorVar] extends VectorStatistics3[S1,S2,S3] with DotTemplate
abstract class TemplateWithStatistics3[N1<:Variable,N2<:Variable,N3<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3]) extends Template3[N1,N2,N3]()(nm1,nm2,nm3) with Statistics3[N1,N2,N3] {
  def statistics(v1:N1, v2:N2, v3:N3): StatisticsType = Stat(v1, v2, v3)
}
abstract class TemplateWithVectorStatistics3[N1<:VectorVar,N2<:VectorVar,N3<:VectorVar](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3]) extends Template3[N1,N2,N3]()(nm1,nm2,nm3) with VectorStatistics3[N1,N2,N3]  {
  def statistics(v1:N1,v2:N2,v3:N3): StatisticsType = Stat(v1,v2,v3)
  init(nm1, nm2, nm3)
}
abstract class TemplateWithDotStatistics3[N1<:VectorVar,N2<:VectorVar,N3<:VectorVar](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3]) extends Template3[N1,N2,N3]()(nm1,nm2,nm3) with DotStatistics3[N1,N2,N3]  {
  def statistics(v1:N1,v2:N2,v3:N3): StatisticsType = Stat(v1,v2,v3)
  init(nm1, nm2, nm3)
}

abstract class Template4[N1<:Variable,N2<:Variable,N3<:Variable,N4<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3], nm4:Manifest[N4]) extends Template {
  val nc1 = nm1.erasure
  val nc2 = nm2.erasure
  val nc3 = nm3.erasure
  val nc4 = nm4.erasure
  val nc1a = { val ta = nm1.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(nc1)) { assert(ta.length == 1); ta.head.erasure } else null }
  val nc2a = { val ta = nm2.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(nc2)) { assert(ta.length == 1); ta.head.erasure } else null }
  val nc3a = { val ta = nm3.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(nc3)) { assert(ta.length == 1); ta.head.erasure } else null }
  val nc4a = { val ta = nm4.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(nc4)) { assert(ta.length == 1); ta.head.erasure } else null }
  override def factors(v: Variable): Iterable[Factor] = {
    var ret = new ListBuffer[Factor]
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
  def unroll1(v:N1): Iterable[Factor]
  def unroll2(v:N2): Iterable[Factor]
  def unroll3(v:N3): Iterable[Factor]
  def unroll4(v:N4): Iterable[Factor]
  def unroll1s(v:N1#ContainedVariableType): Iterable[Factor] = throw new Error("You must override unroll1s.")
  def unroll2s(v:N2#ContainedVariableType): Iterable[Factor] = throw new Error("You must override unroll2s.")
  def unroll3s(v:N3#ContainedVariableType): Iterable[Factor] = throw new Error("You must override unroll3s.")
  def unroll4s(v:N4#ContainedVariableType): Iterable[Factor] = throw new Error("You must override unroll4s.")
  @inline final def _statistics(f:Factor): StatisticsType = statistics(f._1, f._2, f._3, f._4)
  def statistics(v1:N1, v2:N2, v3:N3, v4:N4): StatisticsType
  //def stats(v:Variable) = factors(v).flatMap(_statistics(_))
  case class Factor(_1:N1, _2:N2, _3:N3, _4:N4) extends super.Factor {
    def numVariables = 4
    def variable(i:Int) = i match { case 0 => _1; case 1 => _2; case 2 => _3; case 3 => _4; case _ => throw new IndexOutOfBoundsException(i.toString) }
    def statistics: StatisticsType = _statistics(this)
  } 
}
trait Statistics4[S1<:Variable,S2<:Variable,S3<:Variable,S4<:Variable] extends Template {
  case class Stat(_1:S1, _2:S2, _3:S3, _4:S4) extends super.Stat 
  type StatType = Stat
  type StatisticsType = Statistics
}
trait VectorStatistics4[S1<:VectorVar,S2<:VectorVar,S3<:VectorVar,S4<:VectorVar] extends VectorTemplate {
  //case class Stat(_1:S1, _2:S2, _3:S3, _4:S4) extends super.Stat { lazy val vector: Vector = _1.vector flatOuter (_2.vector flatOuter (_3.vector flatOuter _4.vector)) } 
  case class Stat(_1:S1, _2:S2, _3:S3, _4:S4) extends  { val vector: Vector = _1.vector flatOuter (_2.vector flatOuter (_3.vector flatOuter _4.vector)) } with super.Stat
  type StatType = Stat
  type StatisticsType = Statistics
  isInitialized = false
  def init(implicit m1:Manifest[S1], m2:Manifest[S2], m3:Manifest[S3], m4:Manifest[S4]): this.type = { 
    if (!isInitialized) {
      statClasses ++= List(m1.erasure.asInstanceOf[Class[VectorVar]], m2.erasure.asInstanceOf[Class[VectorVar]], m3.erasure.asInstanceOf[Class[VectorVar]], m4.erasure.asInstanceOf[Class[VectorVar]])
      statClasses.freeze
      isInitialized = true
    }
    this
  }
}
trait DotStatistics4[S1<:VectorVar,S2<:VectorVar,S3<:VectorVar,S4<:VectorVar] extends VectorStatistics4[S1,S2,S3,S4] with DotTemplate
abstract class TemplateWithStatistics4[N1<:Variable,N2<:Variable,N3<:Variable,N4<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3], nm4:Manifest[N4]) extends Template2[N1,N2]()(nm1,nm2) with Statistics4[N1,N2,N3,N4] {
  def statistics(v1:N1, v2:N2, v3:N3, v4:N4): StatisticsType = Stat(v1, v2, v3, v4)
}
abstract class TemplateWithVectorStatistics4[N1<:VectorVar,N2<:VectorVar,N3<:VectorVar,N4<:VectorVar](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3], nm4:Manifest[N4]) extends Template4[N1,N2,N3,N4]()(nm1,nm2,nm3,nm4) with VectorStatistics4[N1,N2,N3,N4]  {
  def statistics(v1:N1,v2:N2,v3:N3,v4:N4): StatisticsType = Stat(v1,v2,v3,v4)
  init(nm1, nm2, nm3, nm4)
}
abstract class TemplateWithDotStatistics4[N1<:VectorVar,N2<:VectorVar,N3<:VectorVar,N4<:VectorVar](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3], nm4:Manifest[N4]) extends Template4[N1,N2,N3,N4]()(nm1,nm2,nm3,nm4) with DotStatistics4[N1,N2,N3,N4]  {
  def statistics(v1:N1,v2:N2,v3:N3,v4:N4): StatisticsType = Stat(v1,v2,v3,v4)
  init(nm1, nm2, nm3, nm4)
}

