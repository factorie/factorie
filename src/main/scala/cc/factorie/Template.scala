/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer, FlatHashTable}
import scala.reflect.Manifest
import scala.util.Random
import scala.Math
import scala.util.Sorting
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import cc.factorie.util.{Log}
import scalala.tensor.sparse.{SparseHashVector, SparseVector, SparseBinaryVector, SingletonBinaryVector}
import java.io.{File,PrintStream,FileOutputStream,PrintWriter,FileReader,FileWriter,BufferedReader}

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
    factor.  Factor inherits from Iterable[Factor] so that we can
    return a single Factor when an Iterable[Factor] is required. 
    @author Andrew McCallum */
trait Factor extends Product with Ordered[Factor] {
  //type TemplateType <: Template
  def template : Template
  def numVariables: Int 
  def variable(index: Int): Variable
  def variables: Iterable[Variable] = for (i <- 0 until numVariables) yield variable(i)
  def statistic : Statistic
  def randomVariable(implicit random:Random): Variable = variable(random.nextInt(numVariables))
  //def score: Double
  //def vector: Vector // TODO Remove this.  Not all Factors have a vector
  /** A Factor can act as as singleton Iterable[Factor].  This makes it easier to return a single Factor from unroll* methods. */
  //def iterator: Iterator[Factor] = Iterator.single(this)
  /**A Factor can be placed into a List with another with this method.
  This makes it easier to return 2-3 Factors from unroll* methods via Factor() :: Factor() :: Factor() */
  def ::(that: Factor) = List(that, this)
  ///**Add this Factor to the FactorList of all the Variables that are this factor's neighbors */
  //def addToVariables = variables.filter(_.isInstanceOf[FactorList]).map(_.asInstanceOf[FactorList].addFactor(this))
  // Implement Ordered, such that worst (lowest) scores are considered "high"
  def compare(that: Factor) = {val d = that.statistic.score - this.statistic.score; if (d > 0.0) 1 else if (d < 0.0) -1 else 0}
  // Implement equality based on class assignability and Variable contents equality
  override def canEqual(other: Any) = (null != other) && other.isInstanceOf[Factor];
  override def equals(other: Any): Boolean = {
  	if (null == other) return false;
    //(this eq other) ||
    if (!canEqual(other)) return false
    val fother = other.asInstanceOf[Factor];
    (this.numVariables == fother.numVariables &&
     (0 until numVariables).forall(i => this.variable(i).hashCode == fother.variable(i).hashCode &&
           this.variable(i) == fother.variable(i)))
  }
  var _hashCode = -1
  override def hashCode: Int = {if (_hashCode == -1) _hashCode = variables.sumInts(_ hashCode); _hashCode}
  def factorName = template.factorName
  override def toString = { // TODO change to mkString
    val sb = new StringBuilder; sb append factorName; sb append '(';
    val iter = variables.iterator
    while (iter.hasNext) { sb append iter.next.toString; if (iter.hasNext) sb append "," } 
    sb append ")"
    sb.toString
  }
}



/**A container for the sufficient statistics of a Factor. */
trait Stat {
  // TODO Make this also extend Product, support scoring, etc, like Factor
  def template: Template
  def score : Double
  /** A Stat can act as as singleton Iterable[Stat].
  		This makes it easier to return a single Stat from unroll* methods. */
  //def iterator: Iterator[Stat] = Iterator.single(this)
  /** A Stat can be placed into a List with another with this method.
  This makes it easier to return 2-3 Factors from unroll* methods via Stat() :: Stat() :: Stat() */
  def ::(that: Stat) = List(that, this)
}

trait Statistic extends Iterable[Stat] {
  def score : Double
}

trait IterableSingle[T] extends Iterable[T] {
	this: T =>
  def iterator: Iterator[T] = Iterator.single(this)
}

/** The template for many factors.  Manages its connections to neighboring variables.
    @Andrew McCallum
*/
trait Template {
  type TemplateType <: Template // like a self-type
  type FactorType <: Factor
  type StatType <: Stat
  type StatisticType <: Statistic
  trait Factor extends cc.factorie.Factor { 
    override def template : TemplateType = Template.this.asInstanceOf[TemplateType];
    def statistics : Iterable[StatType];
    def statistic : StatisticType = Template.this.statistic(this.statistics) // TODO verify that this gets override definitions of statistic()
  }
  def defaultFactorName = "Factor"
  var factorName = defaultFactorName
  /** Assign this Template a name which will be used later when its factors are printed. */
  def name(n:String) : this.type = { factorName = n; this }
  /** Assign this Template a name which will be used later when its factors are printed. */
  def =:(n:String): this.type = name(n) // TODO Is this syntax too weird
  /** Assign this Template a name which will be used later when its factors are printed. */
  def %(n:String): this.type = name(n) // because % is the comment character in shell languages such as /bin/sh and Makefiles.
  trait Stat extends cc.factorie.Stat { 
    override def template : TemplateType = Template.this.asInstanceOf[TemplateType];
    def score = Template.this.score(this.asInstanceOf[StatType]) // TODO can we find a way to get rid of this cast?
  }
  class Statistic(val ss:Iterable[StatType]) extends cc.factorie.Statistic with Iterable[StatType] { /* or just "Stat" */
    def iterator = ss.iterator
    def template : TemplateType = Template.this.asInstanceOf[TemplateType];
    def score = ss.foldLeft(0.0)(_ + Template.this.score(_)) // TODO verify that this gets overriden definitions of score(_)
  }
  /**A version of factors that takes the Diff object instead of just the variable */
  def factors(d: Diff): Iterable[Factor] = if (d.variable == null) Nil else factors(d.variable)
  def factors(v: Variable): Iterable[Factor];
  def factors(difflist: DiffList): Iterable[Factor] = {
    //var result = new LinkedHashSet[Factor]()
    var result = new HashSet[Factor]()
    for (diff <- difflist; factor <- factors(diff)) { if (factor eq null) throw new Error("unroll returned null Factor") else result += factor } 
    //difflist.foreach(diff => result ++= factors(diff))
    result.toList // TODO is this necessary?
  }
  def factors(variables:Iterable[Variable]) : Iterable[Factor] = {
    //var result = new LinkedHashSet[Factor]()
    var result = new HashSet[Factor]()
    for (v <- variables; factor <- factors(v)) { if (factor eq null) throw new Error("unroll returned null Factor") else result += factor }  //variables.foreach(v => result ++= factors(v))
    result.toList // TODO is this necessary?
  }
  def stats(v:Variable): Iterable[Stat]; // TODO make this Iterable[StatType]
  def score(s:StatType) : Double
  //def scoreStats(ss:Iterable[_<:S]) : Double = ss.foldLeft(0.0)(_ + score(_))
  def statistic(ss:Iterable[StatType]) : StatisticType = new Statistic(ss).asInstanceOf[StatisticType] // TODO is there some way to avoid this cast?
  /** The filename into which to save this factor.  If factorName is not the default, use it, otherwise use the class name. */
  protected def filename: String = if (factorName != defaultFactorName) factorName else this.getClass.getName
  def save(dirname:String): Unit = {}
  def load(dirname:String): Unit = {}
}

/** A Template whose sufficient statistics are represented as a Scalala Vector. 
    @author Andrew McCallum
*/
trait VectorTemplate extends Template {
  val statClasses = new ArrayBuffer[Class[DiscreteValues]] {
    var frozen: Boolean = false;
    def freeze = frozen = true;
    override def ensureSize(s:Int) = if (frozen) throw new IllegalStateException("Template already .init'ed.") else super.ensureSize(s)
  }
  def statDomains : Seq[DiscreteDomain[_]] = {
    if (statClasses.isEmpty) throw new IllegalStateException("You must call .init on this Template before use.")
    statClasses.map(Domain.get[DiscreteValues](_))
  } 
  def freezeDomains : Unit = {
    if (statClasses.isEmpty) throw new IllegalStateException("You must call .init on this Template before use.")
    statClasses.foreach(Domain.get[DiscreteValues](_).freeze)
  }
  // TODO Consider changing name to statSize?
  lazy val statsize : Int = {
    if (statClasses.isEmpty) throw new IllegalStateException("You must call .init on this Template before use.")
    val ss = statClasses.multiplyInts(Domain.get[DiscreteValues](_).allocSize)
    //println("statsize "+ss)
    ss
  } 
  type StatType <: Stat
  trait Stat extends super.Stat {
    def vector : Vector
  }
  override type StatisticType = Statistic
  class Statistic(ss:Iterable[_<:StatType]) extends super.Statistic(ss) {
    lazy val vector : Vector = {
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
  override def statistic(ss:Iterable[StatType]) : StatisticType = new Statistic(ss)
  /** Perform the outer-product of two vectors, to yield */
  protected def flatOuter(vector1: Vector, vector2: Vector) : Vector = vector1 match {
    case v1: SingletonBinaryVector => vector2 match {
      case v2: SingletonBinaryVector =>
        new SingletonBinaryVector(v1.size * v2.size, v1.singleIndex * v2.size + v2.singleIndex)
      case v2: SparseBinaryVector =>
        new SparseBinaryVector(v1.size * v2.size,
             {
              val arr = new Array[Int](v2.activeDomain.size);
         var i = 0;
         for (i2 <- v2.activeDomain) {
           arr(i) = v1.singleIndex * v2.size + i2;
           i += 1;
         };
         arr
             })
    }
    case v1: SparseBinaryVector => vector2 match {
      case v2: SingletonBinaryVector =>
        new SparseBinaryVector(v1.size * v2.size,
                               {
         val arr = new Array[Int](v1.activeDomain.size);
         var i = 0;
         for (i1 <- v1.activeDomain) {
           arr(i) = i1 * v2.size + v2.singleIndex;
           i += 1;
         };
         arr
             })
      case v2: SparseBinaryVector =>
        new SparseBinaryVector(v1.size * v2.size,
             {
         val arr = new Array[Int](v1.activeDomain.size * v2.activeDomain.size);
         var i = 0;
         for (i1 <- v1.activeDomain; i2 <- v2.activeDomain) {
           arr(i) = i1 * v2.size + i2;
           i += 1;
         };
         arr
             })
    }
  }
  
  // TODO implement this!
  private def unflattenOuter(weightIndex:Int, dimensions:Int*) : Array[Int] = new Array[Int](2) 
} // end of VectorTemplate


/** A VectorTemplate that also has a vector of weights, and calculates score by a dot-product between statistics.vector and weights. 
    @author Andrew McCallum */
trait DotTemplate extends VectorTemplate {
  type TemplateType <: DotTemplate
  lazy val weights: Vector = { freezeDomains; new DenseVector(statsize) } // Dense by default, may be override in sub-traits
  def score(s:StatType) = weights match {
    case w:DenseVector => w dot s.vector
    case w:SparseHashVector => w dot s.vector
    case w:SparseVector => w dot s.vector
  }
  override def save(dirname:String): Unit = {
    val f = new File(dirname+"/"+filename)
    if (f.exists) return // Already exists, don't write it again
    for (d <- statDomains) d.save(dirname)
    val s = new PrintWriter(new FileWriter(f))
    for (weight <- weights.iterator) {
      s.print(weight._1)
      s.print(" ")
      s.println(weight._2)
    }
    s.close
  }
  override def load(dirname:String): Unit = {
    //println("Loading "+this.getClass.getName+" from directory "+dirname)
    for (d <- statDomains) d.load(dirname)
    if (statsize <= 0 || scalala.Scalala.norm(weights,1) != 0.0) return // Already have non-zero weights, must already be read.
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
  val nc1 = nm1.erasure
  // TODO create methods like this for all Templates and put abstract version in Template
  def respondsTo[NN](implicit m:Manifest[NN]) = nc1.isAssignableFrom(m.erasure)
  def factors(v:Variable): Iterable[Factor] = {
    // TODO Given the surprise about how slow Manifest <:< was, I wonder how slow this is when there are lots of traits!
    // When I substituted "isAssignable" for HashMap caching in GenericSampler I got 42.8 versus 44.4 seconds ~ 3.7%  Perhaps worth considering?
    if (nc1.isAssignableFrom(v.getClass)) unroll1(v.asInstanceOf[N1]) 
    else Nil
  }
  def unroll1(v:N1): Iterable[Factor] = new Factor(v)
  def _statistics(f:Factor) : Iterable[StatType] = statistics(f.n1)
  def statistics(v1:N1) : Iterable[StatType]
  def stats(v:Variable) = factors(v).flatMap(_statistics(_))
  case class Factor(n1:N1) extends super.Factor with IterableSingle[Factor] {
    def numVariables = 1
    def variable(i:Int) = i match { case 0 => n1; case _ => throw new IndexOutOfBoundsException(i.toString) }
    def statistics : Iterable[StatType] = _statistics(this)
  } 
}
trait Statistics1[S1<:Variable] extends Template {
  case class Stat(s1:S1) extends super.Stat with IterableSingle[Stat]
  type StatType = Stat
}
trait VectorStatistics1[S1<:DiscreteValues] extends VectorTemplate {
  type StatType = Stat
  case class Stat(s1:S1) extends super.Stat with IterableSingle[Stat] {
    def vector : Vector = s1.vector
  } 
  def init(implicit m1:Manifest[S1]) : this.type = { statClasses += m1.erasure.asInstanceOf[Class[DiscreteValues]]; statClasses.freeze; this }  
}
trait DotStatistics1[S1<:DiscreteValues] extends VectorStatistics1[S1] with DotTemplate
abstract class TemplateWithStatistics1[N1<:Variable](implicit nm1:Manifest[N1]) extends Template1[N1]()(nm1) with Statistics1[N1] {
  def statistics(v1:N1): Iterable[Stat] = Stat(v1)
}
abstract class TemplateWithVectorStatistics1[N1<:DiscreteValues](implicit nm1:Manifest[N1]) extends Template1[N1]()(nm1) with VectorStatistics1[N1]  {
  def statistics(v1:N1): Iterable[Stat] = Stat(v1)
  init(nm1)
}
class TemplateWithDotStatistics1[N1<:DiscreteValues](implicit nm1:Manifest[N1]) extends Template1[N1]()(nm1) with DotStatistics1[N1] {
  def statistics(v1:N1): Iterable[Stat] = Stat(v1)
  init(nm1)
}

abstract class Template2[N1<:Variable,N2<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2]) extends Template {
  val nc1 = nm1.erasure
  val nc2 = nm2.erasure
  override def factors(v: Variable): Iterable[Factor] = {
    var ret = new ListBuffer[Factor]
    if (nc1.isAssignableFrom(v.getClass)) ret ++= unroll1(v.asInstanceOf[N1]) 
    if (nc2.isAssignableFrom(v.getClass)) ret ++= unroll2(v.asInstanceOf[N2]) 
    ret
  }
  def unroll1(v:N1): Iterable[Factor] 
  def unroll2(v:N2): Iterable[Factor]
  def _statistics(f:Factor) : Iterable[StatType] = statistics(f.n1, f.n2)
  def statistics(v1:N1, v2:N2) : Iterable[StatType]
  def stats(v:Variable) = factors(v).flatMap(_statistics(_))
  case class Factor(n1:N1, n2:N2) extends super.Factor with IterableSingle[Factor] {
    def numVariables = 2
    def variable(i:Int) = i match { case 0 => n1; case 1 => n2; case _ => throw new IndexOutOfBoundsException(i.toString) }
    def statistics : Iterable[StatType] = _statistics(this)
  } 
}
trait Statistics2[S1<:Variable,S2<:Variable] extends Template {
  case class Stat(s1:S1, s2:S2) extends super.Stat with IterableSingle[Stat]
  type StatType = Stat
}
trait VectorStatistics2[S1<:DiscreteValues,S2<:DiscreteValues] extends VectorTemplate {
  case class Stat(s1:S1, s2:S2) extends super.Stat with IterableSingle[Stat] {
    lazy val vector : Vector = flatOuter(s1.vector, s2.vector)
  } 
  type StatType = Stat
  def init(implicit m1:Manifest[S1], m2:Manifest[S2]) : this.type = { statClasses ++= List(m1.erasure.asInstanceOf[Class[DiscreteValues]], m2.erasure.asInstanceOf[Class[DiscreteValues]]); this }  
}
trait DotStatistics2[S1<:DiscreteValues,S2<:DiscreteValues] extends VectorStatistics2[S1,S2] with DotTemplate
abstract class TemplateWithStatistics2[N1<:Variable,N2<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2]) extends Template2[N1,N2]()(nm1,nm2) with Statistics2[N1,N2] {
  def statistics(v1:N1, v2:N2): Iterable[Stat] = Stat(v1, v2)
}
abstract class TemplateWithVectorStatistics2[N1<:DiscreteValues,N2<:DiscreteValues](implicit nm1:Manifest[N1], nm2:Manifest[N2]) extends Template2[N1,N2]()(nm1,nm2) with VectorStatistics2[N1,N2]  {
  def statistics(v1:N1,v2:N2): Iterable[Stat] = Stat(v1,v2)
  init(nm1, nm2)
}
abstract class TemplateWithDotStatistics2[N1<:DiscreteValues,N2<:DiscreteValues](implicit nm1:Manifest[N1], nm2:Manifest[N2]) extends Template2[N1,N2]()(nm1,nm2) with DotStatistics2[N1,N2]  {
  def statistics(v1:N1,v2:N2): Iterable[Stat] = Stat(v1,v2)
  init(nm1, nm2)
}

abstract class Template3[N1<:Variable,N2<:Variable,N3<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3]) extends Template {
  val nc1 = nm1.erasure
  val nc2 = nm2.erasure
  val nc3 = nm3.erasure
  override def factors(v: Variable): Iterable[Factor] = {
    var ret = new ListBuffer[Factor]
    if (nc1.isAssignableFrom(v.getClass)) ret ++= unroll1(v.asInstanceOf[N1])
    if (nc2.isAssignableFrom(v.getClass)) ret ++= unroll2(v.asInstanceOf[N2])
    if (nc3.isAssignableFrom(v.getClass)) ret ++= unroll3(v.asInstanceOf[N3])
    ret
  }
  def unroll1(v:N1): Iterable[Factor]
  def unroll2(v:N2): Iterable[Factor]
  def unroll3(v:N3): Iterable[Factor]
  def _statistics(f:Factor) : Iterable[StatType] = statistics(f.n1, f.n2, f.n3)
  def statistics(v1:N1, v2:N2, v3:N3) : Iterable[StatType]
  def stats(v:Variable) = factors(v).flatMap(_statistics(_))
  case class Factor(n1:N1, n2:N2, n3:N3) extends super.Factor with IterableSingle[Factor] {
    def numVariables = 3
    def variable(i:Int) = i match { case 0 => n1; case 1 => n2; case 2 => n3; case _ => throw new IndexOutOfBoundsException(i.toString) }
    def statistics : Iterable[StatType] = _statistics(this)
  } 
}
trait Statistics3[S1<:Variable,S2<:Variable,S3<:Variable] extends Template {
  case class Stat(s1:S1, s2:S2, s3:S3) extends super.Stat with IterableSingle[Stat]
  type StatType = Stat
}
trait VectorStatistics3[S1<:DiscreteValues,S2<:DiscreteValues,S3<:DiscreteValues] extends VectorTemplate {
  case class Stat(s1:S1, s2:S2, s3:S3) extends super.Stat with IterableSingle[Stat] {
    lazy val vector : Vector = flatOuter(s1.vector, flatOuter(s2.vector, s3.vector))
  } 
  type StatType = Stat
  def init(implicit m1:Manifest[S1], m2:Manifest[S2], m3:Manifest[S3]) : this.type = { statClasses ++= List(m1.erasure.asInstanceOf[Class[DiscreteValues]], m2.erasure.asInstanceOf[Class[DiscreteValues]], m3.erasure.asInstanceOf[Class[DiscreteValues]]); this }  
}
trait DotStatistics3[S1<:DiscreteValues,S2<:DiscreteValues,S3<:DiscreteValues] extends VectorStatistics3[S1,S2,S3] with DotTemplate
abstract class TemplateWithStatistics3[N1<:Variable,N2<:Variable,N3<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3]) extends Template2[N1,N2]()(nm1,nm2) with Statistics3[N1,N2,N3] {
  def statistics(v1:N1, v2:N2, v3:N3): Iterable[Stat] = Stat(v1, v2, v3)
}
abstract class TemplateWithVectorStatistics3[N1<:DiscreteValues,N2<:DiscreteValues,N3<:DiscreteValues](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3]) extends Template3[N1,N2,N3]()(nm1,nm2,nm3) with VectorStatistics3[N1,N2,N3]  {
  def statistics(v1:N1,v2:N2,v3:N3): Iterable[Stat] = Stat(v1,v2,v3)
  init(nm1, nm2, nm3)
}
abstract class TemplateWithDotStatistics3[N1<:DiscreteValues,N2<:DiscreteValues,N3<:DiscreteValues](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3]) extends Template3[N1,N2,N3]()(nm1,nm2,nm3) with DotStatistics3[N1,N2,N3]  {
  def statistics(v1:N1,v2:N2,v3:N3): Iterable[Stat] = Stat(v1,v2,v3)
  init(nm1, nm2, nm3)
}

abstract class Template4[N1<:Variable,N2<:Variable,N3<:Variable,N4<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3], nm4:Manifest[N4]) extends Template {
  val nc1 = nm1.erasure
  val nc2 = nm2.erasure
  val nc3 = nm3.erasure
  val nc4 = nm4.erasure
  override def factors(v: Variable): Iterable[Factor] = {
    var ret = new ListBuffer[Factor]
    if (nc1.isAssignableFrom(v.getClass)) ret ++= unroll1(v.asInstanceOf[N1])
    if (nc2.isAssignableFrom(v.getClass)) ret ++= unroll2(v.asInstanceOf[N2])
    if (nc3.isAssignableFrom(v.getClass)) ret ++= unroll3(v.asInstanceOf[N3])
    if (nc4.isAssignableFrom(v.getClass)) ret ++= unroll4(v.asInstanceOf[N4])
    ret
  }
  def unroll1(v:N1): Iterable[Factor]
  def unroll2(v:N2): Iterable[Factor]
  def unroll3(v:N3): Iterable[Factor]
  def unroll4(v:N4): Iterable[Factor]
  def _statistics(f:Factor) : Iterable[StatType] = statistics(f.n1, f.n2, f.n3, f.n4)
  def statistics(v1:N1, v2:N2, v3:N3, v4:N4) : Iterable[StatType]
  def stats(v:Variable) = factors(v).flatMap(_statistics(_))
  case class Factor(n1:N1, n2:N2, n3:N3, n4:N4) extends super.Factor with IterableSingle[Factor] {
    def numVariables = 4
    def variable(i:Int) = i match { case 0 => n1; case 1 => n2; case 2 => n3; case 3 => n4; case _ => throw new IndexOutOfBoundsException(i.toString) }
    def statistics : Iterable[StatType] = _statistics(this)
  } 
}
trait Statistics4[S1<:Variable,S2<:Variable,S3<:Variable,S4<:Variable] extends Template {
  case class Stat(s1:S1, s2:S2, s3:S3, s4:S4) extends super.Stat with IterableSingle[Stat] 
  type StatType = Stat
}
trait VectorStatistics4[S1<:DiscreteValues,S2<:DiscreteValues,S3<:DiscreteValues,S4<:DiscreteValues] extends VectorTemplate {
  case class Stat(s1:S1, s2:S2, s3:S3, s4:S4) extends super.Stat with IterableSingle[Stat] {
    lazy val vector : Vector = flatOuter(s1.vector, flatOuter(s2.vector, flatOuter(s3.vector, s4.vector))) // TODO Really?  There isn't a risk of a bad cached value here?
  } 
  type StatType = Stat
  def init(implicit m1:Manifest[S1], m2:Manifest[S2], m3:Manifest[S3], m4:Manifest[S4]) : this.type = { statClasses ++= List(m1.erasure.asInstanceOf[Class[DiscreteValues]], m2.erasure.asInstanceOf[Class[DiscreteValues]], m3.erasure.asInstanceOf[Class[DiscreteValues]], m4.erasure.asInstanceOf[Class[DiscreteValues]]); this }  
}
trait DotStatistics4[S1<:DiscreteValues,S2<:DiscreteValues,S3<:DiscreteValues,S4<:DiscreteValues] extends VectorStatistics4[S1,S2,S3,S4] with DotTemplate
abstract class TemplateWithStatistics4[N1<:Variable,N2<:Variable,N3<:Variable,N4<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3], nm4:Manifest[N4]) extends Template2[N1,N2]()(nm1,nm2) with Statistics4[N1,N2,N3,N4] {
  def statistics(v1:N1, v2:N2, v3:N3, v4:N4): Iterable[Stat] = Stat(v1, v2, v3, v4)
}
abstract class TemplateWithVectorStatistics4[N1<:DiscreteValues,N2<:DiscreteValues,N3<:DiscreteValues,N4<:DiscreteValues](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3], nm4:Manifest[N4]) extends Template4[N1,N2,N3,N4]()(nm1,nm2,nm3,nm4) with VectorStatistics4[N1,N2,N3,N4]  {
  def statistics(v1:N1,v2:N2,v3:N3,v4:N4): Iterable[Stat] = Stat(v1,v2,v3,v4)
  init(nm1, nm2, nm3, nm4)
}
abstract class TemplateWithDotStatistics4[N1<:DiscreteValues,N2<:DiscreteValues,N3<:DiscreteValues,N4<:DiscreteValues](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3], nm4:Manifest[N4]) extends Template4[N1,N2,N3,N4]()(nm1,nm2,nm3,nm4) with DotStatistics4[N1,N2,N3,N4]  {
  def statistics(v1:N1,v2:N2,v3:N3,v4:N4): Iterable[Stat] = Stat(v1,v2,v3,v4)
  init(nm1, nm2, nm3, nm4)
}

