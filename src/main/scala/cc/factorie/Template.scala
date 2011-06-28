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
import scala.util.Random
import scala.math
import scala.util.Sorting
import cc.factorie.la._
import cc.factorie.util.Substitutions
import java.io._

object Template {
  var enableCachedStatistics: Boolean = true
}

// Factor Templates are able to create factors in a factor graph on-the-fly as necessary.
// A factor template specifies:
// (1) a description of the arbitrary relationship among its variable neighbors
// (2) a sufficient statistics function that maps those neighbors to the statistics necessary to return a real-valued score
// (3) an aggregator for multiple statistics of the same template
// (4) a function mapping those aggregated statistics to a real-valued score
// (5) optionally, the parameters used in the function to calculate that score;
//     (alternatively the score may be calculated using parameter stored externally to the Template,
//      or in some fixed way without learned parameters).

/** A Factor is a Model because it can return a factor (itself) and a score.
    A Model is not a Factor because Factors *must* be able to list all the variables they touch;
     (this is part of how they are de-duplicated);
     yet a model may only generate Factors on the fly in response to query variables.
    Factors are deduplicated.  Models are not; 
     multiple models may each contribute Factors, all of which define the factor graph.  
    Models can have inner Models, which are used to obtain factors from each.
     Typically all factors from all inner models are summed together.
     But a "Case" model may exist, which simply will not return factors that are not in effect;
     that is, factors from an inner model that are not in effect will never be seen; 
     whereas inner factors may be seen but know to point to the outer factor that knows how to handle them.  
    Factors can have inner factors, which are used to calculate its score; often not summed.
     This is a special case of a Model having inner Models.
    When you ask an inner Factor for its score, it returns the score of the outer Factor to which it contributes.
     (Is this right?  Perhaps not.)
    When you ask an inner Model for its score, it returns its score alone.
    */
/*trait AbstractFactor extends Model with Factor {
  def touches(variable:Variable): Boolean = this.variables.contains(variable) || inner.exists(_.asInstanceOf[AbstractFactor].touches(variable))
  def factors(variables:Iterable[Variable]): Seq[Factor] = if (variables.exists(touches(_))) Seq(this) else Nil
  override def score: Double = 0.0
}*/

/** A single factor in a factor graph.  In other words, a factor
    template packaged with a set of variables neighboring the
    factor.
    @author Andrew McCallum */
trait Factor extends Model with Ordered[Factor] {
  /** The factor template from which this Factor comes. */
  // !!! ??? def template: Template
  /** In some cases a factor "belongs" to some outer factor which uses this inner one as part of its score calculation.
      In this case this inner factor should not also be used for score generation because it would be redundant.
      For example, see method Template{1,2,3,4}.factors() */
  // TODO Change these back to def's
  //var outer: Factor
  //var inner: Seq[Factor]
  def outer: Factor = null
  def outer_=(f:Factor): Unit = throw new Error("Re-assigning Factor.outer not supported.")
  def inner: Seq[Factor] = Nil
  def inner_=(f:Seq[Factor]): Unit = throw new Error("Re-assigning Factor.inner not supported.")
  /** The number of variables neighboring this factor. */
  def numVariables: Int
  def variable(index: Int): Variable
  def statistics: Statistics
  /** Optionally return pre-calculated Statistics.  By default not actually cached, but may be overridden in subclasses. */
  def cachedStatistics: Statistics = statistics
  // The next two methods implement the Model trait
  def touches(variable:Variable): Boolean = this.variables.contains(variable) || inner.exists(_.touches(variable))
  def factors(variables:Iterable[Variable]): Seq[Factor] = if (variables.exists(touches(_))) Seq(this) else Nil
  /** This factors contribution to the unnormalized log-probability of the current possible world. */
  override def score: Double = statistics.score
  /** Returns the collection of variables neighboring this factor. */
  def variables: Seq[Variable] // = { val result = new ArrayBuffer[Variable](numVariables); for (i <- 0 until numVariables) result += variable(i); result }
  def values: Values                   
  /** Randomly selects and returns one of this factor's neighbors. */
  @deprecated def randomVariable(implicit random:Random = cc.factorie.random): Variable = variable(random.nextInt(numVariables))
  /** Return a copy of this factor with some neighbors potentially substituted according to the mapping in the argument. */
  def copy(s:Substitutions): Factor
  // Implement Ordered, such that worst (lowest) scores are considered "high"
  def compare(that: Factor) = {val d = that.score - this.score; if (d > 0.0) 1 else if (d < 0.0) -1 else 0}
  /** In order to two Factors to satisfy "equals", the value returned by this method must by "eq". */
  def equalityPrerequisite: AnyRef = this.getClass
  // Implement equality based on class assignability and Variable contents equality
  //override def canEqual(other: Any) = (null != other) && other.isInstanceOf[Factor]; // TODO Consider putting this back in
  override def equals(other: Any): Boolean = other match {
    case other:Factor =>
      (this eq other) || ((this.equalityPrerequisite eq other.equalityPrerequisite)
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
  def factorName = "Factor"
  override def toString: String = variables.mkString(factorName+"(", ",", ")")
}

/** A container for all the values of the variables neighboring a factor.
    These are necessary to construct a Statistics object. */
trait Values /* extends Product with Ordered[Values] */ {
  // !!! ??? def template: Template
  // def factor: Factor // TODO Consider adding this method
  def outer: Values = null
  def inner: Seq[Values] = Nil
  def statistics: Statistics
  def score: Double = statistics.score
  //def productArity: Int
  //def canEqual(other:Any) = other match { case other:Values => }
}

/** A container for sufficient statistics of a Factor.  
    There is one of these for each Factor. */
trait Statistics {
  // !!! ??? def template: Template
  // def factor: Factor // TODO Consider adding this method
  def outer: Statistics = null
  def inner: Seq[Statistics] = Nil
  def score: Double
}


/** Related factors may be associated with a Family.  
    Those factors may share parameters or other attributes that may be stored in the Family. */
trait Family {
  type FamilyType <: Family // like a self-type
  type FactorType <: Factor
  type ValuesType <: Values
  type StatisticsType <: Statistics
  /** The method responsible for mapping a Statistic object to a real-valued score.  
      Called by the Statistic.score method; implemented here so that it can be easily overriden in user-defined subclasses of Template. */
  def score(s:StatisticsType): Double
  @inline final def score(s:cc.factorie.Statistics): Double = score(s.asInstanceOf[StatisticsType])
  def defaultFactorName = this.getClass.getName
  var factorName: String = defaultFactorName
  /** Assign this Template a name which will be used later when its factors are printed. */
  def setFactorName(n:String): this.type = { factorName = n; this }
  /** Assign this Template a name which will be used later when its factors are printed. */
  def %(n:String): this.type = setFactorName(n) // because % is the comment character in shell languages such as /bin/sh and Makefiles.
  trait Factor extends cc.factorie.Factor { 
    def family: FamilyType = Family.this.asInstanceOf[FamilyType];
    def template: FamilyType = Family.this.asInstanceOf[FamilyType];
    override def statistics: StatisticsType
    override def cachedStatistics: StatisticsType = statistics
    override def factorName = family.factorName
    override def equalityPrerequisite: AnyRef = Family.this
    @deprecated def forSettingsOf(vs:Seq[Variable])(f: =>Unit): Unit = Family.this.forSettingsOf(this.asInstanceOf[FactorType], vs)(f)
  }
  // Values
  trait Values extends cc.factorie.Values {
    def family: FamilyType = Family.this.asInstanceOf[FamilyType]
    def template: FamilyType = Family.this.asInstanceOf[FamilyType];
  }
  // Statistics
  trait Statistics extends cc.factorie.Statistics {
    def family: FamilyType = Family.this.asInstanceOf[FamilyType]
    def template: FamilyType = Family.this.asInstanceOf[FamilyType];
    // TODO Make this non-lazy later, when _statisticsDomains can be initialized earlier
    // Warning: if score gets called too late, might the values of the variables have been changed to something else already?
    lazy val score = Family.this.score(this.asInstanceOf[StatisticsType]) 
    // TODO can we find a way to get rid of this cast?  Yes, use a self-type Stat[This], but too painful
  }
  def statistics(values:ValuesType): StatisticsType
  /** May be overridden in subclasses to actually cache. */
  def cachedStatistics(values:ValuesType, stats:(ValuesType)=>StatisticsType): StatisticsType = stats(values)
  def cachedStatistics(values:ValuesType): StatisticsType = cachedStatistics(values, statistics)
  def clearCachedStatistics: Unit = {}
  
  // Managing settings iteration
  // TODO Replace this with message calculation code that does the settings iteration internally
  def hasSettingsIterator: Boolean = false
  def forSettings(factor:FactorType)(f: =>Unit): Unit = throw new Error("Not supported.")
  def forSettingsOf(factor:FactorType, vs:Seq[Variable])(f: =>Unit): Unit = throw new Error("Not supported.")
  def sparsifySettingsFor(vs:Iterable[Variable]): Unit = throw new Error("Not supported.")
  // New version of settings iteration
  def forSettingStats(factor:FactorType, vs:Seq[Variable])(f: (StatisticsType)=>Unit): Unit = throw new Error("Not supported.") // TODO Also pass variable values?
  // New style for iterating over neighbor value combinations
  def valuesIterator(factor:FactorType, fixed: Assignment): Iterator[Values]

  /** The filename into which to save this factor.  If templateName is not the default, use it, otherwise use the class name. */
  protected def filename: String = factorName
  def save(dirname:String): Unit = {}
  def load(dirname:String): Unit = {}
}



trait FamilyWithNeighborDomains extends Family {
  protected var _neighborDomains: ArrayBuffer[Domain[_]] = null
  protected def _newNeighborDomains = new ArrayBuffer[Domain[_]]
  def neighborDomains: Seq[Domain[_]] = 
    if (_neighborDomains eq null)
      throw new IllegalStateException("You must override neighborDomains if you want to access them before creating any Factor objects.")
    else
      _neighborDomains
}


/** The template for creating factors, given on of its variables, finding the other neighboring variables.
    @Andrew McCallum
*/
trait Template extends FamilyWithNeighborDomains { thisTemplate =>
  type TemplateType <: Template // like a self-type
  type FamilyType <: Template
  //type FactorType <: Factor
  //type ValuesType <: Values
  //type StatisticsType <: Statistics
  /** If true, method "factors" will only create Factors for variables whose domains match neighborDomains. */
  var matchNeighborDomains = true
  /*trait Factor extends super.Factor { 
    def template: TemplateType = Template.this.asInstanceOf[TemplateType];
  }
  trait Values extends super.Values {
    def template: TemplateType = Template.this.asInstanceOf[TemplateType]
  }
  trait Statistics extends super.Statistics {
    def template: TemplateType = Template.this.asInstanceOf[TemplateType];
  }*/
  //def defaultTemplateName = "Factor"
  //var templateName: String = defaultTemplateName
  def factors(v: Variable): Iterable[FactorType] // TODO Consider returning Iterable[Factor]
  /**A version of factors that takes the Diff object instead of just the variable */
  def factors(d: Diff): Iterable[FactorType] = if (d.variable == null) Nil else factors(d.variable)
  def factors(difflist: DiffList): Iterable[FactorType] = {
    //var result = new LinkedHashSet[Factor]()
    var result = new HashSet[FactorType]()
    for (diff <- difflist; factor <- factors(diff)) { if (factor eq null) throw new Error("Template.factors returned null Factor") else result += factor }
    //difflist.foreach(diff => result ++= factors(diff))
    result.toSeq // TODO is this necessary?
  }
  def factors(variables:Iterable[Variable]): Iterable[FactorType] = {
    if (variables.size == 1) return factors(variables.head) // Efficiently avoids the HashSet.
    //var result = new LinkedHashSet[FactorType]()
    var result = new HashSet[FactorType]()
    for (v <- variables; factor <- factors(v)) { if (factor eq null) throw new Error("Template.factors returned null Factor") else result += factor }
    result.toSeq // TODO is this necessary?
  }
  /** Called in implementations of factors(Variable) to give the variable a chance
      to specify additional dependent variables on which factors(Variable) should also be called. */
  def unrollCascade(v:Variable): Iterable[Variable] = v.unrollCascade
}

/** A factor Family whose sufficient statistics are represented as a set of DiscreteVectorValues
    (which inherit from cc.factorie.la.Vector, and also have a DiscreteVectorDomain).
    @author Andrew McCallum
*/
trait VectorFamily extends Family {
  //def vectorLength: Int
  //protected var _vectorLength1 = -1
  //def vectorLength1: Int = if (_vectorLength < 0) throw new Error("Not yet set.") else _vectorLength1
  protected var _statisticsDomains: ArrayBuffer[DiscreteVectorDomain] = null
  protected def _newStatisticsDomains = new ArrayBuffer[DiscreteVectorDomain]
  def statisticsDomains: Seq[DiscreteVectorDomain] = 
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
  //override def statistics(v:Variable): StatisticsType = new Stats(factors(v).map(_.stats).flatten)
  // TODO implement this!
  private def unflattenOuter(weightIndex:Int, dimensions:Int*): Array[Int] = new Array[Int](2)
}

trait VectorTemplate extends VectorFamily with Template


/** A VectorTemplate that also has a vector of weights, and calculates score by a dot-product between statistics.vector and weights.
    @author Andrew McCallum */
trait DotFamily extends VectorFamily {
  type TemplateType <: DotFamily
  type FamilyType <: DotFamily
  lazy val weights: Vector = { freezeDomains; new DenseVector(statisticsVectorLength) } // Dense by default, may be override in sub-traits
  def score(s:StatisticsType) = if (s eq null) 0.0 else weights match {
    case w:DenseVector => { w dot s.vector }
    //case w:SparseHashVector => w dot s.vector // TODO Uncomment this.  It was only commented because latest version of scalala didn't seem to have this class any more
    case w:SparseVector => w dot s.vector
  }

  override def save(dirname:String): Unit = {
    val f = new File(dirname+"/"+filename) // TODO Make this work on MSWindows also
    if (f.exists) return // Already exists, don't write it again
    for (d <- statisticsDomains) d.save(dirname)
    val writer = new PrintWriter(new FileWriter(f))
    saveToWriter(writer)
  }

  def saveToWriter(writer:PrintWriter): Unit = {
    // TODO Do we also need to save the weights.default?
    for (weight <- weights.activeElements; if (weight._2 != 0.0)) { // before June 21 2010, used too be weights.iterator -akm
      writer.print(weight._1)
      writer.print(" ")
      writer.println(weight._2)
    }
    writer.close
  }

  override def load(dirname:String): Unit = {
    //println("Loading "+this.getClass.getName+" from directory "+dirname)
    for (d <- statisticsDomains) { /* println(" Loading Domain["+d+"]"); */ d.load(dirname) }
    val f = new File(dirname+"/"+filename)
    val reader = new BufferedReader(new FileReader(f))
    loadFromReader(reader)
  }

  def loadFromReader(reader:BufferedReader): Unit = {
    // TODO Why would statisticsVectorLength be 0 or negative?
    if (statisticsVectorLength <= 0 || weights.activeElements.exists({case(i,v) => v != 0})) return // Already have non-zero weights, must already be read.
    var line = ""
    while ({ line = reader.readLine; line != null }) {
      val fields = line.split(" +")
      assert(fields.length == 2)
      val index = Integer.parseInt(fields(0))
      val value = java.lang.Double.parseDouble(fields(1))
      weights(index) = value
    }
    reader.close
  }
}

trait DotTemplate extends DotFamily with Template {
  type TemplateType <: DotTemplate
  type FamilyType <: DotTemplate
}

/** A DotTemplate that stores its parameters in a Scalala SparseVector instead of a DenseVector
    @author Andrew McCallum */
trait SparseWeights extends DotFamily {
  override lazy val weights: Vector = { new SparseVector(statisticsVectorLength) } // Dense by default, here overridden to be sparse
}

/** A DotTemplate that stores its parameters in a Scalala SparseHashVector instead of a DenseVector
    @author Sameer Singh */
trait SparseHashWeights extends DotFamily {
  override lazy val weights: Vector = { freezeDomains; new SparseHashVector(statisticsVectorLength) } // Dense by default, override to be sparseHashed
}




// Shortcuts for templates whose statistics are a subset of their neighbors, coming from the end of the neighbor list.
// AKM: Let me know if you think it would be more sensible to have them come from the beginning instead.

abstract class Template2WithStatistics1[N1<:Variable:Manifest,N2<:Variable:Manifest] extends Template2[N1,N2] with Statistics1[N2#Value] {
  def statistics(v:Values) = Stat(v._2)
}
abstract class Template3WithStatistics1[N1<:Variable:Manifest,N2<:Variable:Manifest,N3<:Variable:Manifest] extends Template3[N1,N2,N3] with Statistics1[N3#Value] {
  def statistics(v:Values) = Stat(v._3)
}
abstract class Template3WithStatistics2[N1<:Variable:Manifest,N2<:Variable:Manifest,N3<:Variable:Manifest] extends Template3[N1,N2,N3] with Statistics2[N2#Value,N3#Value] {
  def statistics(v:Values) = Stat(v._2, v._3)
}

/*
trait FooTemplate3 {
  def valuesIterator(value1:Option[N1#Value], value2:Option[N2#Value], value3:Option[N3#Value]): Iterator[this.type.Values]
  def discreteValuesIterator(valueOptions: Option[DiscreteValue]*): Iterator[Values]
  def discreteValuesIterator(factor:Factor, fixed: (Variable,Any)*): Iterator[Values]
  def discreteValuesIterator(factor:Factor, vary:Iterable[DiscreteVar]): Iterator[Values]
  //def argMaxMessage(f:Factor, m1:N1#MessageType, m2:N2#MessageType)
}
*/

//class TreeTemplate extends Template1[Vars[EdgePresence]] { }

//class FooTemplate {}
//val t1 = new FooTemplate
//t1.statistics(values:t1.Values)
