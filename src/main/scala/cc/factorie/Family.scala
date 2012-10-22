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

import scala.collection.mutable.ArrayBuffer
import cc.factorie.la._
import cc.factorie.util.IntSeq
import java.io._
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

trait Family {
  type FamilyType <: Family // like a self-type
  type FactorType <: Factor // TODO Remove this?
  type StatisticsType <: Any
  type NeighborType1
  @inline final def thisFamily: this.type = this
  def defaultFactorName = this.getClass.getName
  var factorName: String = defaultFactorName
  /** Assign this Template a name which will be used later when its factors are printed. */
  def setFactorName(n:String): this.type = { factorName = n; this }
  /** Assign this Template a name which will be used later when its factors are printed. */
  final def %(n:String): this.type = setFactorName(n) // because % is the comment character in shell languages such as /bin/sh and Makefiles.
  trait Factor extends cc.factorie.Factor {
    type StatisticsType = Family.this.StatisticsType
    def family: FamilyType = Family.this.asInstanceOf[FamilyType];
    def _1: NeighborType1 // TODO Consider getting rid of this.
    override def factorName = family.factorName
    override def equalityPrerequisite: AnyRef = Family.this
    override def valuesScore(tensor:Tensor): Double = Family.this.valuesScore(tensor)
    def statisticsScore(tensor:Tensor): Double = Family.this.statisticsScore(tensor)
  }
  def valuesScore(tensor:Tensor): Double = throw new Error("Not yet implemented.")
  def statisticsScore(tensor:Tensor): Double = throw new Error("Not yet implemented.")
  /** The filename into which to save this Family. */
  protected def filename: String = factorName
  def save(dirname:String, gzip: Boolean = false): Unit = {}
  def load(dirname:String, gzip: Boolean = false): Unit = {}
  def loadFromJar(dirname:String): Unit = throw new Error("Unsupported")
}

trait FamilyWithNeighborDomains extends Family {
  def neighborDomains: Seq[Domain[_]]
}

trait FamilyWithNeighborClasses extends Family {
  def neighborClasses: Seq[Class[_]]
}

trait Statistics[A] extends Family {
  type FamilyType <: Statistics[A]
  type StatisticsType = A
}

//trait TensorStatistics extends TensorFamily
// TODO Rename to FamilyWithTensorStatistics?
trait TensorFamily extends Family {
  type FamilyType <: TensorFamily
  type StatisticsType = Tensor
  //trait Statistics extends super.Statistics { def tensor: Tensor }
}

// TODO Rename to FamilyWithDotStatistics
// TODO No, consider renaming this to simply DotStatistics?  E.g. Look at ChainNER2
//  Or alternatively, have Family2WithDotStatistics[N1,N2]
//trait DotStatistics extends DotFamily
trait DotFamily extends TensorFamily {
  type FamilyType <: DotFamily
  def weights: Tensor
  //@inline final def score(s:StatisticsType): Double = if (s eq null) 0.0 else statisticsScore(s.tensor)
  @inline final override def statisticsScore(t:Tensor): Double = weights dot t

  override def save(dirname:String, gzip: Boolean = false): Unit = {
    val f = new File(dirname + "/" + filename + { if (gzip) ".gz" else "" }) // TODO: Make this work on MSWindows also
    if (f.exists) return // Already exists, don't write it again
    // for (d <- statisticsDomainsSeq) d.save(dirname) // TODO!!! These must now be saved by the user!
    val writer = new PrintWriter(new BufferedOutputStream({ if (gzip) new GZIPOutputStream(new BufferedOutputStream(new FileOutputStream(f))) else new FileOutputStream(f) }))
    saveToWriter(writer)
  }

  def saveToWriter(writer:PrintWriter): Unit = {
    // TODO Do we also need to save the weights.default?
    for (weight <- weights.activeElements; if (weight._2 != 0.0)) {
      writer.print(weight._1)
      writer.print(" ")
      writer.println(weight._2)
    }
    writer.close
  }

  override def load(dirname: String, gzip: Boolean = false): Unit = {
    // for (d <- statisticsDomainsSeq) d.load(dirname) // TODO!!! These must now be saved by the user!
    val f = new File(dirname + "/" + filename + { if (gzip) ".gz" else "" })
    val reader = new BufferedReader(new InputStreamReader({ if (gzip) new GZIPInputStream(new BufferedInputStream(new FileInputStream(f))) else new FileInputStream(f) }))
    loadFromReader(reader)
    reader.close()
  }

  def loadFromReader(reader: BufferedReader): Unit = {
    // TODO: consider verifying the weights match the saved model --brian
    if (weights.activeElements.exists({case(i,v) => v != 0})) return // Already have non-zero weights, must already be read.
    var line = ""
    while ({ line = reader.readLine; line != null }) {
      val fields = line.split(" +")
      assert(fields.length == 2)
      val index = Integer.parseInt(fields(0))
      val value = java.lang.Double.parseDouble(fields(1))
      weights(index) = value
    }
    reader.close()
  }

  // does not support gzipped models
  override def loadFromJar(dirname: String): Unit = {
    val cl = this.getClass.getClassLoader
    def readerFromResourcePath(path: String) =
      new BufferedReader(new InputStreamReader(cl.getResourceAsStream(path)))
    // for (d <- statisticsDomainsSeq.map(_.dimensionDomain)) d.loadFromReader(readerFromResourcePath(dirname + "/" + d.filename)) // TODO!!! These must now be loaded by the user!
    loadFromReader(readerFromResourcePath(dirname + "/" + filename))
  }

}


///** Related factors may be associated with a Family.
//    Those factors may share parameters or other attributes that may be stored in the Family. */
//trait Family {
//  type FamilyType <: Family // like a self-type
//  type FactorType <: Factor
////  type ValuesType <: cc.factorie.Values
////  type Values = ValuesType
//  type StatisticsType <: Statistics
//  type NeighborType1
//  @inline final def thisFamily: this.type = this
//  /** The method responsible for mapping a Statistic object to a real-valued score.  
//      Called by the Statistic.score method; implemented here so that it can be easily overriden in user-defined subclasses of Template. */
//  def score(s:StatisticsType): Double
//  //@inline final def score(s:cc.factorie.Statistics): Double = score(s.asInstanceOf[StatisticsType])
//  def defaultFactorName = this.getClass.getName
//  var factorName: String = defaultFactorName
//  /** Assign this Template a name which will be used later when its factors are printed. */
//  def setFactorName(n:String): this.type = { factorName = n; this }
//  /** Assign this Template a name which will be used later when its factors are printed. */
//  def %(n:String): this.type = setFactorName(n) // because % is the comment character in shell languages such as /bin/sh and Makefiles.
//  trait Factor extends cc.factorie.Factor {
//    def family: FamilyType = Family.this.asInstanceOf[FamilyType];
//    //def family = thisFamily
//    def _1: NeighborType1
////    override def values: ValuesType
//    def statistics: StatisticsType // = statistics(values)
////    override def cachedStatistics: StatisticsType = thisFamily.cachedStatistics(values)
//    override def factorName = family.factorName
//    override def equalityPrerequisite: AnyRef = Family.this
//    //@deprecated def forSettingsOf(vs:Seq[Variable])(f: =>Unit): Unit = Family.this.forSettingsOf(this.asInstanceOf[FactorType], vs)(f)
//  }
//  // Values
//  /*trait Values extends cc.factorie.Values {
//    def family: FamilyType = Family.this.asInstanceOf[FamilyType]
//  }*/
//  // Statistics
//  trait Statistics extends cc.factorie.Statistics {
//    def family: FamilyType = Family.this.asInstanceOf[FamilyType]
//    //def family = thisFamily
//    // TODO Make this non-lazy later, when _statisticsDomains can be initialized earlier
//    // Warning: if score gets called too late, might the values of the variables have been changed to something else already?
//    //lazy val score = Family.this.score(this.asInstanceOf[StatisticsType]) 
//    // TODO can we find a way to get rid of this cast?  Yes, use a self-type Stat[This], but too painful
//  }
////  def statistics(values:ValuesType): StatisticsType
//  /** May be overridden in subclasses to actually cache. */
//  //def cachedStatistics(values:ValuesType, stats:(ValuesType)=>StatisticsType): StatisticsType = stats(values)
////  def cachedStatistics(values:ValuesType): StatisticsType = statistics(values)
////  def clearCachedStatistics: Unit = {}
//  
//  /** The filename into which to save this Family. */
//  protected def filename: String = factorName
//  def save(dirname:String, gzip: Boolean = false): Unit = {}
//  def load(dirname:String, gzip: Boolean = false): Unit = {}
//  def loadFromJar(dirname:String): Unit = { throw new Error("Unsupported") }
//}
//
//
//
//trait FamilyWithNeighborDomains extends Family {
//  def neighborDomains: Seq[Domain[_]] 
//}
//
//
///** A factor Family whose sufficient statistics are represented as a Tensor.
//    (Formerly: DiscreteVectorValues which inherit from cc.factorie.la.Vector, and also have a DiscreteVectorDomain).
//    @author Andrew McCallum
//*/
//trait TensorFamily extends Family {
//  type FamilyType <: TensorFamily
//  //def vectorLength: Int
//  //protected var _vectorLength1 = -1
//  //def vectorLength1: Int = if (_vectorLength < 0) throw new Error("Not yet set.") else _vectorLength1
//  //protected var _statisticsDomains: ArrayBuffer[DiscreteTensorDomain] = null
//  //protected def _newStatisticsDomains = new ArrayBuffer[DiscreteTensorDomain]
//  //protected var _tensorDims: IntSeq
//  //def tensorDims: Seq[Int]
////  def statisticsDomains: Product // [Domain[_]]  // Seq[DiscreteTensorDomain]
//////    = if (_statisticsDomains eq null)
//////      throw new IllegalStateException("You must override statisticsDomains if you want to access them before creating any Factor and Stat objects.")
//////    else
//////      _statisticsDomains
////  lazy val statisticsDomainsSeq: Seq[DiscreteTensorDomain] = statisticsDomains.productIterator.map(_.asInstanceOf[DiscreteTensorDomain]).toSeq
////  private var _frozenDomains = false
////  def freezeDomains: Unit = { statisticsDomainsSeq.foreach(_.freeze); _frozenDomains = true }
////  //lazy val statisticsTensorDimensions: Array[Int] = { freezeDomains; statisticsDomainsSeq.map(_.dimensionSize).toArray }
////  private var __statisticsTensorDimensions: Array[Int] = null
////  def statisticsTensorDimensions: Array[Int] = if (__statisticsTensorDimensions eq null) this match { 
////    case s:Statistics1[_] => Array(0)
////    case s:TensorStatistics1[_] => Array(0) // TODO Try to make TensorStatistics1 inherit from Statistics1
////    case s:Statistics2[_,_] => Array(0,0)
////    case s:TensorStatistics2[_,_] => Array(0,0)
////    case s:Statistics3[_,_,_] => Array(0,0,0)
////    case s:TensorStatistics3[_,_,_] => Array(0,0,0)
////    case s:Statistics4[_,_,_,_] => Array(0,0,0,0)
////    case s:TensorStatistics4[_,_,_,_] => Array(0,0,0,0)
////  } else __statisticsTensorDimensions
////  // xxx ?var statisticsTensorDimensions: Array[Int] = null
//  type StatisticsType <: Statistics
//  trait Statistics extends super.Statistics {
//    def tensor: Tensor
////    if (__statisticsTensorDimensions eq null) __statisticsTensorDimensions = tensor.dimensions
//    //println("TensorFamily.Statistics init "+TensorFamily.this.getClass.getName+" "+TensorFamily.this.factorName+" "+statisticsTensorDimensions.toList)
//  }
//}
//
//
///** A TensorFamily that also has a vector of weights, and calculates score by a dot-product between statistics.vector and weights.
//    @author Andrew McCallum */
//trait DotFamily extends TensorFamily {
//  //type TemplateType <: DotFamily
//  type FamilyType <: DotFamily
////  private var _weights: Tensor = null
//  //lazy val defaultWeights = { freezeDomains; newWeightsTypeTensor } // What is the use-case here? -akm
//  def weights: Tensor // = { if (_weights != null) _weights else setWeights(newWeightsTypeTensor); _weights }
////  def setWeights(w: Tensor): Unit = _weights = w
////  //def newWeightsTypeTensor(default:Double = 0.0): Tensor = Tensor.dense(statisticsTensorDimensions)
////  def newWeightsTypeTensor: Tensor = Tensor.newDense(statisticsTensorDimensions)  // Dense by default, may be override in sub-traits
////  def newDenseTensor: Tensor = Tensor.newDense(weights)
////  def newSparseTensor: Tensor = Tensor.newSparse(weights)
//  @inline final def score(s:StatisticsType): Double = if (s eq null) 0.0 else statisticsScore(s.tensor)
//  @inline final def statisticsScore(t:Tensor): Double = {
////    if (!weights.dimensionsMatch(t)) {
////      require(weights.oneNorm == 0.0)
////      println("DotFamily.statisticsScore re-allocating weights Tensor.")
////      _weights = newWeightsTypeTensor
////    }
//    weights dot t
//  }
//
//  override def save(dirname:String, gzip: Boolean = false): Unit = {
//    val f = new File(dirname + "/" + filename + { if (gzip) ".gz" else "" }) // TODO: Make this work on MSWindows also
//    if (f.exists) return // Already exists, don't write it again
//    // for (d <- statisticsDomainsSeq) d.save(dirname) // TODO!!! These must now be saved by the user!
//    val writer = new PrintWriter(new BufferedOutputStream({
//      if (gzip)
//        new GZIPOutputStream(new BufferedOutputStream(new FileOutputStream(f)))
//      else
//        new FileOutputStream(f)
//    }))
//    saveToWriter(writer)
//  }
//
//  def saveToWriter(writer:PrintWriter): Unit = {
//    // TODO Do we also need to save the weights.default?
//    for (weight <- weights.activeElements; if (weight._2 != 0.0)) {
//      writer.print(weight._1)
//      writer.print(" ")
//      writer.println(weight._2)
//    }
//    writer.close
//  }
//
//  override def load(dirname: String, gzip: Boolean = false): Unit = {
//    // for (d <- statisticsDomainsSeq) d.load(dirname) // TODO!!! These must now be saved by the user!
//    val f = new File(dirname + "/" + filename + { if (gzip) ".gz" else "" })
//    val reader = new BufferedReader(new InputStreamReader({
//      if (gzip)
//        new GZIPInputStream(new BufferedInputStream(new FileInputStream(f)))
//      else
//        new FileInputStream(f)
//    }))
//    loadFromReader(reader)
//    reader.close()
//  }
//
//  def loadFromReader(reader: BufferedReader): Unit = {
//    // TODO: consider verifying the weights match the saved model --brian
//    if (weights.activeElements.exists({case(i,v) => v != 0})) return // Already have non-zero weights, must already be read.
//    var line = ""
//    while ({ line = reader.readLine; line != null }) {
//      val fields = line.split(" +")
//      assert(fields.length == 2)
//      val index = Integer.parseInt(fields(0))
//      val value = java.lang.Double.parseDouble(fields(1))
//      weights(index) = value
//    }
//    reader.close()
//  }
//
//  // does not support gzipped models
//  override def loadFromJar(dirname: String): Unit = {
//    val cl = this.getClass.getClassLoader
//    def readerFromResourcePath(path: String) =
//      new BufferedReader(new InputStreamReader(cl.getResourceAsStream(path)))
//
//    // for (d <- statisticsDomainsSeq.map(_.dimensionDomain)) d.loadFromReader(readerFromResourcePath(dirname + "/" + d.filename)) // TODO!!! These must now be loaded by the user!
//    loadFromReader(readerFromResourcePath(dirname + "/" + filename))
//  }
//
//}
//

///** A DotTemplate that stores its parameters in a Scalala SparseTensor instead of a DenseTensor
//    @author Andrew McCallum */
//trait SparseWeights extends DotFamily {
//  override def newWeightsTypeTensor: Tensor = {
//    if (statisticsTensorDimensions eq null) throw new Error("statisticsTensorDimensions not yet set on Family "+getClass.getName+" "+factorName)
//    Tensor.newSparse(statisticsTensorDimensions)
//  }
//  //override def newWeightsTypeTensor(defaultVal:Double): Tensor = new SparseVector(statisticsVectorLength) { default = defaultVal }
//}

/** A DotTemplate that stores its parameters in a SparseHashVector instead of a DenseVector
    @author Sameer Singh */
//trait SparseHashWeights extends DotFamily {
//  override def newWeightsTypeVector(defaultVal:Double): Vector =
//    new SparseHashVector(statisticsVectorLength) { default = defaultVal }
//}


