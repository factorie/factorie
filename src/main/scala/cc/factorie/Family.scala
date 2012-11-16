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

// TODO Is this used?  Should it be? Perhaps yes.  TensorFamily extends Statistics[Tensor] -akm
trait Statistics[A] extends Family {
  type FamilyType <: Statistics[A]
  type StatisticsType = A
}

/** A Family whose Factors have statistics that are Tensors. */
trait TensorFamily extends Family {
  type FamilyType <: TensorFamily
  type StatisticsType = Tensor
  //trait Statistics extends super.Statistics { def tensor: Tensor }
}

/** A Family whose Factors have scores calculated as a dot-product between sufficient statistics Tensors and the Family's weights Tensor. */
trait DotFamily extends TensorFamily {
  type FamilyType <: DotFamily
  def weights: Tensor
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

import cc.factorie.util._
class DotFamilyCubbie(val family:DotFamily) extends Cubbie {
  val weights = AnySlot[Tensor]("weights")
  weights := family.weights
}


