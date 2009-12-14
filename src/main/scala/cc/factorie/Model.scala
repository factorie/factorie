/* Copyright (C) 2008-2009 Univ of Massachusetts Amherst, Computer Science Dept
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
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}
import cc.factorie.util.{Log, ConsoleLogging, LinkedHashSet}
import cc.factorie.util.Implicits._

/** A Model in FACTORIE consists of a collection of factor Templates and methods that operate on the collection.
    Most of these methods are implemented in TemplateList.
    @author Andrew McCallum
    @since 0.8
    @see Template
    @see TemplateList
 */
class Model(templates:Template*) extends TemplateList[Template] {
  //def this() = this(Nil)
  //def this(templates:Template*) = this(templates)

  this ++= templates
  
  def save(dirname:String): Unit = {
    import java.io.File
    //println("Saving model "+getClass.getName+" to "+dirname)
    val f = new File(dirname)
    // Recursively delete all files in directory "f"
    def delete(f:File): Boolean = { if (f.isDirectory) f.listFiles.forall(f2 => delete(f2)) else f.delete }
    if (f.exists) if (!delete(f)) throw new Error("Error deleting directory "+dirname)
    f.mkdir
    this.foreach(_.save(dirname))
  }
 
  def load(dirname:String): Unit = {
    this.foreach(_.load(dirname))
  }
}

/* TODO
 * I will add this trick when we have Scala 2.8, so that we can use package objects to make the implicit conversions nicer.

// These classes, together with the implicit conversions from Template to InitializedTemplate perform a little
// slight of hand in order to automatically initialize templates with their proper Manifests before they are
// appended to a model.  This will no longer be necessary in the future when Scala has trait constructor arguments.
class InitializedTemplate(val template:Template)
class InitializedVectorTemplate1[S1<:DiscreteValues](template:VectorStatistics1[S1])(implicit m1:Manifest[S1]) extends InitializedTemplate(template) {
  template.init(m1)
} 
class InitializedVectorTemplate2[S1<:DiscreteValues,S2<:DiscreteValues](template:VectorStatistics2[S1,S2])(implicit m1:Manifest[S1], m2:Manifest[S2]) extends InitializedTemplate(template) {
  template.init(m1,m2)
} 
class InitializedVectorTemplate3[S1<:DiscreteValues,S2<:DiscreteValues,S3<:DiscreteValues](template:VectorStatistics3[S1,S2,S3])(implicit m1:Manifest[S1], m2:Manifest[S2], m3:Manifest[S3]) extends InitializedTemplate(template) {
  template.init(m1,m2,m3)
} 
class InitializedVectorTemplate4[S1<:DiscreteValues,S2<:DiscreteValues,S3<:DiscreteValues,S4<:DiscreteValues](template:VectorStatistics4[S1,S2,S3,S4])(implicit m1:Manifest[S1], m2:Manifest[S2], m3:Manifest[S3], m4:Manifest[S4]) extends InitializedTemplate(template) {
  template.init(m1,m2,m3,m4)
} 

*/
