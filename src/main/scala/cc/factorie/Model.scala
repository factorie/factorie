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
//import scalala.tensor.Vector
//import scalala.tensor.dense.DenseVector
//import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}
//import cc.factorie.la._
import cc.factorie.util.{Log}

/** A Model in FACTORIE consists of a collection of factor Templates and methods that operate on the collection.
    @author Andrew McCallum
    @since 0.8
    @see Template
 */
class Model(templates:Template*) extends ArrayBuffer[Template] {
  type T = Template
  this ++= templates

  def templatesOf[T2<:T](implicit m:Manifest[T2]) : IndexedSeq[T2] = {
    val templateClass = m.erasure
    val ret = new ArrayBuffer[T2]
    for (t <- this) if (templateClass.isAssignableFrom(t.getClass)) ret += t.asInstanceOf[T2]
    ret
  }
  def templatesOfClass[T2<:T](cls:Class[T2]): IndexedSeq[T2] = {
    val ret = new ArrayBuffer[T2]
    for (t <- this) if (cls.isAssignableFrom(t.getClass)) ret += t.asInstanceOf[T2]
    ret
  }
  /*override def filter(test:(T)=>Boolean): Seq[T2] = {
    val ret = new Model
    for (t <- this) if (test(t)) ret += t
    ret
  }*/
  def factors(d:DiffList) : Seq[Factor] = if (d.size == 0) Nil else this.flatMap(template => template.factors(d))
  def factorsOf[T2<:T](d:DiffList)(implicit m:Manifest[T2]) : Seq[T2#Factor] = if (d.size == 0) Nil else this.templatesOf[T2](m).flatMap(template => template.factors(d))
  def factorsOf[T2<:T](cls:Class[T2])(d:DiffList): Seq[T2#Factor] = if (d.size == 0) Nil else this.templatesOfClass[T2](cls).flatMap(template => template.factors(d))
  def factorsOf[T2<:T](vs:Iterable[Variable])(implicit m:Manifest[T2]) : Seq[T2#Factor] = this.templatesOf[T2](m).flatMap(template => template.factors(vs))
  def factorsOf[T2<:T](v:Variable)(implicit m:Manifest[T2]) : Seq[Factor] = this.templatesOf[T2](m).flatMap(template => template.factors(v))
  /** Given a variable, return a collection of Factors that touch it.  Note that combining these results for multiple variables may result in duplicate Factors. */
  def factors(v:Variable) : List[Factor] = this.flatMap(template => template.factors(v)).toList
  def factors(vs:Iterable[Variable]) : Seq[Factor] = this.flatMap(template => template.factors(vs))
  def score(d:DiffList) : Double = factors(d).foldLeft(0.0)(_+_.statistic.score)
  def score1(v:Variable) : Double = factors(v).foldLeft(0.0)(_+_.statistic.score) // For use when the Variable is also Iterable
  def score(v:Variable) : Double = factors(v).foldLeft(0.0)(_+_.statistic.score)
  def score(vars:Iterable[Variable]) : Double = factors(vars).foldLeft(0.0)(_+_.statistic.score)
  /** Score all variables in the Iterable collection.  This method is useful when a Variable is also a Iterable[Variable]; 
      it forces the Iterable interpretation and avoids the single variable interpretation of score(Variable). */
  def scoreAll(vars:Iterable[Variable]) : Double = factors(vars).foldLeft(0.0)(_+_.statistic.score)
  /** Returns the average score, that is scoreAll of vars, normalized by the size of the collections vars. */
  def aveScore(vars:Collection[Variable]): Double = scoreAll(vars) / vars.size

  
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
class InitializedVectorTemplate1[S1<:DiscreteVars](template:VectorStatistics1[S1])(implicit m1:Manifest[S1]) extends InitializedTemplate(template) {
  template.init(m1)
} 
class InitializedVectorTemplate2[S1<:DiscreteVars,S2<:DiscreteVars](template:VectorStatistics2[S1,S2])(implicit m1:Manifest[S1], m2:Manifest[S2]) extends InitializedTemplate(template) {
  template.init(m1,m2)
} 
class InitializedVectorTemplate3[S1<:DiscreteVars,S2<:DiscreteVars,S3<:DiscreteVars](template:VectorStatistics3[S1,S2,S3])(implicit m1:Manifest[S1], m2:Manifest[S2], m3:Manifest[S3]) extends InitializedTemplate(template) {
  template.init(m1,m2,m3)
} 
class InitializedVectorTemplate4[S1<:DiscreteVars,S2<:DiscreteVars,S3<:DiscreteVars,S4<:DiscreteVars](template:VectorStatistics4[S1,S2,S3,S4])(implicit m1:Manifest[S1], m2:Manifest[S2], m3:Manifest[S3], m4:Manifest[S4]) extends InitializedTemplate(template) {
  template.init(m1,m2,m3,m4)
} 

*/
