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
import scala.reflect.Manifest
import scala.util.Random
import scala.Math
import scala.util.Sorting

/** A Model in FACTORIE consists of a collection of factor Templates and methods that operate on the collection.
    @author Andrew McCallum
    @since 0.8
    @see Template
 */
class Model(initTemplates:InitializedTemplate*) extends Seq[Template] {
  private val ts = new ArrayBuffer[Template]
  def apply(i:Int) = ts.apply(i)
  def length = ts.length
  def iterator = ts.iterator

  type T = Template
  this ++= initTemplates

  // Jumping through hoops just to call automatically call .init on templates that are added.
  // This in turn is just a work-around for the fact that we can't get Manifests for traits because traits cannot take constructor arguments.
  def ++=(iTemplates:Iterable[InitializedTemplate]) = ts ++= iTemplates.map(_.template)
  def +=(iTemplate:InitializedTemplate) = ts += iTemplate.template

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
  def factorsOf[T2<:T](v:Variable)(implicit m:Manifest[T2]) : Seq[T2#Factor] = this.templatesOf[T2](m).flatMap(template => template.factors(v))
  /** Given a variable, return a collection of Factors that touch it.  Note that combining these results for multiple variables may result in duplicate Factors. */
  def factors(v:Variable) : Seq[Factor] = this.flatMap(template => template.factors(v)) //.toList
  def factors(vs:Iterable[Variable]) : Seq[Factor] = this.flatMap(template => template.factors(vs))
  def score(d:DiffList) : Double = factors(d).foldLeft(0.0)(_+_.statistics.score)
  def score1(v:Variable) : Double = factors(v).foldLeft(0.0)(_+_.statistics.score) // For use when the Variable is also Iterable
  def score(v:Variable) : Double = factors(v).foldLeft(0.0)(_+_.statistics.score)
  def score(vars:Iterable[Variable]) : Double = factors(vars).foldLeft(0.0)(_+_.statistics.score)
  /** Score all variables in the Iterable collection.  This method is useful when a Variable is also a Iterable[Variable]; 
      it forces the Iterable interpretation and avoids the single variable interpretation of score(Variable). */
  def scoreAll(vars:Iterable[Variable]) : Double = factors(vars).foldLeft(0.0)(_+_.statistics.score)
  /** Returns the average score, that is scoreAll of vars, normalized by the size of the collections vars. */
  def aveScore(vars:Iterable[Variable]): Double = scoreAll(vars) / vars.size

  
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
