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
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}
import cc.factorie.util.{Log, LinkedHashSet}
import cc.factorie.util.Implicits._

// TODO Consider simply moving these methods to Model and deleting this class

/** Management of the collection of Factor Templates within the Model. 
    @author Andrew McCallum */
class TemplateList[T<:Template] extends ArrayBuffer[T] {
  def templatesOf[T2<:T](implicit m:Manifest[T2]) : TemplateList[T2] = {
      val templateClass = m.erasure
      val ret = new TemplateList[T2]
   for (t <- this) if (templateClass.isAssignableFrom(t.getClass)) ret += t.asInstanceOf[T2]
   ret
  }
  override def filter(test:(T)=>Boolean) : TemplateList[T] = {      
    val ret = new TemplateList[T]
    for (t <- this) if (test(t)) ret += t
    ret
  }
  def factors(d:DiffList) : Seq[Factor] = if (d.size == 0) Nil else this.flatMap(template => template.factors(d))
  def factorsOf[T2<:T](d:DiffList)(implicit m:Manifest[T2]) : Seq[T2#Factor] = if (d.size == 0) Nil else this.templatesOf[T2](m).flatMap(template => template.factors(d))
  def factorsOf[T2<:T](vs:Iterable[Variable])(implicit m:Manifest[T2]) : Seq[T2#Factor] = this.templatesOf[T2](m).flatMap(template => template.factors(vs))
  def factorsOf[T2<:T](v:Variable)(implicit m:Manifest[T2]) : Seq[Factor] = this.templatesOf[T2](m).flatMap(template => template.factors(v))
  /** Given a variable, return a collection of Factors that touch it.  Note that combining these results for multiple variables may result in duplicate Factors. */
  def factors(v:Variable) : Seq[Factor] = this.flatMap(template => template.factors(v)).toList
  def factors(vs:Iterable[Variable]) : Seq[Factor] = this.flatMap(template => template.factors(vs))
  /* This kind of construction should now be done in a Lattice, not touching the Variables themselves.
  def registerFactorsInVariables(variables: Iterable[Variable with FactorList]): Seq[Factor] = {
    val factors = this.factors(variables)
    // Make sure each variables factor list starts empty
    variables.foreach(_.clearFactors)
    // Add relevant factors to each relevant neighboring variable
    factors.foreach(_.addToVariables)
    factors.toSeq
  }*/
  def score(d:DiffList) : Double = factors(d).foldLeft(0.0)(_+_.statistic.score)
  def score1(v:Variable) : Double = factors(v).foldLeft(0.0)(_+_.statistic.score) // For use when the Variable is also Iterable
  def score(v:Variable) : Double = factors(v).foldLeft(0.0)(_+_.statistic.score)
  def score(vars:Iterable[Variable]) : Double = factors(vars).foldLeft(0.0)(_+_.statistic.score)
  /** Score all variables in the Iterable collection.  This method is useful when a Variable is also a Iterable[Variable]; 
      it forces the Iterable interpretation and avoids the single variable interpretation of score(Variable). */
  def scoreAll(vars:Iterable[Variable]) : Double = factors(vars).foldLeft(0.0)(_+_.statistic.score)
  /** Returns the average score, that is scoreAll of vars, normalized by the size of the collections vars. */
  def aveScore(vars:Collection[Variable]): Double = scoreAll(vars) / vars.size
}

