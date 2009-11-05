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

// Management of Factor Templates within the Model
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
	/** Given a variable, return a collection of Factors that touch it.  Note that combining these results for multiple variables may result in duplicate Factors. */
	def factors(v:Variable) : Seq[Factor] = this.flatMap(template => template.factors(v)).toList
	def factors(vs:Iterable[Variable]) : Seq[Factor] = this.flatMap(template => template.factors(vs))
	// TODO Should the method below be renamed "factorsOf"?
	def factorsOfClass[T2<:T](v:Variable)(implicit m:Manifest[T2]) : Seq[Factor] = this.templatesOf[T2](m).flatMap(template => template.factors(v))
	def registerFactorsInVariables(variables: Iterable[Variable with FactorList]): Seq[Factor] = {
	  val factors = this.factors(variables)
    // Make sure each variables factor list starts empty
    variables.foreach(_.clearFactors)
    // Add relevant factors to each relevant neighboring variable
    factors.foreach(_.addToVariables)
    factors.toSeq
	}
	def score(d:DiffList) : Double = factors(d).foldLeft(0.0)(_+_.statistic.score)
 	def score1(v:Variable) : Double = factors(v).foldLeft(0.0)(_+_.statistic.score) // For use when the Variable is also Iterable
	def score(v:Variable) : Double = factors(v).foldLeft(0.0)(_+_.statistic.score)
	def score(vars:Iterable[Variable]) : Double = factors(vars).foldLeft(0.0)(_+_.statistic.score)
	def scoreAll(vars:Iterable[Variable]) : Double = factors(vars).foldLeft(0.0)(_+_.statistic.score)
 	def aveScore(vars:Collection[Variable]): Double = score(vars) / vars.size
}

