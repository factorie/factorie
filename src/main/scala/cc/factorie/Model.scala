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

import scala.collection.mutable.{ArrayBuffer,HashMap,HashSet}

// TODO Make models composable
// Model { score:Double = 0.0; variables:Seq[Variable] = Nil; score(vs:Seq[Variable]):Double; factors:Seq[Factor]; factors(vs:Seq[Variable]):Seq[Factor]; }
// TemplateModel
// FactorModel { +=(f:Factor); }
// GenerativeModel
// CombinedModel(models:Model*)
// MyModel(MyTemplateModel, GenerativeModel)
// trait Factor extends Model

// Factor extends Model

// POSTagger extends TemplateModel with Inferencer[POSLabel]
// ConcreteLinearChainPOS extends OuterFactor with BPInferencer[POSLabel]

trait Model {
  def factors(variables:Iterable[Variable]): Seq[Factor]
  def factors(d:DiffList, outer:Factor = null) : Seq[Factor] = if (d.size == 0) Nil else normalize(factors(d.map(_.variable)), outer)
  def score(variables:Iterable[Variable]): Double = factors(variables).foldLeft(0.0)((sum, f) => sum + f.score)
  def score(d:DiffList) : Double = factors(d).foldLeft(0.0)(_+_.statistics.score)
  // Before returning a sequence of Factors make sure they are de-duplicated and don't include inner factors.
  def normalize(factors:Seq[Factor]): Seq[Factor] = normalize(factors, null)
  def normalize(factors:Seq[Factor], outer:Factor): Seq[Factor] = {
    if (factors.forall(_.outer eq null)) factors
    else {
      val result = new scala.collection.mutable.HashSet[Factor] {
        override def addEntry(f:Factor): Boolean = {
          if (f.outer eq null) super.addEntry(f)
          else {
            var f2 = f.outer
            while ((f2.outer ne null) && (f2.outer ne outer)) f2 = f2.outer
            super.addEntry(f2)
          }
        }
      }
      result ++= factors
      result.toSeq
    }
  }
  // Special Model subclasses that have a fixed set of factors and variables can override the methods below
  def variables: Seq[Variable] = Nil
  def factors: Seq[Factor] = Nil
  def score: Double = factors.foldLeft(0.0)((sum, f) => sum + f.score)
}

object GenerativeModel extends Model {
  /** Only works on Iterable[GeneratedVar] */
  def factors(variables:Iterable[Variable]): Seq[Factor] = {
    val result = new ArrayBuffer[Factor]
    variables.foreach(v => v match {
      case p:cc.factorie.generative2.Parameter => { result += p.parentFactor; result ++= p.childFactors }
      case gv:cc.factorie.generative2.GeneratedVar => result += gv.parentFactor
    })
    normalize(result)
  }
}

class CombinedModel(val subModels:Model*) extends Model {
  def factors(variables:Iterable[Variable]): Seq[Factor] = normalize(subModels.flatMap(_.factors(variables)))
  override def variables = subModels.flatMap(_.variables)
  override def factors = subModels.flatMap(_.factors)
}


/** A Model in FACTORIE consists of a collection of factor Templates and methods that operate on the collection.
    @author Andrew McCallum
    @since 0.8
    @see Template
 */
class TemplateModel(initialTemplates:Template*) extends Model {
  type T = Template

  private val _templates = new ArrayBuffer[T] ++= initialTemplates
  def templates: Seq[T] = _templates
  //def apply(i:Int) = ts.apply(i)
  //def length = ts.length
  //override def iterator = ts.iterator
  def ++=(moreTemplates:Iterable[Template]) = _templates ++= moreTemplates
  def +=(template:Template) = _templates += template
  @deprecated def clear = _templates.clear // TODO Consider removing this.

  // TODO Also add Map[Variable,Factor] for storing Factors that do not belong to an unrolling Template
  private val _factors = new HashMap[Variable,HashSet[Factor]] {
    override def default(v:Variable) = new HashSet[Factor]
  }
  override def factors: Seq[Factor] = _factors.values.flatMap(set => set).toSeq.distinct // TODO Make more efficient?
  def +=(f:Factor): Unit = f.variables.foreach(v => _factors(v) += f)
  def -=(f:Factor): Unit = f.variables.foreach(v => _factors(v) -= f)
  def ++=(fs:Iterable[Factor]): Unit = fs.foreach(f => this.+=(f))
  def --=(fs:Iterable[Factor]): Unit = fs.foreach(f => this.-=(f))
  
  def templatesOf[T2<:T](implicit m:Manifest[T2]) : IndexedSeq[T2] = {
    val templateClass = m.erasure
    val ret = new ArrayBuffer[T2]
    for (t <- templates) if (templateClass.isAssignableFrom(t.getClass)) ret += t.asInstanceOf[T2]
    ret
  }
  def templatesOfClass[T2<:T](cls:Class[T2]): IndexedSeq[T2] = {
    val ret = new ArrayBuffer[T2]
    for (t <- templates) if (cls.isAssignableFrom(t.getClass)) ret += t.asInstanceOf[T2]
    ret
  }
  /*override def filter(test:(T)=>Boolean): Model = {
    val ret = new TemplateModel
    for (t <- templates) if (test(t)) ret += t
    ret
  }*/
  
  override def normalize(factors:Seq[Factor], outer:Factor = null): Seq[Factor] = {
    if (factors.forall(_.outer eq null)) factors
    else {
      val result = new scala.collection.mutable.HashSet[Factor] {
        override def addEntry(f:Factor): Boolean = {
          if (f.outer eq null) super.addEntry(f)
          else {
            var f2 = f.outer
            while ((f2.outer ne null) && (f2.outer ne outer)) f2 = f2.outer
            super.addEntry(f2)
          }
        }
      }
      result ++= factors
      result.toSeq
    }
  }
  override def factors(d:DiffList, outer:Factor = null) : Seq[Factor] = if (d.size == 0) Nil else normalize(templates.flatMap(template => template.factors(d)), outer)
  def factorsOf[T2<:T](d:DiffList)(implicit m:Manifest[T2]) : Seq[T2#Factor] = if (d.size == 0) Nil else this.templatesOf[T2](m).flatMap(template => template.factors(d))
  def factorsOf[T2<:T](cls:Class[T2])(d:DiffList): Seq[T2#Factor] = if (d.size == 0) Nil else this.templatesOfClass[T2](cls).flatMap(template => template.factors(d))
  // TODO Rename factorsOfAll
  def factorsOf[T2<:T](vs:Iterable[Variable])(implicit m:Manifest[T2]) : Seq[T2#Factor] = this.templatesOf[T2](m).flatMap(template => template.factors(vs))
  def factorsOf[T2<:T](v:Variable)(implicit m:Manifest[T2]) : Seq[T2#Factor] = this.templatesOf[T2](m).flatMap(template => template.factors(v))
  /** Given a variable, return a collection of Factors that touch it.  Note that combining these results for multiple variables may result in duplicate Factors. */
  //def factors(v:Variable) : Seq[Factor] = normalize(templates.flatMap(template => template.factors(v)), null)
  def factors(v:Variable, outer:Factor) : Seq[Factor] = normalize(templates.flatMap(template => template.factors(v)), outer)
  def factors(vs:Iterable[Variable]) : Seq[Factor] = normalize(templates.flatMap(template => template.factors(vs)), null)
  @deprecated def factorsAll(vs:Iterable[Variable], outer:Factor) : Seq[Factor] = normalize(templates.flatMap(template => template.factors(vs)), outer)
  //def score1(v:Variable) : Double = factors(v).foldLeft(0.0)(_+_.statistics.score) // For use when the Variable is also Iterable
  //def score(v:Variable) : Double = factors(v).foldLeft(0.0)(_+_.statistics.score)
  //def score(vars:Iterable[Variable]) : Double = factorsAll(vars).foldLeft(0.0)(_+_.statistics.score)
  /** Score all variables in the Iterable collection.  This method is useful when a Variable is also a Iterable[Variable]; 
      it forces the Iterable interpretation and avoids the single variable interpretation of score(Variable). */
  //def score(vars:Iterable[Variable]) : Double = factors(vars).foldLeft(0.0)(_+_.statistics.score)
  /** Returns the average score, that is scoreAll of vars, normalized by the size of the collections vars. */
  // TODO Rename to scoreAve
  def aveScore(vars:Iterable[Variable]): Double = score(vars) / vars.size

  
  def save(dirname:String): Unit = {
    import java.io.File
    //println("Saving model "+getClass.getName+" to "+dirname)
    val f = new File(dirname)
    // Recursively delete all files in directory "f"
    def delete(f:File): Boolean = { if (f.isDirectory) f.listFiles.forall(f2 => delete(f2)) else f.delete }
    if (f.exists) if (!delete(f)) throw new Error("Error deleting directory "+dirname)
    f.mkdir
    templates.foreach(_.save(dirname))
  }
 
  def load(dirname:String): Unit = {
    templates.foreach(_.load(dirname))
  }
}
