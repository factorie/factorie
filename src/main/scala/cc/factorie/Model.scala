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

/** In FACTORIE a Model is a source of factors.
    In particular, it can return the list of factors that touch a collection of variables.
    (Typically variables do not know directly about the factors that touch them.
    This allows us to consider multiple different Models applied to the same set of data.)
    @author Andrew McCallum
    @since 0.11
 */
trait Model {
  def factors(variables:Iterable[Variable]): Seq[Factor]
  def factors(d:DiffList) : Seq[Factor] = if (d.size == 0) Nil else normalize(factors(d.map(_.variable)))

  def filterByFactorClass[F<:Factor](factors:Seq[Factor], fclass:Class[F]): Seq[F] = factors.filter(f => fclass.isAssignableFrom(f.getClass)).asInstanceOf[Seq[F]]
  def factorsOfClass[F<:Factor](variables:Iterable[Variable], fclass:Class[F]): Seq[F] = filterByFactorClass(factors(variables), fclass)
  def factorsOfClass[F<:Factor](variables:Iterable[Variable])(implicit fm:Manifest[F]): Seq[F] = factorsOfClass(variables, fm.erasure.asInstanceOf[Class[F]])
  def factorsOfClass[F<:Factor](d:DiffList, fclass:Class[F]): Seq[F] = filterByFactorClass(factors(d), fclass)
  def factorsOfClass[F<:Factor](d:DiffList)(implicit fm:Manifest[F]): Seq[F] = factorsOfClass[F](d, fm.erasure.asInstanceOf[Class[F]])

  def filterByFamilyClass[F<:Family](factors:Seq[Factor], fclass:Class[F]): Seq[F#Factor] =
    factors.filter(f => f match {
      case f:Family#Factor => fclass.isAssignableFrom(f.family.getClass)
      case _ => false
    }).asInstanceOf[Seq[F#Factor]]
  def factorsOfFamilyClass[F<:Family](variables:Iterable[Variable], fclass:Class[F]): Seq[F#Factor] = filterByFamilyClass[F](factors(variables), fclass)
  def factorsOfFamilyClass[F<:Family](variables:Iterable[Variable])(implicit fm:Manifest[F]): Seq[F#Factor] = factorsOfFamilyClass[F](variables, fm.erasure.asInstanceOf[Class[F]])
  def factorsOfFamilyClass[F<:Family](d:DiffList, fclass:Class[F]): Seq[F#Factor] = filterByFamilyClass(factors(d), fclass)
  def factorsOfFamilyClass[F<:Family](d:DiffList)(implicit fm:Manifest[F]): Seq[F#Factor] = filterByFamilyClass[F](factors(d), fm.erasure.asInstanceOf[Class[F]])

  def filterByFamily[F<:Family](factors:Seq[Factor], family:F): Seq[F#Factor] = 
    factors.filter(f => f match {
      case f:Family#Factor => f.family.equals(family)
      case _ => false
    }).asInstanceOf[Seq[F#Factor]]
  def filterByFamilies[F<:Family](factors:Seq[Factor], families:Seq[F]): Seq[F#Factor] = 
    factors.filter(f => f match {
      case f:Family#Factor => families.contains(f.family)
      case _ => false
    }).asInstanceOf[Seq[F#Factor]]
  def factorsOfFamily[F<:Family](variables:Iterable[Variable], family:F): Seq[F#Factor] = filterByFamily(factors(variables), family)
  def factorsOfFamily[F<:Family](d:DiffList, family:F): Seq[F#Factor] = filterByFamily(factors(d), family)
  def factorsOfFamilies[F<:Family](variables:Iterable[Variable], families:Seq[F]): Seq[F#Factor] = filterByFamilies(factors(variables), families)
  def factorsOfFamilies[F<:Family](d:DiffList, families:Seq[F]): Seq[F#Factor] = filterByFamilies(factors(d), families)
  
  def score(variables:Iterable[Variable]): Double = factors(variables).foldLeft(0.0)((sum, f) => sum + f.score)
  def score(d:DiffList) : Double = {
    var result = 0.0
    for (f <- factors(d)) result += f.statistics.score
    result
  }
  /** Returns the average score, that is score of variables, normalized by the size of the collections vars. */
  def aveScore(variables:Iterable[Variable]): Double = score(variables) / variables.size  // TODO Rename to scoreAve?

  /** Deduplicate a sequence of Factors while also being sure not to include inner factors 
      (whose interpretation may require special handling by the outer factor).
      This method should be called on all Seq[Factor] before they are returned by methods such as "factors" */
  def normalize(factors:Seq[Factor]): Seq[Factor] = {
    //if (factors.forall(_.outer eq null)) factors
    //else {
      val result = new scala.collection.mutable.HashSet[Factor] {
        override def addEntry(f:Factor): Boolean = {
          if (f.outer eq null) super.addEntry(f)
          else {
            var f2 = f.outer
            while ((f2.outer ne null) && (f2.outer ne Model.this)) f2 = f2.outer
            super.addEntry(f2)
          }
        }
      }
      result ++= factors
      result.toSeq
    //}
  }
  
  // Some Model subclasses have a list of Families to which all its factors belong
  def families: Seq[Family] = Nil
  def familiesOfClass[F<:Family](fclass:Class[F]): Seq[F] = Nil
  def familiesOfClass[F<:Family]()(implicit m:Manifest[F]): Seq[F] = familiesOfClass[F](m.erasure.asInstanceOf[Class[F]])

  // Some Model subclasses that have a fixed set of factors and variables can override the methods below
  // TODO Consider making a Model trait for these methods.  Yes!
  def variables: Seq[Variable] = Nil
  def factors: Seq[Factor] = Nil
  def score: Double = factors.foldLeft(0.0)((sum, f) => sum + f.score)
}


/** A Model that concatenates the factors of multiple contained models.
    @author Andrew McCallum
    @since 0.11
 */
class CombinedModel(val subModels:Model*) extends Model {
  def factors(variables:Iterable[Variable]): Seq[Factor] = normalize(subModels.flatMap(_.factors(variables)))
  override def variables = subModels.flatMap(_.variables) // TODO Does this need normalization, de-duplication?
  override def factors = subModels.flatMap(_.factors) // TODO Does this need normalization, de-duplication?
}

/** A Model that explicitly stores all factors, with an efficient map from variables to their neighboring factors.
    @author Andrew McCallum
    @since 0.11
 */
class FactorModel(initialFactors:Factor*) extends Model {
  // TODO Also add Map[Variable,Factor] for storing Factors that do not belong to an unrolling Template
  private val _factors = new HashMap[Variable,HashSet[Factor]] {
    override def default(v:Variable) = {
      val result = new HashSet[Factor]
      this(v) = result
      result
    }
  }
  this ++= initialFactors
  // TODO The next method is incredibly inefficient because it will result in the creation of a HashSet for every variable queried,
  // even if that variable does not have any factors in this model.
  // TODO The next method needs to handle ContainerVariables.
  def factors(variables:Iterable[Variable]): Seq[Factor] = normalize(variables.flatMap(v => _factors(v)).toSeq)
  override def factors: Seq[Factor] = _factors.values.flatMap(set => set).toSeq.distinct // TODO Make more efficient?
  def +=(f:Factor): Unit = f.variables.foreach(v => _factors(v) += f)
  def -=(f:Factor): Unit = f.variables.foreach(v => _factors(v) -= f)
  def ++=(fs:Iterable[Factor]): Unit = fs.foreach(f => this.+=(f))
  def --=(fs:Iterable[Factor]): Unit = fs.foreach(f => this.-=(f))
}

/** A Model that creates factors on the fly from Templates.
    @author Andrew McCallum
    @since 0.8
    @see Template
 */
class TemplateModel(initialTemplates:Template*) extends Model {

  private val _templates = new ArrayBuffer[Template] ++= initialTemplates
  def templates: Seq[Template] = _templates
  def ++=(moreTemplates:Iterable[Template]) = _templates ++= moreTemplates
  def +=(template:Template) = _templates += template
  @deprecated def clear = _templates.clear // TODO Consider removing this.

  override def families = _templates
  override def familiesOfClass[F<:Family](fclass:Class[F]): Seq[F] = _templates.filter(t => fclass.isAssignableFrom(t.getClass)).asInstanceOf[Seq[F]]

  def limitDiscreteValuesIteratorAsIn(variables:Iterable[DiscreteVar]): Unit = _templates.foreach(_.limitDiscreteValuesIteratorAsIn(variables))
  
  /*@deprecated
  def templatesOf[T2<:Template](implicit m:Manifest[T2]) : IndexedSeq[T2] = {
    val templateClass = m.erasure
    val ret = new ArrayBuffer[T2]
    for (t <- templates) if (templateClass.isAssignableFrom(t.getClass)) ret += t.asInstanceOf[T2]
    ret
  }
  @deprecated
  def templatesOfClass[T2<:Template](cls:Class[T2]): IndexedSeq[T2] = {
    val ret = new ArrayBuffer[T2]
    for (t <- templates) if (cls.isAssignableFrom(t.getClass)) ret += t.asInstanceOf[T2]
    ret
  }*/

  override def normalize(factors: Seq[Factor]) = {
    // do nothing since templates are responsible for de-duplication
    factors
  }

  def factors(vs:Iterable[Variable]) : Seq[Factor] = normalize(templates.flatMap(template => template.factors(vs)))
  override def factors(d:DiffList) : Seq[Factor] = if (d.size == 0) Nil else normalize(templates.flatMap(template => template.factors(d)))
  
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
