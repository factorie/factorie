/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie.model

import cc.factorie.util.SingletonIndexedSeq
import cc.factorie.variable._

import scala.collection.immutable.ListSet
import scala.collection.mutable.{ArrayBuffer, HashMap, LinkedHashSet, Set}
import scala.reflect.ClassTag

/** In FACTORIE a Model is a source of factors.
    In particular, it can return the collection of factors that touch a collection of variables.
    Variables do not know directly about the factors that touch them.
    This allows us to consider multiple different Models applied to the same data.
    @author Andrew McCallum
 */
trait Model {
  // TODO Consider adding "type FactorType <: Factor" here so that Template2.factors can return the right type. -akm
  
  /** Return all Factors in this Model that touch any of the given "variables".  The result will not have any duplicate Factors. */
  def factors(variables:Iterable[Var]): Iterable[Factor]
  /** Return all Factors in this Model that touch the given "variable".  The result will not have any duplicate Factors. */
  def factors(variable:Var): Iterable[Factor] = factors(new SingletonIndexedSeq(variable))
  /** Return all Factors in this Model that are affected by the given Diff.  The result will not have any duplicate Factors.
      By default returns just the factors that neighbor Diff.variable, but this method may be overridden for special handling of the Diff */
  def factors(d:Diff): Iterable[Factor] = if (d.variable eq null) Nil else factors(d.variable)
  /** Return all Factors in this Model that are affected by the given DiffList.  The result will not have any duplicate Factors.
      By default returns just the factors that neighbor the DiffList.variables, but this method may be overridden for special handling of the DiffList */
  def factors(dl:DiffList): Iterable[Factor] = if (dl.size == 0) Nil else factors(dl.foldLeft(List[Var]())((vs,d) => if (d.variable ne null) d.variable :: vs else vs))

  // TODO Make these addFactors protected?  Perhaps not because they could be reasonably useful to outside users. -akm
  /** Append to "result" all Factors in this Model that touch any of the given "variables".  This method must not append duplicates. */
  def addFactors(variables:Iterable[Var], result:Set[Factor]): Unit = result ++= factors(variables)
  /** Append to "result" all Factors in this Model that touch the given "variable".  This method must not append duplicates. */
  def addFactors(variable:Var, result:Set[Factor]): Unit = addFactors(new SingletonIndexedSeq(variable), result)
  /** Append to "result" all Factors in this Model that are affected by the given Diff.  This method must not append duplicates. */
  def addFactors(d:Diff, result:Set[Factor]): Unit = if (d.variable ne null) addFactors(d.variable, result)
  /** Append to "result" all Factors in this Model that are affected by the given DiffList.  This method must not append duplicates. */
  def addFactors(dl:DiffList, result:Set[Factor]): Unit = if (dl.size > 0) addFactors(dl.foldLeft(List[Var]())((vs,d) => if (d.variable ne null) d.variable :: vs else vs), result)
  /** The "factors" methods need a new collection to return; this method is used by them to construct this collection. */
  def newFactorsCollection: Set[Factor] = new collection.mutable.LinkedHashSet[Factor]

  def filterByFactorClass[F<:Factor](factors:Iterable[Factor], fclass:Class[F]): Iterable[F] = factors.filter(f => fclass.isAssignableFrom(f.getClass)).asInstanceOf[Iterable[F]]
  def factorsOfClass[F<:Factor](variable:Var, fclass:Class[F]): Iterable[F] = filterByFactorClass(factors(variable), fclass)
  def factorsOfClass[F<:Factor](variables:Iterable[Var], fclass:Class[F]): Iterable[F] = filterByFactorClass(factors(variables), fclass)
  def factorsOfClass[F<:Factor](variable:Var)(implicit fm:ClassTag[F]): Iterable[F] = factorsOfClass(variable, fm.runtimeClass.asInstanceOf[Class[F]])
  def factorsOfClass[F<:Factor](variables:Iterable[Var])(implicit fm:ClassTag[F]): Iterable[F] = factorsOfClass(variables, fm.runtimeClass.asInstanceOf[Class[F]])
  def factorsOfClass[F<:Factor](d:DiffList, fclass:Class[F]): Iterable[F] = filterByFactorClass(factors(d), fclass)
  def factorsOfClass[F<:Factor](d:DiffList)(implicit fm:ClassTag[F]): Iterable[F] = factorsOfClass[F](d, fm.runtimeClass.asInstanceOf[Class[F]])

  // TODO: why can't this just take the ClassTag for F as an implicit and avoid this explicit passing of the "Class" object? -luke
  // TODO maybe these methods should be moved to a model companion object since they do not require anything from the model -luke, akm
  def filterByFamilyClass[F<:Family](factors:Iterable[Factor], fclass:Class[F]): Iterable[F#Factor] =
    factors.filter(f => f match {
      case f:Family#Factor => fclass.isAssignableFrom(f.family.getClass)
      case _ => false
    }).asInstanceOf[Iterable[F#Factor]]
  def filterByNotFamilyClass[F<:Family](factors:Iterable[Factor], fclass:Class[F]): Iterable[Factor] =
    factors.filterNot({
      case f:Family#Factor => fclass.isAssignableFrom(f.family.getClass)
      case _ => false
    })
  def factorsOfFamilyClass[F<:Family](variable:Var, fclass:Class[F]): Iterable[F#Factor] = filterByFamilyClass[F](factors(variable), fclass)
  def factorsOfFamilyClass[F<:Family](variables:Iterable[Var], fclass:Class[F]): Iterable[F#Factor] = filterByFamilyClass[F](factors(variables), fclass)
  def factorsOfFamilyClass[F<:Family](variable:Var)(implicit fm:ClassTag[F]): Iterable[F#Factor] = factorsOfFamilyClass[F](variable, fm.runtimeClass.asInstanceOf[Class[F]])
  def factorsOfFamilyClass[F<:Family](variables:Iterable[Var])(implicit fm:ClassTag[F]): Iterable[F#Factor] = factorsOfFamilyClass[F](variables, fm.runtimeClass.asInstanceOf[Class[F]])
  def factorsOfFamilyClass[F<:Family](d:DiffList, fclass:Class[F]): Iterable[F#Factor] = filterByFamilyClass(factors(d), fclass)
  def factorsOfFamilyClass[F<:Family](d:DiffList)(implicit fm:ClassTag[F]): Iterable[F#Factor] = filterByFamilyClass[F](factors(d), fm.runtimeClass.asInstanceOf[Class[F]])

  def filterByFamily[F<:Family](factors:Iterable[Factor], family:F): Iterable[F#Factor] = 
    factors.filter(f => f match {
      case f:Family#Factor => f.family.equals(family)
      case _ => false
    }).asInstanceOf[Iterable[F#Factor]]
  def filterByFamilies[F<:Family](factors:Iterable[Factor], families:Seq[F]): Iterable[F#Factor] = 
    factors.filter(f => f match {
      case f:Family#Factor => families.contains(f.family)
      case _ => false
    }).asInstanceOf[Iterable[F#Factor]]
  def factorsOfFamily[F<:Family](variable:Var, family:F): Iterable[F#Factor] = filterByFamily(factors(variable), family)
  def factorsOfFamily[F<:Family](variables:Iterable[Var], family:F): Iterable[F#Factor] = filterByFamily(factors(variables), family)
  def factorsOfFamily[F<:Family](d:DiffList, family:F): Iterable[F#Factor] = filterByFamily(factors(d), family)
  def factorsOfFamilies[F<:Family](variable:Var, families:Seq[F]): Iterable[F#Factor] = filterByFamilies(factors(variable), families)
  def factorsOfFamilies[F<:Family](variables:Iterable[Var], families:Seq[F]): Iterable[F#Factor] = filterByFamilies(factors(variables), families)
  def factorsOfFamilies[F<:Family](d:DiffList, families:Seq[F]): Iterable[F#Factor] = filterByFamilies(factors(d), families)
  
  // Getting sums of scores from all neighboring factors
  def currentScore(variable:Var): Double = { var sum = 0.0; for (f <- factors(variable)) sum += f.currentScore; sum }
  def currentScore(vars:Iterable[Var]): Double = { var sum = 0.0; for (f <- factors(vars)) sum += f.currentScore; sum }
  def currentScore(d:Diff): Double = { var sum = 0.0; for (f <- factors(d)) sum += f.currentScore; sum }
  def currentScore(dl:DiffList): Double = { var sum = 0.0; for (f <- factors(dl)) sum += f.currentScore; sum }
  // ...using not current values, but the values in an Assignment
  def assignmentScore(variable:Var, assignment:Assignment): Double = { var sum = 0.0; for (f <- factors(variable)) sum += f.assignmentScore(assignment); sum }
  def assignmentScore(vars:Iterable[Var], assignment:Assignment): Double = { var sum = 0.0; for (f <- factors(vars)) sum += f.assignmentScore(assignment); sum }
  def assignmentScore(d:Diff, assignment:Assignment): Double = { var sum = 0.0; for (f <- factors(d)) sum += f.assignmentScore(assignment); sum }
  def assignmentScore(dl:DiffList, assignment:Assignment): Double = { var sum = 0.0; for (f <- factors(dl)) sum += f.assignmentScore(assignment); sum }

  // Return a fully unrolled model for a given context
  def itemizedModel(variable:Var): ItemizedModel = new ItemizedModel(factors(variable))
  def itemizedModel(variables:Iterable[Var]): ItemizedModel = new ItemizedModel(factors(variables))
  def itemizedModel(d:Diff): ItemizedModel = new ItemizedModel(factors(d))
  def itemizedModel(dl:DiffList): ItemizedModel = new ItemizedModel(factors(dl))
}

/** A Model that explicitly stores all factors, with an efficient map from variables to their neighboring factors.
    A DirectedModel is a subclass of this.
    @author Andrew McCallum
 */
class ItemizedModel(initialFactors:Factor*) extends Model {
  def this(initialFactors:Iterable[Factor]) = { this(initialFactors.toSeq:_*) }
  private val _factors = new HashMap[Var,scala.collection.Set[Factor]] {
    override def default(v:Var) = ListSet.empty[Factor]
  }
  this ++= initialFactors
  override def addFactors(variable:Var, result:Set[Factor]): Unit = result ++= _factors(variable) // This is new primitive
  override def addFactors(variables:Iterable[Var], result:Set[Factor]): Unit = variables.foreach(addFactors(_, result))
  override def factors(variable:Var): Iterable[Factor] = _factors(variable)
  def factors(variables:Iterable[Var]): Iterable[Factor] = { val set = newFactorsCollection; variables.foreach(v => addFactors(v, set)); set }
  def factors: Iterable[Factor] = _factors.values.flatten.toSeq.distinct
  def +=(f:Factor): Unit = f.variables.foreach(v => _factors(v) match {
    case h:ListSet[Factor] => 
      if (h.size > 3) _factors(v) = { val nh = new LinkedHashSet[Factor] ++= h; nh += f; nh }
      else _factors(v) = h + f
    case h:LinkedHashSet[Factor] => h += f
  })
  def -=(f:Factor): Unit = f.variables.foreach(v => _factors(v) match {
    case h:ListSet[Factor] => _factors(v) = h - f
    case h:LinkedHashSet[Factor] => h -= f
  }) 
  def ++=(fs:Iterable[Factor]): Unit = fs.foreach(f => this.+=(f))
  def --=(fs:Iterable[Factor]): Unit = fs.foreach(f => this.-=(f))
}

/** A Model that concatenates the factors of multiple contained models.
    @author Andrew McCallum
 */
class CombinedModel(theSubModels:Model*) extends Model {
  val subModels = new ArrayBuffer[Model] ++= theSubModels
  def +=(model:Model): Unit = subModels += model
  def ++=(models:Iterable[Model]): Unit = subModels ++= models
  def factors(variables:Iterable[Var]): Iterable[Factor] = {
    val result = newFactorsCollection
    subModels.foreach(_.addFactors(variables, result))
    result
  }
  //override def factors(variable:Var): Iterable[Factor] = { val result = newFactorsCollection; addFactors(variable, result); result }
  override def addFactors(variables:Iterable[Var], result:Set[Factor]): Unit = subModels.foreach(_.addFactors(variables, result))
}

/** A Model whose Factors come from Templates.
    If you want your TemplateModel to have learnable parameters, then also extend Parameters.
    @author Andrew McCallum */
class TemplateModel(theTemplates:Template*) extends Model {
  val templates = new ArrayBuffer[Template] ++= theTemplates
  def +=[T<:Template](template:T): T = { templates += template; template }
  def ++=[T<:Template](templates:Iterable[T]): Iterable[T] = { this.templates ++= templates; templates }
  // Just a method name aliase, aiming to make use inside the TemplateModel subclass prettier.
  def addTemplate[T<:Template](template:T): T = { templates += template; template }
  // Just a method name alias, aiming to make use inside the TemplateModel subclass prettier.
  def addTemplates[T<:Template](templates:T*): Iterable[T] = { this.templates ++= templates; templates }
  override def addFactors(variable:Var, result:Set[Factor]): Unit = templates.foreach(_.addFactors(variable, result)) // This is the new primitive
  def factors(variables:Iterable[Var]): Iterable[Factor] = { val result = newFactorsCollection; addFactors(variables, result); result }
  override def factors(variable:Var): Iterable[Factor] = { val result = newFactorsCollection; addFactors(variable, result); result }
  override def addFactors(variables:Iterable[Var], result:Set[Factor]): Unit = variables.foreach(v => addFactors(v, result))
  def families: Seq[Template] = templates
  def familiesOfClass[F<:Template](fclass:Class[F]): Iterable[F] = families.filter(f => fclass.isAssignableFrom(f.getClass)).asInstanceOf[Iterable[F]]
  def limitDiscreteValuesAsIn(vars:Iterable[DiscreteVar]): Unit = templates.foreach(_.limitDiscreteValuesAsIn(vars)) 
}


