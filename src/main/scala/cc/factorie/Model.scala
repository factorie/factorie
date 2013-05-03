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
import cc.factorie.la._
import scala.collection.mutable.{ArrayBuffer,HashMap,LinkedHashSet,ListBuffer}
import scala.collection.immutable.ListSet
import scala.collection.mutable.Set
import scala.collection.generic.Growable

// TODO In the future, consider things like:
// POSTagger extends TemplateModel with Inferencer[POSLabel]
// ConcreteLinearChainPOS extends POSTemplateModel with BPInferencer[POSLabel]

/** In FACTORIE a Model is a source of factors.
    In particular, it can return the collection of factors that touch a collection of variables.
    (Variables do not know directly about the factors that touch them.
    This allows us to consider multiple different Models applied to the same set of data.)
    @author Andrew McCallum
    @since 0.11
 */

trait Model {
  /** Return all Factors in this Model that touch the given "variable".  The result will not have any duplicate Factors. */
  def factors(variable:Var): Iterable[Factor] // = { val set = newFactorsCollection; addFactors(variable, set); set }
  /** Return all Factors in this Model that touch any of the given "variables".  The result will not have any duplicate Factors. */
  def factors(variables:Iterable[Var]): Iterable[Factor] = { val set = newFactorsCollection; addFactors(variables, set); set }
  /** Return all Factors in this Model that are affected by the given Diff.  The result will not have any duplicate Factors. */
  def factors(d:Diff): Iterable[Factor] = if (d.variable eq null) Nil else { val set = newFactorsCollection; addFactors(d, set); set }
  /** Return all Factors in this Model that are affected by the given DiffList.  The result will not have any duplicate Factors. */
  def factors(dl:DiffList): Iterable[Factor] = if (dl.size == 0) Nil else { val set = newFactorsCollection; addFactors(dl, set); set } //factors(dl.foldLeft(List[Variable]())((vs,d) => if (d.variable ne null) d.variable :: vs else vs))

  /** Append to "result" all Factors in this Model that touch the given "variable".  This method must not append duplicates. */
  def addFactors(variable:Var, result:Set[Factor]): Unit = result ++= factors(variable)
  /** Append to "result" all Factors in this Model that touch any of the given "variables".  This method must not append duplicates. */
  def addFactors(variables:Iterable[Var], result:Set[Factor]): Unit  = for (v <- variables) addFactors(v, result)
  /** Append to "result" all Factors in this Model that are affected by the given Diff.  This method must not append duplicates. */
  def addFactors(d:Diff, result:Set[Factor]): Unit = if (d.variable ne null) addFactors(d.variable, result)
  /** Append to "result" all Factors in this Model that are affected by the given DiffList.  This method must not append duplicates. */
  def addFactors(dl:DiffList, result:Set[Factor]): Unit = if (dl.size > 0) addFactors(dl.foldLeft(List[Var]())((vs,d) => if (d.variable ne null) d.variable :: vs else vs), result)
  /** The "factors" methods need a new collection to return; this method is used by them to construct this collection. */
  protected def newFactorsCollection: Set[Factor] = new collection.mutable.LinkedHashSet[Factor]

  def filterByFactorClass[F<:Factor](factors:Iterable[Factor], fclass:Class[F]): Iterable[F] = factors.filter(f => fclass.isAssignableFrom(f.getClass)).asInstanceOf[Iterable[F]]
  def factorsOfClass[F<:Factor](variable:Var, fclass:Class[F]): Iterable[F] = filterByFactorClass(factors(variable), fclass)
  def factorsOfClass[F<:Factor](variables:Iterable[Var], fclass:Class[F]): Iterable[F] = filterByFactorClass(factors(variables), fclass)
  def factorsOfClass[F<:Factor](variable:Var)(implicit fm:Manifest[F]): Iterable[F] = factorsOfClass(variable, fm.erasure.asInstanceOf[Class[F]])
  def factorsOfClass[F<:Factor](variables:Iterable[Var])(implicit fm:Manifest[F]): Iterable[F] = factorsOfClass(variables, fm.erasure.asInstanceOf[Class[F]])
  def factorsOfClass[F<:Factor](d:DiffList, fclass:Class[F]): Iterable[F] = filterByFactorClass(factors(d), fclass)
  def factorsOfClass[F<:Factor](d:DiffList)(implicit fm:Manifest[F]): Iterable[F] = factorsOfClass[F](d, fm.erasure.asInstanceOf[Class[F]])

  def filterByFamilyClass[F<:Family](factors:Iterable[Factor], fclass:Class[F]): Iterable[F#Factor] =
    factors.filter(f => f match {
      case f:Family#Factor => fclass.isAssignableFrom(f.family.getClass)
      case _ => false
    }).asInstanceOf[Iterable[F#Factor]]
  def filterNotByFamilyClass[F<:Family](factors:Iterable[Factor], fclass:Class[F]): Iterable[Factor] =
    factors.filterNot({
      case f:Family#Factor => fclass.isAssignableFrom(f.family.getClass)
      case _ => false
    })
  def factorsOfFamilyClass[F<:Family](variable:Var, fclass:Class[F]): Iterable[F#Factor] = filterByFamilyClass[F](factors(variable), fclass)
  def factorsOfFamilyClass[F<:Family](variables:Iterable[Var], fclass:Class[F]): Iterable[F#Factor] = filterByFamilyClass[F](factors(variables), fclass)
  def factorsOfFamilyClass[F<:Family](variable:Var)(implicit fm:Manifest[F]): Iterable[F#Factor] = factorsOfFamilyClass[F](variable, fm.erasure.asInstanceOf[Class[F]])
  def factorsOfFamilyClass[F<:Family](variables:Iterable[Var])(implicit fm:Manifest[F]): Iterable[F#Factor] = factorsOfFamilyClass[F](variables, fm.erasure.asInstanceOf[Class[F]])
  def factorsOfFamilyClass[F<:Family](d:DiffList, fclass:Class[F]): Iterable[F#Factor] = filterByFamilyClass(factors(d), fclass)
  def factorsOfFamilyClass[F<:Family](d:DiffList)(implicit fm:Manifest[F]): Iterable[F#Factor] = filterByFamilyClass[F](factors(d), fm.erasure.asInstanceOf[Class[F]])

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

trait ModelWithContext[-C] extends Model {
  // Factors from context (different than Variable, Iterable[Variable] or DiffList
  def factorsWithContext(context:C): Iterable[Factor] // = throw new Error("Model subclass does not implement factors(C): "+getClass)
  def factorsWithContext(contexts:Iterable[C]): Iterable[Factor] = { val result = newFactorsCollection; contexts.foreach(c => addFactorsWithContext(c, result)); result }
  def variablesWithContext(context:C): Iterable[Var] = factorsWithContext(context).flatMap(_.variables).toSeq.distinct
  def addFactorsWithContext(context:C, result:Set[Factor]): Unit = result ++= factorsWithContext(context)
  //def addFactors[A<:Iterable[Factor] with Growable[Factor]](context:C, result:A): A = { result ++= factors(context); result } 
  //def filterByFactorClass[F<:Factor](factors:Iterable[Factor], fclass:Class[F]): Iterable[F] = factors.filter(f => fclass.isAssignableFrom(f.getClass)).asInstanceOf[Iterable[F]]
  def factorsWithContextOfClass[F<:Factor](context:C, fclass:Class[F]): Iterable[F] = filterByFactorClass(factorsWithContext(context), fclass)
  //def filterByFamilyClass[F<:Family](factors:Iterable[Factor], fclass:Class[F]): Iterable[F#Factor] = factors.filter(f => f match { case f:Family#Factor => fclass.isAssignableFrom(f.family.getClass); case _ => false }).asInstanceOf[Iterable[F#Factor]]
  def factorsWithContextOfFamilyClass[F<:Family](context:C, fclass:Class[F]): Iterable[F#Factor] = filterByFamilyClass[F](factorsWithContext(context), fclass)
  //def filterByFamily[F<:Family](factors:Iterable[Factor], family:F): Iterable[F#Factor] = factors.filter(f => f match { case f:Family#Factor => f.family.equals(family); case _ => false }).asInstanceOf[Iterable[F#Factor]]
  def factorsWithContextOfFamily[F<:Family](context:C, family:F): Iterable[F#Factor] = filterByFamily(factorsWithContext(context), family)
  //def filterByFamilies[F<:Family](factors:Iterable[Factor], families:Seq[F]): Iterable[F#Factor] = factors.filter(f => f match { case f:Family#Factor => families.contains(f.family); case _ => false }).asInstanceOf[Iterable[F#Factor]]
  def factorsWithContextOfFamilies[F<:Family](context:C, families:Seq[F]): Iterable[F#Factor] = filterByFamilies(factorsWithContext(context), families)
  def itemizedModelWithContext(context:C): ItemizedModel = new ItemizedModel(factorsWithContext(context))
  
  // Getting sums of scores from all neighboring factors
  def currentScore(context:C): Double = { var sum = 0.0; for (f <- factorsWithContext(context)) sum += f.currentScore; sum }
  def assignmentScore(context:C, assignment:Assignment): Double = { var sum = 0.0; for (f <- factorsWithContext(context)) sum += f.assignmentScore(assignment); sum }
}

trait DotFamilyModel extends Model {
  //abstract override def families: Seq[DotFamily]
}




// TODO Is this used?
/** Assumes that all calls to addFactors() will only add Factors of type FactorType, and then appropriately casts the return type of factors() methods. */
trait ModelWithFactorType extends Model {
  trait FactorType <: Factor
  /** Return all Factors in this Model that touch any of the given "variables".  The result will not have any duplicate Factors. */
  //override def factors(context:C): Iterable[FactorType] // = super.factors(context).asInstanceOf[FactorType] 
  //{ val result = newFactorsCollection; addFactors(context, newFactorsCollection).asInstanceOf[Iterable[FactorType]] }
  //override def addFactors[A<:Iterable[Factor] with Growable[Factor]](context:C, result:A): A = { result ++= super.factors(context); result } 
//  /** Return all Factors in this Model that touch the given "variable".  The result will not have any duplicate Factors. */
//  override def factors(variable:Variable): Iterable[FactorType] = { val set = newFactorsCollection; addFactors(variable, set); set.asInstanceOf[Iterable[FactorType]] }
//  /** Return all Factors in this Model that are affected by the given Diff.  The result will not have any duplicate Factors. */
//  override def factors(d:Diff): Iterable[FactorType] = if (d.variable eq null) Nil else { val set = newFactorsCollection; addFactors(d, set); set.asInstanceOf[Iterable[FactorType]] }
//  /** Return all Factors in this Model that are affected by the given DiffList.  The result will not have any duplicate Factors. */
//  override def factors(dl:DiffList): Iterable[FactorType] = if (dl.size == 0) Nil else { val set = newFactorsCollection; addFactors(dl, set); set.asInstanceOf[Iterable[FactorType]] }
}


/** A Model that explicitly stores all factors, with an efficient map from variables to their neighboring factors.
    @author Andrew McCallum
    @since 0.11
 */
class ItemizedModel(initialFactors:Factor*) extends Model {
  def this(initialFactors:Iterable[Factor]) = { this(initialFactors.toSeq:_*) }
  private val _factors = new HashMap[Var,scala.collection.Set[Factor]] {
    override def default(v:Var) = ListSet.empty[Factor]
  }
  this ++= initialFactors
  // TODO The next method needs to handle ContainerVariables.
  override def addFactors(variable:Var, result:Set[Factor]): Unit = result ++= _factors(variable)
  def factors(variable:Var): Iterable[Factor] = _factors(variable)
  //override def addFactors[A<:Iterable[Factor] with Growable[Factor]](variable:Variable, result:A): A = result ++= _factors(variable)
  //override def addFactors(variable:Variable, result:Set[Factor]): Unit = result ++= _factors(variable)
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
    @since 0.11
 */
class CombinedModel(theSubModels:Model*) extends Model {
  val subModels = new ArrayBuffer[Model] ++= theSubModels
  def +=(model:Model): Unit = subModels += model
  def ++=(models:Iterable[Model]): Unit = subModels ++= models
  def factors(context:Var): Iterable[Factor] = { val result = newFactorsCollection; addFactors(context, result); result }
  //override def newFactorsCollection: ListBuffer[Factor] = new collection.mutable.ListBuffer[Factor]
  override def addFactors(variable:Var, result:Set[Factor]): Unit = {
    //override def addFactors[A<:Iterable[Factor] with Growable[Factor]](context:C, result:A): A = 
    val len = subModels.length; var s = 0
    while (s < len) { subModels(s).addFactors(variable, result); s += 1 }
    result
  }
}

class TemplateModel(theSubModels:ModelAsTemplate*) extends Model with Weights {
  val templates = new ArrayBuffer[ModelAsTemplate] ++= theSubModels
  def +=[M<:ModelAsTemplate](model:M): M = { templates += model; model }
  def ++=[M<:ModelAsTemplate](models:Iterable[M]): Iterable[M] = { templates ++= models; models }
  def factors(context:Var): Iterable[Factor] = { val result = newFactorsCollection; addFactors(context, result); result }
  override def addFactors(variable:Var, result:Set[Factor]): Unit = { templates.foreach(_.addFactors(variable, result)); result }
  //override def variables = subModels.flatMap(_.variables) // TODO Does this need normalization, de-duplication?
  //override def factors = subModels.flatMap(_.factors) // TODO Does this need normalization, de-duplication?
  def families: Seq[Family] = templates
    // Getting parameter weight Tensors for models; only really works for Models whose parameters are in Families
  //def weights: Tensor = weightsTensor
  lazy val weights: Tensors = new Tensors(families.filter(_.isInstanceOf[DotFamily]).map(f => (f,f.asInstanceOf[DotFamily].weightsTensor)))
}


trait ProxyModel[C1,C2] extends ModelWithContext[C2] {
  def model: ModelWithContext[C1]
}

