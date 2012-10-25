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

trait Model[-C] {
  //def variables(context:C): Iterable[Variable]
  def factors(context:C): Iterable[Factor]
  def addFactors[A<:Iterable[Factor] with Growable[Factor]](context:C, result:A): A = { result ++= factors(context); result } 
  def newFactorsCollection: Iterable[Factor] with Growable[Factor] = new collection.mutable.LinkedHashSet[Factor]
  def filterByFactorClass[F<:Factor](factors:Iterable[Factor], fclass:Class[F]): Iterable[F] = factors.filter(f => fclass.isAssignableFrom(f.getClass)).asInstanceOf[Iterable[F]]
  def factorsOfClass[F<:Factor](context:C, fclass:Class[F]): Iterable[F] = filterByFactorClass(factors(context), fclass)
  def filterByFamilyClass[F<:Family](factors:Iterable[Factor], fclass:Class[F]): Iterable[F#Factor] =
    factors.filter(f => f match {
      case f:Family#Factor => fclass.isAssignableFrom(f.family.getClass)
      case _ => false
    }).asInstanceOf[Iterable[F#Factor]]
  def factorsOfFamilyClass[F<:Family](context:C, fclass:Class[F]): Iterable[F#Factor] = filterByFamilyClass[F](factors(context), fclass)
  def filterByFamily[F<:Family](factors:Iterable[Factor], family:F): Iterable[F#Factor] = 
    factors.filter(f => f match {
      case f:Family#Factor => f.family.equals(family)
      case _ => false
    }).asInstanceOf[Iterable[F#Factor]]
  def factorsOfFamily[F<:Family](context:C, family:F): Iterable[F#Factor] = filterByFamily(factors(context), family)
  def filterByFamilies[F<:Family](factors:Iterable[Factor], families:Seq[F]): Iterable[F#Factor] = 
    factors.filter(f => f match {
      case f:Family#Factor => families.contains(f.family)
      case _ => false
    }).asInstanceOf[Iterable[F#Factor]]
  def factorsOfFamilies[F<:Family](context:C, families:Seq[F]): Iterable[F#Factor] = filterByFamilies(factors(context), families)
  
  // Getting sums of scores from all neighboring factors
  // TODO Consider an alternative name to "sumScore".  Cannot name "score" because conflicts with Template1.score(N1#Value)
  // Consider factorsScore and factorsScoreAverage
  def currentScore(context:C): Double = { var sum = 0.0; for (f <- factors(context)) sum += f.currentScore; sum }
  def assignmentScore(context:C, assignment:Assignment): Double = { var sum = 0.0; for (f <- factors(context)) sum += f.assignmentScore(assignment); sum }

  // Some Model subclasses have a list of Families to which all its factors belong
  def families: Seq[Family] = throw new Error("Model class does not implement method 'families': "+ this.getClass.getName)
  def familiesOfClass[F<:Family](fclass:Class[F]): Seq[F] = families.filter(f => fclass.isAssignableFrom(f.getClass)).asInstanceOf[Seq[F]]
  def familiesOfClass[F<:Family]()(implicit m:Manifest[F]): Seq[F] = familiesOfClass[F](m.erasure.asInstanceOf[Class[F]])

  // Getting parameter weight Tensors for models; only really works for Models whose parameters are in Families
  //def weights: Tensor = weightsTensor
  def weightsTensor: Tensor = {
    val t = new WeightsTensor(f => f match {
      case f:DotFamily if (families.contains(f)) => f.weights.blankCopy // So that Model.weightsTensor.blankCopy will work
      case _ => throw new Error("Trying to add Tensor for DotFamily that was not part of initialization") 
    })
    familiesOfClass[DotFamily].foreach(f => t(f) = f.weights); t
  }
  def newWeightsTensor: Tensor = weightsTensor.blankCopy
  def newDenseWeightsTensor: WeightsTensor = new WeightsTensor(dotFamily => la.Tensor.newDense(dotFamily.weights))
  def newSparseWeightsTensor: WeightsTensor = new WeightsTensor(dotFamily => la.Tensor.newSparse(dotFamily.weights))

  // Some Model subclasses that have a fixed set of factors and variables can override the methods below
  // TODO Consider making a Model trait for these methods.  Yes!
  def variables: Iterable[Variable] = throw new Error("Model class does not implement method 'variables': "+ this.getClass.getName)
  def factors: Iterable[Factor] = throw new Error("Model class does not implement method 'factors': "+ this.getClass.getName)
  def currentScore: Double = { var s = 0.0; for (f <- factors) s += f.currentScore; s } 
}

trait DotFamilyModel[-C] extends Model[C] {
  //abstract override def families: Seq[DotFamily]
}


/** Assumes that all calls to addFactors() will only add Factors of type FactorType, and then appropriately casts the return type of factors() methods. */
trait ModelWithFactorType[-C] extends Model[C] {
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
class ItemizedModel(initialFactors:Factor*) extends Model[Variable] {
  def this(initialFactors:Iterable[Factor]) = { this(initialFactors.toSeq:_*) }
  private val _factors = new HashMap[Variable,scala.collection.Set[Factor]] {
    override def default(v:Variable) = ListSet.empty[Factor]
  }
  this ++= initialFactors
  // TODO The next method needs to handle ContainerVariables.
  def factors(variable:Variable): Iterable[Factor] = _factors(variable)
  override def addFactors[A<:Iterable[Factor] with Growable[Factor]](variable:Variable, result:A): A = result ++= _factors(variable)
  //override def addFactors(variable:Variable, result:Set[Factor]): Unit = result ++= _factors(variable)
  override def factors: Iterable[Factor] = _factors.values.flatten.toSeq.distinct
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
class CombinedModel[C](theSubModels:Model[C]*) extends Model[C] {
  val subModels = new ArrayBuffer[Model[C]] ++= theSubModels
  def +=(model:Model[C]): Unit = subModels += model
  def ++=(models:Iterable[Model[C]]): Unit = subModels ++= models
  def factors(context:C): Iterable[Factor] = addFactors(context, newFactorsCollection)
  override def newFactorsCollection: ListBuffer[Factor] = new collection.mutable.ListBuffer[Factor]
  override def addFactors[A<:Iterable[Factor] with Growable[Factor]](context:C, result:A): A = {
    val len = subModels.length; var s = 0
    while (s < len) { subModels(s).addFactors(context, result); s += 1 }
    result
  }
  override def variables = subModels.flatMap(_.variables) // TODO Does this need normalization, de-duplication?
  override def factors = subModels.flatMap(_.factors) // TODO Does this need normalization, de-duplication?
  override def families: Seq[Family] = subModels.flatMap(_.families) // filterByClass(classOf[Family]).toSeq

  protected def filename: String = throw new Error("Not yet implemented")
  def save(dirname:String, gzip: Boolean = false): Unit = throw new Error("Not yet implemented")
  def load(dirname:String, gzip: Boolean = false): Unit = throw new Error("Not yet implemented")
  def loadFromJar(dirname:String): Unit = throw new Error("Unsupported")
}

class TemplateModel(theSubModels:ModelAsTemplate*) extends Model[Variable] {
  val templates = new ArrayBuffer[ModelAsTemplate] ++= theSubModels
  def +=(model:ModelAsTemplate): Unit = templates += model
  def ++=(models:Iterable[ModelAsTemplate]): Unit = templates ++= models
  def factors(context:Variable): Iterable[Factor] = addFactors(context, newFactorsCollection)
  override def newFactorsCollection: ListBuffer[Factor] = new collection.mutable.ListBuffer[Factor]
  override def addFactors[A<:Iterable[Factor] with Growable[Factor]](context:Variable, result:A): A = { templates.foreach(_.addFactors(context, result)); result }
  //override def variables = subModels.flatMap(_.variables) // TODO Does this need normalization, de-duplication?
  //override def factors = subModels.flatMap(_.factors) // TODO Does this need normalization, de-duplication?
  override def families: Seq[Family] = templates

  protected def filename: String = throw new Error("Not yet implemented")
  def save(dirname:String, gzip: Boolean = false): Unit = {
    import java.io.File
    //println("Saving model "+getClass.getName+" to "+dirname)
    val f = new File(dirname)
    // Recursively delete all files in directory "f"
    def delete(f:File): Boolean = { if (f.isDirectory) f.listFiles.forall(f2 => delete(f2)) else f.delete }
    if (f.exists) if (!delete(f)) throw new Error("Error deleting directory "+dirname)
    f.mkdir
    templates.foreach(_.save(dirname, gzip))
  }
  def load(dirname:String, gzip: Boolean = false): Unit = templates.foreach(_.load(dirname, gzip))
  def loadFromJar(dirname: String): Unit = templates.foreach(_.loadFromJar(dirname))
}


trait ProxyModel[C1,C2] extends Model[C2] {
  def model: Model[C1]
  override def newFactorsCollection: ArrayBuffer[Factor] = new collection.mutable.ArrayBuffer[Factor]
  //override def addFactors[A<:Iterable[Factor] with Growable[Factor]](context:C2, result:A): A = { subModels.foreach(_.addFactors(context, result)); result }
  override def variables = model.variables
  override def factors = model.factors
  override def families = model.families
}

class Variable2DiffListModel(val model:Model[Variable]) extends ProxyModel[Variable,DiffList] {
  def factors(context:DiffList): Iterable[Factor] = {
    val result = new collection.mutable.LinkedHashSet[Factor]
    assert(model ne null)
    assert(context ne null)
    assert(context.variables.forall(_ ne null))
    context.variables.foreach(v => model.addFactors(v, result))
    result
  }
}

//class Variable2IterableModel[V<:Variable](val model:Model[V]) extends ProxyModel[V,Iterable[V]] {
//  def factors(context:Iterable[V]): Iterable[Factor] = {
//    val result = new collection.mutable.LinkedHashSet[Factor]
//    context.foreach(v => model.addFactors(v, result))
//    result
//  }
//}

class Element2IterableModel[C](val model:Model[C]) extends ProxyModel[C,Iterable[C]] {
  def factors(context:Iterable[C]): Iterable[Factor] = {
    val result = new collection.mutable.LinkedHashSet[Factor]
    context.foreach(v => model.addFactors(v, result))
    result
  }
}

//class Variable2DiffListModel(val model:Model[Variable]) extends ProxyModel[Variable,DiffList] {
//  def factors(dl:DiffList): Iterable[Factor] = {
//    val result = new collection.mutable.LinkedHashSet[Factor] // Because there might be duplicates, even of Variables in the DiffList
//    dl.foreach(d => if (d.variable ne null) model.addFactors(d.variable, result))
//    result
//  }  
//}



