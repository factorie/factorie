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
import scala.collection.mutable.{ArrayBuffer,HashMap,LinkedHashSet}
import scala.collection.immutable.ListSet
import scala.collection.mutable.Set

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
  //type FactorType <: Factor
  /** Append to "result" all Factors in this Model that touch the given "variable".  This method must not append duplicates. */
  def addFactors(variable:Variable, result:Set[Factor]): Unit
  //def addFactors[F>:FactorType](variable:Variable, result:Growable[F]): Unit = addFactors(Seq(variable), result)
  /** Append to "result" all Factors in this Model that touch any of the given "variables".  This method must not append duplicates. */
  def addFactors(variables:Iterable[Variable], result:Set[Factor]): Unit  = for (v <- variables) addFactors(v, result)
  //def addFactors[F>:FactorType](variables:Iterable[Variable], result:Growable[F]): Unit
  /** Append to "result" all Factors in this Model that are affected by the given Diff.  This method must not append duplicates. */
  def addFactors(d:Diff, result:Set[Factor]): Unit = if (d.variable ne null) addFactors(d.variable, result)
  //def addFactors[F>:FactorType](d:Diff, result:Growable[F]): Unit = if (d.variable ne null) addFactors(d.variable, result)
  /** Append to "result" all Factors in this Model that are affected by the given DiffList.  This method must not append duplicates. */
  def addFactors(dl:DiffList, result:Set[Factor]): Unit = if (dl.size > 0) addFactors(dl.foldLeft(List[Variable]())((vs,d) => if (d.variable ne null) d.variable :: vs else vs), result)
  //def addFactors[F>:FactorType](dl:DiffList, result:Growable[F]): Unit = if (dl.size > 0) addFactors(dl.foldLeft(List[Variable]())((vs,d) => if (d.variable ne null) d.variable :: vs else vs), result)
  /** The "factors" methods need a new collection to return; this method is used by them to construct this collection. */
  //protected def newFactorsCollection: collection.mutable.Iterable[FactorType] with Growable[FactorType] = new collection.mutable.ListBuffer[FactorType]
  protected def newFactorsCollection: Set[Factor] = new collection.mutable.LinkedHashSet[Factor]
  /** Return all Factors in this Model that touch any of the given "variables".  The result will not have any duplicate Factors. */
  def factors(variables:Iterable[Variable]): Iterable[Factor] = { val set = newFactorsCollection; addFactors(variables, set); set }
  //def factors(variables:Iterable[Variable]): Iterable[FactorType] = { val set = newFactorsCollection; addFactors(variables, set); set }
  /** Return all Factors in this Model that touch the given "variable".  The result will not have any duplicate Factors. */
  def factors(variable:Variable): Iterable[Factor] = { val set = newFactorsCollection; addFactors(variable, set); set }
  //def factors(variable:Variable): Iterable[FactorType] = { val set = newFactorsCollection; addFactors(variable, set); set }
  /** Return all Factors in this Model that are affected by the given Diff.  The result will not have any duplicate Factors. */
  def factors(d:Diff): Iterable[Factor] = if (d.variable eq null) Nil else { val set = newFactorsCollection; addFactors(d, set); set }
  //def factors(d:Diff): Iterable[FactorType] = if (d.variable eq null) Nil else { val set = newFactorsCollection; addFactors(d, set); set }
  /** Return all Factors in this Model that are affected by the given DiffList.  The result will not have any duplicate Factors. */
  def factors(dl:DiffList): Iterable[Factor] = if (dl.size == 0) Nil else { val set = newFactorsCollection; addFactors(dl, set); set } //factors(dl.foldLeft(List[Variable]())((vs,d) => if (d.variable ne null) d.variable :: vs else vs))
  //def factors(dl:DiffList): Iterable[FactorType] = if (dl.size == 0) Nil else { val set = newFactorsCollection; addFactors(dl, set); set } //factors(dl.foldLeft(List[Variable]())((vs,d) => if (d.variable ne null) d.variable :: vs else vs))

  def filterByFactorClass[F<:Factor](factors:Iterable[Factor], fclass:Class[F]): Iterable[F] = factors.filter(f => fclass.isAssignableFrom(f.getClass)).asInstanceOf[Iterable[F]]
  def factorsOfClass[F<:Factor](variables:Iterable[Variable], fclass:Class[F]): Iterable[F] = filterByFactorClass(factors(variables), fclass)
  def factorsOfClass[F<:Factor](variables:Iterable[Variable])(implicit fm:Manifest[F]): Iterable[F] = factorsOfClass(variables, fm.erasure.asInstanceOf[Class[F]])
  def factorsOfClass[F<:Factor](d:DiffList, fclass:Class[F]): Iterable[F] = filterByFactorClass(factors(d), fclass)
  def factorsOfClass[F<:Factor](d:DiffList)(implicit fm:Manifest[F]): Iterable[F] = factorsOfClass[F](d, fm.erasure.asInstanceOf[Class[F]])

  def filterByFamilyClass[F<:Family](factors:Iterable[Factor], fclass:Class[F]): Iterable[F#Factor] =
    factors.filter(f => f match {
      case f:Family#Factor => fclass.isAssignableFrom(f.family.getClass)
      case _ => false
    }).asInstanceOf[Iterable[F#Factor]]
  def factorsOfFamilyClass[F<:Family](variables:Iterable[Variable], fclass:Class[F]): Iterable[F#Factor] = filterByFamilyClass[F](factors(variables), fclass)
  def factorsOfFamilyClass[F<:Family](variables:Iterable[Variable])(implicit fm:Manifest[F]): Iterable[F#Factor] = factorsOfFamilyClass[F](variables, fm.erasure.asInstanceOf[Class[F]])
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
  def factorsOfFamily[F<:Family](variables:Iterable[Variable], family:F): Iterable[F#Factor] = filterByFamily(factors(variables), family)
  def factorsOfFamily[F<:Family](d:DiffList, family:F): Iterable[F#Factor] = filterByFamily(factors(d), family)
  def factorsOfFamilies[F<:Family](variables:Iterable[Variable], families:Seq[F]): Iterable[F#Factor] = filterByFamilies(factors(variables), families)
  def factorsOfFamilies[F<:Family](d:DiffList, families:Seq[F]): Iterable[F#Factor] = filterByFamilies(factors(d), families)
  
  // TODO Add score(Iterable[Variable],TypedAssignment[Variable]): Double
  def score(variables:Iterable[Variable]): Double = { var sum = 0.0; for (f <- factors(variables)) sum += f.currentScore; sum } // factors(variables).foldLeft(0.0)((sum, f) => sum + f.score)
  def score(variable:Variable): Double = { var sum = 0.0; for (f <- factors(variable)) sum += f.currentScore; sum }
  def score(d:DiffList) : Double = { var sum = 0.0; for (f <- factors(d)) sum += f.currentScore; sum }
  /** Returns the average score, that is score of variables, normalized by the size of the collections vars. */
  def aveScore(variables:Iterable[Variable]): Double = score(variables) / variables.size  // TODO Rename to scoreAve?

  /** Deduplicate a sequence of Factors.
      This method should be called on all Iterable[Factor] before they are returned by methods such as "factors" */
  // TODO Remove this method
//  private def dedup[F<:Factor](factors:Iterable[F]): Iterable[F] = {
//    if (factors.size == 1) factors
//    else if (factors.size == 2) {
//      if (factors.head == factors.last) factors.head
//      else factors
//    } else {
//      val result = new scala.collection.mutable.LinkedHashSet[F]
//      result ++= factors
//      result
//    }
//  }
  
  // Some Model subclasses have a list of Families to which all its factors belong
  def families: Seq[Family] = throw new Error("Model class does not implement method 'families': "+ this.getClass.getName)
  def familiesOfClass[F<:Family](fclass:Class[F]): Seq[F] = families.filter(f => fclass.isAssignableFrom(f.getClass)).asInstanceOf[Seq[F]]
  def familiesOfClass[F<:Family]()(implicit m:Manifest[F]): Seq[F] = familiesOfClass[F](m.erasure.asInstanceOf[Class[F]])

  // Getting parameter weight Tensors for models; only really works for Models whose parameters are in Families
  //def weightsTensor: ConcatenatedTensor = new ConcatenatedTensor(familiesOfClass[DotFamily].map(_.weights))
  def weightsTensor: WeightsTensor = { val t = new WeightsTensor(f => throw new Error); familiesOfClass[DotFamily].foreach(f => t(f) = f.weights); t }
  //def newDenseWeightsTensor: WeightsTensor = { val t = new WeightsTensor(f => throw new Error); familiesOfClass[DotFamily].foreach(f => t(f) = f.newDenseTensor); t }
  //def newSparseWeightsTensor: WeightsTensor = { val t = new WeightsTensor(f => throw new Error); familiesOfClass[DotFamily].foreach(f => t(f) = f.newSparseTensor); t }
  def newDenseWeightsTensor: WeightsTensor = new WeightsTensor(dotFamily => la.Tensor.newDense(dotFamily.weights))
  def newSparseWeightsTensor: WeightsTensor = new WeightsTensor(dotFamily => la.Tensor.newSparse(dotFamily.weights))

  // Some Model subclasses that have a fixed set of factors and variables can override the methods below
  // TODO Consider making a Model trait for these methods.  Yes!
  def variables: Iterable[Variable] = throw new Error("Model class does not implement method 'variables': "+ this.getClass.getName)
  def factors: Iterable[Factor] = throw new Error("Model class does not implement method 'factors': "+ this.getClass.getName)
  def score: Double = { var s = 0.0; for (f <- factors) s += f.currentScore; s } 
}


/** Assumes that all calls to addFactors() will only add Factors of type FactorType, and then appropriately casts the return type of factors() methods. */
trait TypedModel extends Model {
  trait FactorType <: Factor
  /** Return all Factors in this Model that touch any of the given "variables".  The result will not have any duplicate Factors. */
  override def factors(variables:Iterable[Variable]): Iterable[FactorType] = { val set = newFactorsCollection; addFactors(variables, set); set.asInstanceOf[Iterable[FactorType]] }
  /** Return all Factors in this Model that touch the given "variable".  The result will not have any duplicate Factors. */
  override def factors(variable:Variable): Iterable[FactorType] = { val set = newFactorsCollection; addFactors(variable, set); set.asInstanceOf[Iterable[FactorType]] }
  /** Return all Factors in this Model that are affected by the given Diff.  The result will not have any duplicate Factors. */
  override def factors(d:Diff): Iterable[FactorType] = if (d.variable eq null) Nil else { val set = newFactorsCollection; addFactors(d, set); set.asInstanceOf[Iterable[FactorType]] }
  /** Return all Factors in this Model that are affected by the given DiffList.  The result will not have any duplicate Factors. */
  override def factors(dl:DiffList): Iterable[FactorType] = if (dl.size == 0) Nil else { val set = newFactorsCollection; addFactors(dl, set); set.asInstanceOf[Iterable[FactorType]] }
}


/** A Model that concatenates the factors of multiple contained models.
    @author Andrew McCallum
    @since 0.11
 */
class CombinedModel(theSubModels:Model*) extends Model {
  val subModels = new ArrayBuffer[Model] ++= theSubModels
  def +=(model:Model): Unit = subModels += model
  def ++=(models:Iterable[Model]): Unit = subModels ++= models
  def addFactors(variable:Variable, result:Set[Factor]): Unit = subModels.foreach(_.addFactors(variable, result))
  override def addFactors(variables:Iterable[Variable], result:Set[Factor]): Unit = subModels.foreach(_.addFactors(variables, result))
  override def addFactors(d:Diff, set:Set[Factor]): Unit = if (d.variable ne null) subModels.foreach(_.addFactors(d, set))
  override def addFactors(dl:DiffList, set:Set[Factor]): Unit = if (dl.size > 0) subModels.foreach(_.addFactors(dl, set))
  override def variables = subModels.flatMap(_.variables) // TODO Does this need normalization, de-duplication?
  override def factors = subModels.flatMap(_.factors) // TODO Does this need normalization, de-duplication?
  override def families: Seq[Family] = subModels.flatMap(_.families) // filterByClass(classOf[Family]).toSeq

  protected def filename: String = throw new Error("Not yet implemented")
  def save(dirname:String, gzip: Boolean = false): Unit = throw new Error("Not yet implemented")
  def load(dirname:String, gzip: Boolean = false): Unit = throw new Error("Not yet implemented")
  def loadFromJar(dirname:String): Unit = throw new Error("Unsupported")
}

/** A Model that explicitly stores all factors, with an efficient map from variables to their neighboring factors.
    @author Andrew McCallum
    @since 0.11
 */
class ItemizedModel(initialFactors:Factor*) extends Model {
  def this(initialFactors:Iterable[Factor]) = { this(initialFactors.toSeq:_*) }
  private val _factors = new HashMap[Variable,scala.collection.Set[Factor]] {
    override def default(v:Variable) = ListSet.empty[Factor]
  }
  this ++= initialFactors
  // TODO The next method needs to handle ContainerVariables.
  def addFactors(variable:Variable, result:Set[Factor]): Unit = result ++= _factors(variable)
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

/** A Model that creates factors on the fly from Templates.
    @author Andrew McCallum
    @since 0.8
    @see Template
 */
//@deprecated("Use CombinedModel instead") // TODO But we need to implement "save" and "load".
//class TemplateModel(initialTemplates:Template*) extends Model {
//  private val _templates = new ArrayBuffer[Template] ++= initialTemplates
//  def templates: Seq[Template] = _templates
//  def ++=(moreTemplates:Iterable[Template]) = _templates ++= moreTemplates
//  def +=(template:Template) = _templates += template
//  @deprecated("Will be removed") def clear = _templates.clear // TODO Consider removing this.
//  override def families = _templates
//  def limitDiscreteValuesIteratorAsIn(variables:Iterable[DiscreteVar]): Unit = _templates.foreach(_.limitDiscreteValuesIteratorAsIn(variables))
//  def addFactors(variable:Variable, result:Growable[Factor]): Iterable[Factor] = templates.flatMap(template => template.factorsWithDuplicates(variable))
//  
//  def save(dirname:String, gzip: Boolean = false): Unit = {
//    import java.io.File
//    //println("Saving model "+getClass.getName+" to "+dirname)
//    val f = new File(dirname)
//    // Recursively delete all files in directory "f"
//    def delete(f:File): Boolean = { if (f.isDirectory) f.listFiles.forall(f2 => delete(f2)) else f.delete }
//    if (f.exists) if (!delete(f)) throw new Error("Error deleting directory "+dirname)
//    f.mkdir
//    templates.foreach(_.save(dirname, gzip))
//  }
//  def load(dirname:String, gzip: Boolean = false): Unit = templates.foreach(_.load(dirname, gzip))
//  def loadFromJar(dirname: String): Unit = templates.foreach(_.loadFromJar(dirname))
//}


//trait NewModel {
//  /** Append to "result" all Factors in this Model that touch any of the given "variables".  This method must not append duplicates. */
//  def addFactors(variables:Iterable[Variable], result:collection.generic.Growable[Factor]): Unit
//  /** Append to "result" all Factors in this Model that touch the given "variable".  This method must not append duplicates. */
//  def addFactors(variable:Variable, result:collection.generic.Growable[Factor]): Unit = addFactors(Seq(variable), result)
//  /** Append to "result" all Factors in this Model that are affected by the given Diff.  This method must not append duplicates. */
//  def addFactors(d:Diff, result:collection.generic.Growable[Factor]): Unit = if (d.variable ne null) addFactors(d.variable, result)
//  /** Append to "result" all Factors in this Model that are affected by the given DiffList.  This method must not append duplicates. */
//  def addFactors(dl:DiffList, result:collection.generic.Growable[Factor]): Unit = if (dl.size > 0) addFactors(dl.foldLeft(List[Variable]())((vs,d) => if (d.variable ne null) d.variable :: vs else vs), result)
//  /** The "factors" methods need a new collection to return; this method is used by them to construct this collection. */
//  protected def newFactorsCollection: collection.mutable.Iterable[Factor] with collection.generic.Growable[Factor] = new collection.mutable.ListBuffer[Factor]
//  /** Return all Factors in this Model that touch any of the given "variables".  The result will not have any duplicate Factors. */
//  final def factors(variables:Iterable[Variable]): Iterable[Factor] = { val set = newFactorsCollection; addFactors(variables, set); set }
//  /** Return all Factors in this Model that touch the given "variable".  The result will not have any duplicate Factors. */
//  final def factors(variable:Variable): Iterable[Factor] = { val set = newFactorsCollection; addFactors(variable, set); set }
//  /** Return all Factors in this Model that are affected by the given Diff.  The result will not have any duplicate Factors. */
//  final def factors(d:Diff): Iterable[Factor] = if (d.variable eq null) Nil else { val set = newFactorsCollection; addFactors(d, set); set }
//  /** Return all Factors in this Model that are affected by the given DiffList.  The result will not have any duplicate Factors. */
//  final def factors(dl:DiffList): Iterable[Factor] = if (dl.size == 0) Nil else { val set = newFactorsCollection; addFactors(dl, set); set } //factors(dl.foldLeft(List[Variable]())((vs,d) => if (d.variable ne null) d.variable :: vs else vs))
//}
//
//trait NewTemplate[C] {
//  def factors(context:C): Iterable[Factor]
//  def itemizedModel(context:C) = new ItemizedModel(factors(context))
//}
//
//
//abstract class NewTemplateModel2[N1<:Variable,N2<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2]) extends NewModel {
//  def unroll1(v:N1): Iterable[Factor]
//  def unroll2(v:N2): Iterable[Factor]
//  // We must assume that variables does not contain duplicates; if debug flag on, then check.
//  def addFactors(variables:Iterable[Variable], set:collection.generic.Growable[Factor]): Unit = {
//    val factorList = new collection.mutable.ListBuffer[Factor]
//    if (nm1.erasure ne nm2.erasure) { 
//      for (v <- variables) {
//        if (nm1.erasure.isAssignableFrom(v.getClass)) {
//          val fs = unroll1(v.asInstanceOf[N1])
//          fs match { case fs:IterableSingleFactor[Factor] => factorList += fs.factor; case _ => factorList ++= fs }
//        }
//        if (nm2.erasure.isAssignableFrom(v.getClass)) set ++= unroll2(v.asInstanceOf[N2])
//      }
//    } else {
//      // We have to be more careful about deduplication
//    }
//    if (factorList.size == 1) set += factorList.head
//    else if (factorList.size == 2) { val head = factorList.head; val last = factorList.last; if (head != last) { set += head; set += last } else set += head }
//    else if (factorList.size > 2) { val unique = new collection.mutable.HashSet[Factor]; for (f <- factorList) if (!unique.contains(f)) { set += f; unique += f } }
//  }
//}
//
//class NewCombinedModel(theSubModels:NewModel*) extends NewModel {
//  val subModels = new ArrayBuffer[NewModel] ++= theSubModels
//  def +=(model:NewModel): Unit = subModels += model
//  def ++=(models:Iterable[NewModel]): Unit = subModels ++= models
//  def addFactors(variables:Iterable[Variable], set:collection.generic.Growable[Factor]): Unit = subModels.foreach(_.addFactors(variables, set))
//  override def addFactors(variable:Variable, set:collection.generic.Growable[Factor]): Unit = subModels.foreach(_.addFactors(variable, set))
//  override def addFactors(d:Diff, set:collection.generic.Growable[Factor]): Unit = if (d.variable ne null) subModels.foreach(_.addFactors(d, set))
//  override def addFactors(dl:DiffList, set:collection.generic.Growable[Factor]): Unit = if (dl.size > 0) subModels.foreach(_.addFactors(dl, set))
//  //override def variables = subModels.flatMap(_.variables) // TODO Does this need normalization, de-duplication?
//  //override def factors = subModels.flatMap(_.factors) // TODO Does this need normalization, de-duplication?
//}
//
//
//object NewTemplateModel {
//  def main(args:Array[String]): Unit = {
//    val t = new NewTemplateModel2[BooleanVariable,BooleanVariable] {
//      def unroll1(b:BooleanVariable): Iterable[Factor] = { println("unroll1"); new Factor2(b,b) { def score(b1:BooleanValue, b2:BooleanValue) = 0.0 } }
//      def unroll2(b:BooleanVariable): Iterable[Factor] = { println("unroll1"); List(new Factor2(b,b) { def score(b1:BooleanValue, b2:BooleanValue) = 0.0 }) }
//    }
//    println(t.factors(List(new IntegerVariable(2))))
//  }
//}
