package cc.factorie.generative
import cc.factorie._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

trait GenerativeModel extends Model {
  def getParentFactor(v:Variable): Option[GenerativeFactor]
  def getChildFactors(v:Variable): Option[Iterable[GenerativeFactor]]
  def parentFactor(v:Variable): GenerativeFactor
  def childFactors(v:Variable): Iterable[GenerativeFactor]
  def extendedParentFactors(v:Variable): Iterable[GenerativeFactor]
  def extendedChildFactors(v:Variable): Iterable[GenerativeFactor]
  def extendedParents(v:Variable): Iterable[Variable]
  def extendedChildren(v:Variable): Iterable[Variable]
  def parents(v:Variable): Seq[Variable] 
  def children(v:Variable): Iterable[Variable]
  def sampleFromParents(v:MutableVar[_])(implicit d:DiffList): Unit
}

trait MutableGenerativeModel extends GenerativeModel {
  def +=(f:GenerativeFactor): Unit
  def -=(f:GenerativeFactor): Unit
}

object GenerativeModel {
  /** Constructor for a default GenerativeModel */
  def apply(): ItemizedGenerativeModel = new ItemizedGenerativeModel
}

class ItemizedGenerativeModel extends MutableGenerativeModel {
  private val _parentFactor = new HashMap[Variable,GenerativeFactor]
  private val _childFactors = new HashMap[Variable,ArrayBuffer[GenerativeFactor]]
  override def addFactors(variable:Variable, result:scala.collection.mutable.Set[Factor]): Unit = {
    if (_parentFactor.contains(variable)) result += _parentFactor(variable)
    // TODO Do we need to use extendedParentFactors also?
    //if (_childFactors.contains(v)) result ++= _childFactors(v)
    if (_childFactors.contains(variable)) result ++= extendedChildFactors(variable)
    // TODO special handling of ContainerVariable[_]??
    //result
  }
//  override def addFactors[A<:Iterable[Factor] with collection.generic.Growable[Factor]](v:Variable, result:A): A = {
//    val set = new scala.collection.mutable.HashSet[Factor]
//    if (_parentFactor.contains(v)) set += _parentFactor(v)
//    // TODO Do we need to use extendedParentFactors also?
//    //if (_childFactors.contains(v)) result ++= _childFactors(v)
//    if (_childFactors.contains(v)) set ++= extendedChildFactors(v)
//    // TODO special handling of ContainerVariable[_]??
//    result ++= set
//    result
//  }
  def factors(v:Variable): Iterable[Factor] = { val result = new collection.mutable.HashSet[Factor]; addFactors(v, result); result }
//  override def factorsWithDuplicates(variables:Iterable[Variable]): Iterable[Factor] = {
//    val result = new ArrayBuffer[GenerativeFactor]
//    variables.foreach(v => {
//      if (_parentFactor.contains(v)) result += _parentFactor(v)
//      // TODO Do we need to use extendedParentFactors also?
//      //if (_childFactors.contains(v)) result ++= _childFactors(v)
//      if (_childFactors.contains(v)) result ++= extendedChildFactors(v)
//      // TODO special handling of ContainerVariable[_]??
//    })
//    result
//  }
//  def factorsWithDuplicates(v:Variable): Iterable[Factor] = {
//    val result = new ArrayBuffer[GenerativeFactor]
//    if (_parentFactor.contains(v)) result += _parentFactor(v)
//    // TODO Do we need to use extendedParentFactors also?
//    //if (_childFactors.contains(v)) result ++= _childFactors(v)
//    if (_childFactors.contains(v)) result ++= extendedChildFactors(v)
//    // TODO special handling of ContainerVariable[_]??
//    result
//  }
  def allFactors: Iterable[Factor] = _parentFactor.values ++ _childFactors.values.flatten
  def getParentFactor(v:Variable): Option[GenerativeFactor] = _parentFactor.get(v)
  def getChildFactors(v:Variable): Option[Iterable[GenerativeFactor]] = _childFactors.get(v)
  def parentFactor(v:Variable): GenerativeFactor = _parentFactor.getOrElse(v, null)
  def childFactors(v:Variable): Iterable[GenerativeFactor] = _childFactors.getOrElse(v, Nil)
  def extendedParentFactors(v:Variable): Iterable[GenerativeFactor] = {
    val result = new ArrayBuffer[GenerativeFactor]
    result ++= getParentFactor(v)
    for (parent <- parents(v); if (parent.isDeterministic)) result ++= extendedParentFactors(parent)
    result
  }
  def extendedChildFactors(v:Variable): Iterable[GenerativeFactor] = {
    if (!_childFactors.contains(v)) return Nil
    val result = new ArrayBuffer[GenerativeFactor]
    for (factor <- childFactors(v)) {
      result += factor
      if (factor.child.isDeterministic) result ++= extendedChildFactors(factor.child)
    }
    result
  }
  def extendedParents(v:Variable): Iterable[Variable] = extendedParentFactors(v).flatMap(_.parents)
  def extendedChildren(v:Variable): Iterable[Variable] = extendedChildFactors(v).map(_.child)
  def parents(v:Variable): Seq[Variable] = 
    if (_parentFactor.contains(v)) _parentFactor(v).parents else Nil
  def children(v:Variable): Iterable[Variable] = childFactors(v).map(_.child)

  def sampleFromParents(v:MutableVar[_])(implicit d:DiffList): Unit = v.set(parentFactor(v).sampledValue.asInstanceOf[v.Value])

  def +=(f:GenerativeFactor): Unit = {
    require(!_parentFactor.contains(f.child))
    _parentFactor(f.child) = f
    f.parents.foreach(v => _childFactors.getOrElseUpdate(v, new ArrayBuffer[GenerativeFactor]) += f)
  }
  def -=(f:GenerativeFactor): Unit = {
    require(_parentFactor(f.child) eq f)
    _parentFactor.remove(f.child)
    f.parents.foreach(v => _childFactors(v) -= f)
  }
  def ++=(fs:Iterable[GenerativeFactor]): Unit = fs.foreach(f => this.+=(f))
  def --=(fs:Iterable[GenerativeFactor]): Unit = fs.foreach(f => this.-=(f))
}
  