package cc.factorie.generative
import cc.factorie._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

trait GenerativeModel extends Model {
  def getParentFactor(v:Variable): Option[GenerativeFactor]
  def getChildFactors(v:Variable): Option[Seq[GenerativeFactor]]
  def parentFactor(v:Variable): GenerativeFactor
  def childFactors(v:Variable): Seq[GenerativeFactor]
  def extendedParentFactors(v:Variable): Seq[GenerativeFactor]
  def extendedChildFactors(v:Variable): Seq[GenerativeFactor]
  def extendedParents(v:Variable): Seq[Variable]
  def extendedChildren(v:Variable): Seq[Variable]
  def parents(v:Variable): Seq[Variable] 
  def children(v:Variable): Seq[Variable]
  def sampleFromParents(v:MutableVar)(implicit d:DiffList): Unit
}

class GenerativeFactorModel extends GenerativeModel {
  private val _parentFactors = new HashMap[Variable,GenerativeFactor]
  private val _childFactors = new HashMap[Variable,ArrayBuffer[GenerativeFactor]]
  def factors(variables:Iterable[Variable]): Seq[Factor] = {
    val result = new ArrayBuffer[GenerativeFactor]
    variables.foreach(v => {
      if (_parentFactors.contains(v)) result += _parentFactors(v)
      // TODO Do we need to use extendedParentFactors also?
      //if (_childFactors.contains(v)) result ++= _childFactors(v)
      if (_childFactors.contains(v)) result ++= extendedChildFactors(v)
      // TODO special handling of ContainerVariable[_]??
    })
    normalize(result)
  }
  def getParentFactor(v:Variable): Option[GenerativeFactor] = _parentFactors.get(v)
  def getChildFactors(v:Variable): Option[Seq[GenerativeFactor]] = _childFactors.get(v)
  def parentFactor(v:Variable): GenerativeFactor = _parentFactors.getOrElse(v, null)
  def childFactors(v:Variable): Seq[GenerativeFactor] = _childFactors.getOrElse(v, Nil)
  def extendedParentFactors(v:Variable): Seq[GenerativeFactor] = {
    val result = new ArrayBuffer[GenerativeFactor]
    result ++= getParentFactor(v)
    for (parent <- parents(v); if (parent.isDeterministic)) result ++= extendedParentFactors(parent)
    result
  }
  def extendedChildFactors(v:Variable): Seq[GenerativeFactor] = {
    if (!_childFactors.contains(v)) return Nil
    val result = new ArrayBuffer[GenerativeFactor]
    for (factor <- childFactors(v)) {
      result += factor
      if (factor.child.isDeterministic) result ++= extendedChildFactors(factor.child)
    }
    result
  }
  def extendedParents(v:Variable): Seq[Variable] = extendedParentFactors(v).flatMap(_.parents)
  def extendedChildren(v:Variable): Seq[Variable] = extendedChildFactors(v).map(_.child)
  def parents(v:Variable): Seq[Variable] = 
    if (_parentFactors.contains(v)) _parentFactors(v).parents else Nil
  def children(v:Variable): Seq[Variable] = childFactors(v).map(_.child)

  def sampleFromParents(v:MutableVar)(implicit d:DiffList): Unit = v.set(parentFactor(v).sampledValue.asInstanceOf[v.Value])

  def +=(f:GenerativeFactor): Unit = {
    require(!_parentFactors.contains(f.child))
    _parentFactors(f.child) = f
    f.parents.foreach(v => _childFactors.getOrElseUpdate(v, new ArrayBuffer[GenerativeFactor]) += f)
  }
  def -=(f:GenerativeFactor): Unit = { require(_parentFactors(f.child) eq f); _parentFactors.remove(f.child); _childFactors(f.child) -= f }
  def ++=(fs:Iterable[GenerativeFactor]): Unit = fs.foreach(f => this.+=(f))
  def --=(fs:Iterable[GenerativeFactor]): Unit = fs.foreach(f => this.-=(f))
}
  