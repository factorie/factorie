package cc.factorie.er1
import scala.collection.mutable.{HashMap,HashSet}

trait Attribute extends Variable {
	type AttributeOwnerType <: Variable
	def owner : AttributeOwnerType
}

trait AttributeOf[O<:Variable] extends Attribute {
	type AttributeOwnerType = O
	//this.getClass.getDeclaredFields.foreach(println(_))
	def owner : O = this.getClass().getDeclaredField("$outer").get(this).asInstanceOf[O];
	//def owner : O = this.`$outer` // Why doesn't this work?
	override def toString = this match {
	  case t : TypedCategoricalVariable[_] => owner.toString+"."+printName+"="+t.value
	  case _ => super.toString
	}
}

class SymmetricAttribute[O<:Variable](val getf:O=>SymmetricAttribute[O]) extends PrimitiveVariable[O] with AttributeOf[O] {
  protected def _set(newValue:O)(implicit d:DiffList) = super.set(newValue)(d)
  override def set(newValue:O)(implicit d:DiffList) = {
    if (value != null) getf(value)._set(newValue)(d)
    super.set(newValue)(d)
    if (value != null) getf(value)._set(value)(d)
  }
}

class BoolAttributeOf[O<:Variable] extends Bool with AttributeOf[O]

@deprecated
trait Attributes[This<:Attributes[This] with Variable] {
	this : This =>
	def myself : This = this
	trait Attribute extends cc.factorie.er1.Attribute {
		type AttributeOwnerType = This
		def owner = myself
		//def owner = `$outer`() // TODO Why doesn't this work?
	}
  class SymmetricFunction(initval:This, val getr:This=>SymmetricFunction) extends PrimitiveVariable(initval) {
    def this(b:This=>SymmetricFunction) = this(null.asInstanceOf[This], b)
  	override def set(newValue:This)(implicit d:DiffList) = {
  		if (value != null) getr(value)._set(null.asInstanceOf[This]) // TODO Why is this cast necessary?
  		super.set(newValue)(d)
  		if (newValue != null) getr(newValue)._set(myself)
  	}
  	protected def _set(newValue:This)(implicit d:DiffList) = super.set(newValue)(d)
  }
}



