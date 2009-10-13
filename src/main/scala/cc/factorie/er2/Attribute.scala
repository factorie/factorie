package cc.factorie.er2
import scala.collection.mutable.{HashMap,HashSet}

trait Attribute extends Variable {
	type AttributeOwner <: Variable
	def owner : AttributeOwner
}

trait AttributeOf[O<:Variable] extends Attribute {
	type AttributeOwner = O
	def owner : O = this.getClass().getDeclaredField("$outer").get(this).asInstanceOf[O];
}

