package cc.factorie.er2
import scala.collection.mutable.{HashMap,HashSet}

trait Attribute extends Variable {
	type AttributeOwnerType <: Variable
	def owner : AttributeOwnerType
}

trait AttributeOf[O<:Variable] extends Attribute {
	type AttributeOwnerType = O
	def owner : O = this.getClass().getDeclaredField("$outer").get(this).asInstanceOf[O];
}


// More efficient alternative
trait AttributesOf[This<:AttributesOf[This] with Variable] {
  this : This =>
  def myself : This = this
  trait AttributeOfOuter extends cc.factorie.er2.Attribute {
    type AttributeOwnerType = This
    def owner = myself
  }
}

// TODO Arg!  All this self-typing [This] is a pain.
// consider making a trait
// trait SelfType[This<:SelfType[This]] {
//   type ThisType = This
// }

