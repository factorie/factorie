package cc.factorie.er2
import scala.collection.mutable.{HashSet}
import cc.factorie._

class SymmetricFunction[A](initval:A, val getr:A=>SymmetricFunction[A]) extends PrimitiveVariable(initval) {
	def this(b:A=>SymmetricFunction[A]) = this(null.asInstanceOf[A], b)
	protected lazy val owner : A = this.getClass().getDeclaredField("$outer").get(this).asInstanceOf[A];
	override def set(newValue:A)(implicit d:DiffList) = {
		if (value != null) getr(value)._set(null.asInstanceOf[A]) // TODO Why is this cast necessary?
		super.set(newValue)(d)
		if (newValue != null) getr(newValue)._set(owner)
	}
	protected def _set(newValue:A)(implicit d:DiffList) = super.set(newValue)(d)
}
class SymmetricFunctionAccessor[A,B](prefix:Accessor[A,B], rel:SymmetricFunction[B]) extends Accessor[A,B] {
  def forward = (a:A) => prefix.forward(a).map(rel.getr(_).value)
  def reverse = (b:B) => prefix.reverse(rel.getr(b).value)
}

class Relation[A,B](val getra:A=>Relation[A,B], val getrb:B=>Relation[B,A]) extends SetVariable[B] {
	protected val back = new HashSet[B] // those b:B's who have relation (b,this); note that this may not have the relation (this,b)
	protected def owner : A = this.getClass().getDeclaredField("$outer").get(this).asInstanceOf[A];
	override def add(b:B)(implicit d:DiffList): Unit = if (!this.contains(b)) { getrb(b).back += owner; super.add(b)(d); if (d != null) d += RelationAddDiff(b) }
	override def remove(b:B)(implicit d:DiffList) : Unit = if (this.contains(b)) { getrb(b).back -= owner; super.remove(b)(d); if (d != null) d += RelationRemoveDiff(b) }
 	case class RelationAddDiff(b:B)    extends Diff { def variable = Relation.this; def redo = getrb(b).back += owner; def undo = getrb(b).back -= owner }
 	case class RelationRemoveDiff(b:B) extends Diff { def variable = Relation.this; def redo = getrb(b).back -= owner; def undo = getrb(b).back += owner }
}
class RelationAccessor[A,B,C](prefix:Accessor[A,B], getrb:B=>Relation[B,C], getrc:C=>Relation[C,B]) extends Accessor[A,C] {
  def forward = (a:A) => prefix.forward(a).flatMap(b => getrb(b).members)
  def reverse = (c:C) => getrc(c).members.flatMap(prefix.reverse(_)) 
}



