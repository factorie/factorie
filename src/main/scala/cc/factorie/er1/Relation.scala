package cc.factorie.er1
import scala.collection.mutable.{HashMap,HashSet}
import scala.reflect.Manifest

/** Representing a directed relationship from src to dst.  
 * Its boolean value indicates whether the relationship is actually present. */
case class Relationship[A<:Variable,B<:Variable](src:A, dst:B) extends Bool 

/** Represents a many-to-many relation in an Entity-Relationship model */
class Relation[A<:Variable,B<:Variable] extends Collection[Relationship[A,B]] with GetterN[A,B] {
	private val a2t = new HashMap[A,HashMap[B,RelationshipType]] // a to tuple
	private val b2t = new HashMap[B,HashMap[A,RelationshipType]] // a to tuple
 	def newRelationship(a:A, b:B) : RelationshipType = new Relationship(a,b)
  type RelationshipType = cc.factorie.er1.Relationship[A,B]
  protected def addRelationship(r:RelationshipType) = {
    r := true
    //println("Relationship add "+r.src+" "+r.dst)
		a2t.getOrElseUpdate(r.src, new HashMap[B,RelationshipType])(r.dst) = r
		b2t.getOrElseUpdate(r.dst, new HashMap[A,RelationshipType])(r.src) = r
  }
  protected def removeRelationship(r:RelationshipType) = {
    r := false
		a2t(r.src) -= r.dst
		b2t(r.dst) -= r.src
  }
	protected def addEntry(pair:(A,B)) : RelationshipType = {
	  val r = newRelationship(pair._1, pair._2)
	  addRelationship(r)
		r
	}
	protected def removeEntry(pair:(A,B)) : RelationshipType = {
		val r = a2t(pair._1)(pair._2)
		removeRelationship(r)
		r
	}
	def add(a:A, b:B)(implicit d:DiffList) = { val r = addEntry((a,b)); if (d != null) d += RelationAddDiff(r) }
	def remove(a:A, b:B)(implicit d:DiffList) = { val r = removeEntry((a,b)); if (d != null) d += RelationRemoveDiff(r) }
	def size = a2t.values.foldLeft(0)(_+_.size)
	def elements = a2t.values.flatMap(_.values)
	def get1(b:B) : Iterable[RelationshipType] = if (b2t.contains(b)) b2t(b).values.toList else Nil // TODO make this more efficient
	def get2(a:A) : Iterable[RelationshipType] = if (a2t.contains(a)) a2t(a).values.toList else Nil
	def forward : A=>Iterable[B] = (a:A) => get2(a).map(_.dst)
	def reverse : B=>Iterable[A] = (b:B) => get1(b).map(_.src)
	def apply(a:A,b:B) = add(a,b)(null)
	//def apply(g1:Getter[_,A],g2:Getter[_,B]) : Formula = new ... 
	def +=(pair:(A,B)) = add(pair._1, pair._2)(null)
	def ++=(abs:Iterable[(A,B)]) = abs.foreach(pair => +=(pair))
	def -=(pair:(A,B)) = remove(pair._1, pair._2)(null)
	def --=(abs:Iterable[(A,B)]) = abs.foreach(pair => -=(pair))
	def contains(a:A,b:B) = a2t(a).contains(b)
	protected override def stringPrefix ="Relation"
	case class RelationAddDiff(r:RelationshipType) extends Diff {
	  def variable = if (r.value) r else null
	  def redo = addRelationship(r)
	  def undo = removeRelationship(r)
	} 
	case class RelationRemoveDiff(r:RelationshipType) extends Diff {
	  def variable = if (r.value) r else null
	  def redo = removeRelationship(r)
	  def undo = addRelationship(r)
	} 
}

// TODO It would be easier to make *Relation be a SingleIndexedVariable (for use with SamplingInferencer) if Relation(Amy,Bob) were a SingleIndexedVariable
class ItemizedRelation[A<:ItemizedVariable[A],B<:ItemizedVariable[B]](implicit ma:Manifest[A], mb:Manifest[B]) extends Relation[A,B] with Variable with IterableSettings {
	def settings = new SettingIterator {
	  var i = -1
	  val domainb = Domain[B](mb) 
	  val a = Domain[A](ma).randomValue // randomly pick a src
	  val max = domainb.size - 1 // we will iterate over all possible changes to dst's
	  def hasNext = i < max
	  def set(d:DiffList) : Unit = { val b = domainb.get(i); if (contains(a,b)) remove(a,b)(d) else add(a,b)(d) }
	  def next(difflist:DiffList) = { i += 1; val d = newDiffList; set(d); d }
	  def reset = i = -1
	}

}

class SymmetricFunction[A>:Null<:Variable] extends Getter1[A,A] {
	private val a2t = new HashMap[A,A];
	def add(a:A,b:A) = {
	  if (a2t.contains(a)) a2t -= a2t(a)
	  if (a2t.contains(b)) a2t -= a2t(b)
		a2t(a) = b
		a2t(b) = a
	}
	def remove(a:A,b:A) = {
		a2t -= a
		a2t -= b
	}
	def size = a2t.size / 2
	def contains(pair:(A,A)) = a2t.contains(pair._1)
	def elements = a2t.elements.filter(pair=>pair._1.hashCode < pair._2.hashCode)
	def get(a:A) = a2t.getOrElse(a, null)
	def apply(a1:A, a2:A) = this.add(a1,a2)
	def forward1 = (a:A) => a2t.getOrElse(a,null)
	def reverse1 = (a:A) => a2t.getOrElse(a,null)
	//protected override def stringPrefix ="SymmetricFunction" 
}



