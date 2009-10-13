package cc.factorie.er2
import scala.collection.mutable.{HashSet}
import cc.factorie._


trait Accessor[A,B] {
	def forward : A=>Iterable[B];
	def reverse : B=>Iterable[A];
}
trait Accessor1[A,B] extends Accessor[A,B] {
	def forward1 : A=>B
	def reverse1 : B=>A
	def forward = (a:A) => List(forward1(a))
	def reverse = (b:B) => List(reverse1(b))
}
class MultiAccessor[A,B,C](prefix:Accessor[A,B], aforward:B=>Iterable[C], areverse:C=>Iterable[B]) extends Accessor[A,C] {
	val _forward : A=>Iterable[C] = {
			if (prefix == null) {
				if (aforward == null) (a:A) => List(a).asInstanceOf[List[C]];
				else (a:A) => aforward(a.asInstanceOf[B]) 
			} else {
				if (aforward == null) (a:A) => prefix.forward(a).asInstanceOf[Iterable[C]];
				else (a:A) => { /*println("MultiAccessor.forward="+a);*/ prefix.forward(a).flatMap(aforward) }
			}
	}
	val _reverse : C=>Iterable[A] = {
			if (prefix == null) {
				if (areverse == null) (c:C) => List(c).asInstanceOf[List[A]];
				else (c:C) => areverse(c).asInstanceOf[Iterable[A]]
			} else {
				if (areverse == null) (c:C) => prefix.reverse(c.asInstanceOf[B])
				else (c:C) => { /*println("MultiAccessor.reverse="+c+" prefix="+prefix+" areverse="+areverse);*/ areverse(c).flatMap(prefix.reverse) }
			}
	} 
	def forward =  _forward
	def reverse = _reverse
}
class SingleAccessor[A,B,C](prefix:Accessor[A,B], aforward:B=>C, areverse:C=>B) extends Accessor[A,C] {
	def forward = a => prefix.forward(a).map(aforward(_))
	def reverse = b => { /*println("SingleAccessor.reverse="+b);*/ prefix.reverse(areverse(b)) }
}

class AttributeAccessor[A,B,C<:AttributeOf[B]](prefix:Accessor[A,B], aforward:B=>C) extends Accessor[A,C] {
	def forward = (a:A) => prefix.forward(a).map(aforward(_))
	def reverse = (c:C) => { val o = prefix.reverse(c.owner); /*println("AttributeAccessor o="+o);*/ o }
}

/*case class AccessorUnit[A] extends Accessor[A,A] { // Remove?
		def forward = (a:A) => List(a)
		def reverse = (a:A) => { /*println("Arg a="+a);*/ List(a) }
}*/

trait AccessorUnit {
	type AccessorUnitType <: Object
}




