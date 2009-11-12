package cc.factorie.er2
import scala.collection.mutable.{HashSet}
import scala.reflect.Manifest
import cc.factorie._

// Because we are lacking Scala 2.8 package objects
object er2 {
	type Faccessor[A,B] = A=>Iterable[B];
	type Baccessor[A,B] = B=>Iterable[A];
	type AccessorArgs[A,B,C] = (Accessor[A,B], B=>Iterable[C], C=>Iterable[B])
}

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

/** Just to save typing by users in definitions of new MultiAccessors */
import cc.factorie.er2.er2._
class MultiAccessor1[A,B,C](args:AccessorArgs[A,B,C]) extends MultiAccessor[A,B,C](args._1, args._2, args._3)

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

/*
@deprecated
trait AccessorUnit {
	type AccessorUnitType <: Object
}*/

trait AccessorType {
  type AccessorType <: Accessor[_,_]
}

case class Arg[A] extends Accessor[A,A] { // Where is this supposed to come from?
  def forward = (a:A) => List(a)
  def reverse = (a:A) => { /*println("Arg a="+a);*/ List(a) }
}

object AccessorUnit {
  def apply[A<:AccessorType](implicit m:Manifest[A#AccessorType]): A#AccessorType = {
    println("AccessorUnit m="+m)
    val constructors = m.erasure.getConstructors
    if (constructors.size != 1) throw new Error("Accessors must have only one constructor")
    val constructor = m.erasure.getConstructors.apply(0)
    val numArgs = constructor.getParameterTypes.length
    if (numArgs == 1)
      constructor.newInstance((Arg[A#AccessorType], (a:AnyRef)=>List(a), (a:AnyRef)=>List(a))).asInstanceOf[A#AccessorType]
    else if (numArgs == 3)
      constructor.newInstance(Arg[A#AccessorType], (a:AnyRef)=>List(a), (a:AnyRef)=>List(a)).asInstanceOf[A#AccessorType]
    else
    	throw new Error("Accessor constructor args="+numArgs+"; must have either 1 argument or 3 arguments: type Tuple3 or Accessor,=>,=> ")
  }
}


