package cc.factorie.er2
import scala.collection.mutable.{HashSet}
import scala.reflect.Manifest
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
  
  def get[D<:AccessorType](fwd:C=>Iterable[D], bwd:D=>Iterable[C])(implicit m:Manifest[D]): Accessor[A,D] = {
    // TODO use same procedure as AccessorUnit to construct a A#AccessorType
    // Usage:  class PersonAccessor { def mother = get[Person](_.mother, _.children)
    throw new Error("Not yet implemented")
  } 
  //def getAttribute
  // TODO Avoid the need for lots of constructor arguments by making MultiAccessor have abstract method 'forward1', 
  // satisfied by subclass created here!
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

// TODO Consider creating an ObservationAccessor, which only needs to be one-way, since observations don't change.

// TODO avoid the need to mix this in to Person by using instead duck typing: type WithAccessorType = { type AccessorType <: Accessor[_,_] }
trait AccessorType {
  type AccessorType <: Accessor[_,_]
}

case class Arg[A] extends Accessor[A,A] { // Where is this supposed to come from?
  def forward = (a:A) => List(a)
  def reverse = (a:A) => { /*println("Arg a="+a);*/ List(a) }
}

/*object Accessor {
    /** Construct a new Accessor of type A. */
    def apply[A<:Accessor[_,_]](implicit m:Manifest[A]): A = {
      val constructors = m.erasure.getConstructors
      if (constructors.size != 1) throw new Error("Accessors must have only one constructor")
      val constructor = m.erasure.getConstructors.apply(0)
      val numArgs = constructor.getParameterTypes.length
      if (numArgs == 1) // I could just pass 'null' as the first argument to the constructors
        constructor.newInstance((Arg[A], (a:AnyRef)=>List(a), (a:AnyRef)=>List(a))).asInstanceOf[A]
      else if (numArgs == 3)
        constructor.newInstance(Arg[A], (a:AnyRef)=>List(a), (a:AnyRef)=>List(a)).asInstanceOf[A]
      else
        throw new Error("Accessor constructor args="+numArgs+"; must have either 1 argument or 3 arguments: type Tuple3 or Accessor,=>,=> ")
    }
  }*/

object AccessorUnit {
  /** Construct a new Accessor representing the beginning of an accessor chain, taking input A. */
  def apply[A<:AccessorType](implicit m:Manifest[A#AccessorType]): A#AccessorType = {
    println("AccessorUnit m="+m)
    val constructors = m.erasure.getConstructors
    if (constructors.size != 1) throw new Error("Accessors must have only one constructor")
    val constructor = m.erasure.getConstructors.apply(0)
    val numArgs = constructor.getParameterTypes.length
    if (numArgs == 1) // I could just pass 'null' as the first argument to the constructors
      constructor.newInstance((Arg[A#AccessorType], (a:AnyRef)=>List(a), (a:AnyRef)=>List(a))).asInstanceOf[A#AccessorType]
    else if (numArgs == 3)
      constructor.newInstance(Arg[A#AccessorType], (a:AnyRef)=>List(a), (a:AnyRef)=>List(a)).asInstanceOf[A#AccessorType]
    else
      throw new Error("Accessor constructor args="+numArgs+"; must have either 1 argument or 3 arguments: type Tuple3 or Accessor,=>,=> ")
  }
}


