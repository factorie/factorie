package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.ArrayStack
import cc.factorie._

// I'd like to create a cc.factorie.er package and spread these class definitions across multiple files
// but I need Scala 2.8 package objects for the implicit conversions.

/** Classes and functions supporting Entity-Relationship languages for creating Templates. */
object er {
  
  // Define attributes and entities

  /** A generic Attribute */
  trait AttributeOf[E] {
    def attributeOwner: E
  }
  /** A trait for entities that have attributes.  Provides an inner trait 'Attribute' for its attribute classes. */
  // TODO Change this name because it is too generic.  Might be desired for coref.
  trait Entity[This<:Entity[This] with Variable] extends Variable with AccessorType[This] {
    this: This =>
    //type AccessorType[This] <: AccessorTail[This]
    def thisEntity: This = this
    //def entityName = classOf[This].getName
    trait Attribute extends cc.factorie.er.AttributeOf[This] {
      override def attributeOwner: This = thisEntity
    }
  }
  
  
  // Accessors for bi-directional relations

  // TODO Consider renaming these "Getter", matching the 'get' method name.
  /** An abstract Accessor will have methods providing forward and reverse mappings through many-to-many relations. */
  /*trait Accessor0 {
    type A// <: Variable
    type B
    type C
    protected var prefix: Accessor0 with AccessorTail[B] with AccessorHead[A]
    protected var forward1s: B=>C
    protected var reverse1s: C=>B
    protected var forward1m: B=>Iterable[C]
    protected var reverse1m: C=>Iterable[B]
    def forward : A=>Iterable[C];
    def reverse : C=>Iterable[A];
  }*/
  trait AccessorHead[A1,C1] extends Accessor[C1] {
    type A = A1
  }
  //trait AccessorTail[C1] extends Accessor0 { type C = C1 }
  trait AccessorMiddle[B1,C1] extends Accessor[C1] {
    type B = B1
  }
  type Accessor2[A1,C1] = AccessorHead[A1,C1]
  /** An abstract trait that provides forward and reverse mappings through one-to-one relations. */ // TODO No longer needed
  /*trait Accessor1[A,B] extends Accessor0 with AccessorHead[A] with AccessorTail[B] {
    def forward1 : A=>B
    def reverse1 : B=>A
    def forward = (a:A) => List(forward1(a))
    def reverse = (b:B) => List(reverse1(b))
  }*/
  /** A class that provides forward and reverse mappings through relations, 
      enabling creation of nested mappings through what look like normal one-way access method calls.
      For examples of its usage, see example/LogicDemo*.scala.
      Typically, post-construction, callers would immediately change the prefix, forward1m (or forward1s) and reverse1m (or reverse1s).  
      You can make the "unit" (initial) element for a chain by constructing FooAccessor[Foo,Foo,Foo],
      and leaving all the above vars unchanged. */
  // Change to class Accessor[C1] extends Accessor0 with AccessorTail[C1]
  trait Accessor[C1] {
    type A// <: Variable
    type B
    type C = C1
    protected var prefix: Accessor2[A,B] = null
    protected var forward1s: B=>C = (b:B) => b.asInstanceOf[C] // Default no-op for "unit" Accessor
    protected var reverse1s: C=>B = (c:C) => c.asInstanceOf[B] // Default no-op for "unit" Accessor
    protected var forward1m: B=>Iterable[C] = null
    protected var reverse1m: C=>Iterable[B] = null
    private lazy val _forward: A=>Iterable[C] = { // the different ways of combining prefix with different forward1* to append a link to the chain
      if (prefix == null) {
        if (forward1m == null)
          (a:A) => List(forward1s(a.asInstanceOf[B]))
        else
          (a:A) => forward1m(a.asInstanceOf[B])
      } else {
      	if (forward1m == null)
      		(a:A) => prefix.forward(a).map(forward1s)
      	else
          (a:A) => prefix.forward(a).flatMap(forward1m)
      }
    }
    private lazy val _reverse: C=>Iterable[A] = { // the different ways of combining prefix with different reverse1* to prepend a link to the chain
      if (prefix == null) {
        if (reverse1m == null)
          (c:C) => List(reverse1s(c)).asInstanceOf[Iterable[A]]
        else
          (c:C) => reverse1m(c).asInstanceOf[Iterable[A]]
      } else {
        if (reverse1m == null)
          (c:C) => prefix.reverse(reverse1s(c))
        else
          (c:C) => reverse1m(c).flatMap(prefix.reverse)
      }
    }
    def forward: A=>Iterable[C] = _forward //(a:A) => if (prefix == null) forward1(a.asInstanceOf[B]) else prefix.forward(a).flatMap(forward1)
    def reverse: C=>Iterable[A] = _reverse //(c:C) => if (prefix == null) reverse1(c).asInstanceOf[Iterable[A]] else reverse1(c).flatMap(prefix.reverse)
    /** Create a new Accessor, starting from this one and appending an additional many-to-many mapping. */
    def getManyToMany[D<:AccessorType[D]](fwd1:C=>Iterable[D], rev1:D=>Iterable[C])(implicit m:Manifest[D#AccessorType]): D#AccessorType with AccessorHead[A,D] with AccessorMiddle[C,D] = {
      val ret = newAccessor[D](m).asInstanceOf[D#AccessorType with AccessorHead[A,D] with AccessorMiddle[C,D]]
      ret.prefix = Accessor.this.asInstanceOf[Accessor[C] with AccessorHead[A,C]]
      ret.forward1m = fwd1
      ret.reverse1m = rev1
      ret
    }
    /** Alias for getManyToMany */
    def get[D<:AccessorType[D]](fwd1:C=>Iterable[D], rev1:D=>Iterable[C])(implicit m:Manifest[D#AccessorType]): D#AccessorType with AccessorHead[A,D] with AccessorMiddle[C,D] = getManyToMany[D](fwd1, rev1)(m)
    /** Create a new Accessor, starting from this one and appending an additional many-to-one mapping. */
    def getManyToOne[D<:AccessorType[D]](fwd1:C=>D, rev1:D=>Iterable[C])(implicit m:Manifest[D#AccessorType]): D#AccessorType with AccessorHead[A,D] with AccessorMiddle[C,D] = {
      val ret = newAccessor[D](m).asInstanceOf[D#AccessorType with AccessorHead[A,D] with AccessorMiddle[C,D]]
      ret.prefix = Accessor.this.asInstanceOf[Accessor[C] with AccessorHead[A,C]]
      ret.forward1s = fwd1
      ret.reverse1m = rev1
      ret
    } 
    /** Create a new Accessor, starting from this one and appending an additional one-to-one mapping. */
    def getOneToOne[D<:AccessorType[D]](fwd1:C=>D, rev1:D=>C)(implicit m:Manifest[D#AccessorType]): D#AccessorType with AccessorHead[A,D] with AccessorMiddle[C,D] = {
      val ret = newAccessor[D](m).asInstanceOf[D#AccessorType with AccessorHead[A,D] with AccessorMiddle[C,D]]
      ret.prefix = Accessor.this.asInstanceOf[Accessor[C] with AccessorHead[A,C]]
      ret.forward1s = fwd1
      ret.reverse1s = rev1
      ret
    } 
    /** Create a new Accessor, starting from this one and appending an additional one-to-many mapping. */
    def getOneToMany[D<:AccessorType[D]](fwd1:C=>Iterable[D], rev1:D=>C)(implicit m:Manifest[D#AccessorType]): D#AccessorType with AccessorHead[A,D] with AccessorMiddle[C,D] = {
      val ret = newAccessor[D](m).asInstanceOf[D#AccessorType with AccessorHead[A,D] with AccessorMiddle[C,D]]
      ret.prefix = Accessor.this.asInstanceOf[Accessor[C] with AccessorHead[A,C]]
      ret.forward1m = fwd1
      ret.reverse1s = rev1
      ret
    }
    //def getSymmetric[D<:C with AccessorType](fwd1:C=>Iterable[D])(implicit m:Manifest[D#AccessorType]) = getOneToOne[D](fwd1,fwd1)(m)
    /** Create a new Accessor, starting from this one and appending a mapping to one of its Attributes. */
    def getAttribute[D<:AttributeOf[C]](fwd1:C=>D)(implicit m:Manifest[D]): Accessor[D] with AccessorHead[A,D] with AccessorMiddle[C,D] = {
      val ret = new Accessor[D] with AccessorHead[A,D] with AccessorMiddle[C,D] //.asInstanceOf[]
      ret.prefix = Accessor.this.asInstanceOf[Accessor[C] with AccessorHead[A,C]]
      ret.forward1s = (c:C) => fwd1(c)
      ret.reverse1s = (d:D) => d.attributeOwner
      ret
    }
  }
  trait EntityAccessor[A<:Entity[A]] extends Accessor[A] // NO! with AccessorHead[A,A]
  // TODO avoid the need to mix this in to Person by using instead duck typing: type WithAccessorType = { type AccessorType <: Accessor[_,_] }
  trait AccessorType[D] {
    type AccessorType <: Accessor[D] //[_,_]
  }
  /** Construct a new Accessor of type A. */
  def newAccessor[A<:AccessorType[A]](implicit m:Manifest[A#AccessorType]): A#AccessorType = {
  	val constructors = m.erasure.getConstructors
    if (constructors.size != 1) throw new Error("Accessors must have only one constructor")
    val constructor = m.erasure.getConstructors.apply(0)
    val numArgs = constructor.getParameterTypes.length
    if (numArgs != 0) throw new Error("Accessor constructors must not take any arguments.")
    constructor.newInstance().asInstanceOf[A#AccessorType]
  }
  /** Currently unused. */ // TODO Remove this.
  /*trait AccessorUnit0[A] extends Accessor2[A,A] {
    def forward = (a:A) => List(a)
    def reverse = (a:A) => { /*println("AccessorUnit0 a="+a);*/ List(a) }
  }*/
  /** Construct a new Accessor representing the beginning of an accessor chain, taking input A. */
  def newAccessorUnit[A<:AccessorType[A]](implicit m:Manifest[A#AccessorType]): A#AccessorType with AccessorHead[A,A] = {
  	println("AccessorUnit m="+m)
  	newAccessor[A](m).asInstanceOf[A#AccessorType with AccessorHead[A,A]];
  	//a.asInstanceOf[A#AccessorType]
  }
  
  
  // Define function for scoring compatibility between accessor targets with CategoricalValues
  // Example usage:  Forany[Token] { t => Score(t, t.label) }
  
	case class Score[X<:Variable](sns:ScoreNeighbor0[X]*) {
  	def manifests : Seq[Manifest[_<:Variable]] = sns.flatMap(_.manifests)
  	def accessors : Seq[Accessor2[X,CategoricalValues]] = sns.flatMap(_.accessors)
  }
  trait ScoreNeighbor0[X<:Variable] {
  	def manifests : Iterable[Manifest[CategoricalValues]];
  def accessors : Iterable[Accessor2[X,CategoricalValues]]
  }
  class ScoreNeighbor[X<:Variable,A<:CategoricalValues](a1:Accessor2[X,A])(implicit mx:Manifest[X], ma:Manifest[A]) extends ScoreNeighbor0[X] {
  	def manifests = List(ma.asInstanceOf[Manifest[CategoricalValues]])
  	def accessors = List(a1.asInstanceOf[Accessor2[X,CategoricalValues]])
  }
  
  implicit def accessor2scoreneighbor[X<:Variable,A<:CategoricalValues](a:Accessor[A] with AccessorHead[X,A])(implicit mx:Manifest[X], ma:Manifest[A]): ScoreNeighbor0[X] = 
    new ScoreNeighbor(a)(mx,ma)
  
  
  // Define functions for clauses in first-order logic
  // Example usage: Forany[Person] { p => p.smokes ==> p.cancer }
  
  trait Formula[X<:Variable] {
    def eval(x:ArrayStack[BooleanValue]) : Boolean
    def manifests : Seq[Manifest[_<:Variable]]
    def accessors : Seq[Accessor2[X,BooleanValue]]
    def ==>(f:Formula[X]) = Implies(this, f)
    def ^(f:Formula[X]) = And(this, f)
    def v(f:Formula[X]) = Or(this, f)
    def !: = Not(this)
    def <==>(f:Formula[X]) = BooleanEquals(this, f)
  }
  case class Term[X<:Variable,A<:BooleanValue](g1:Accessor2[X,A])(implicit mx:Manifest[X], ma:Manifest[A]) extends Formula[X] {
    def eval(x:ArrayStack[BooleanValue]) = x.pop.value
    def manifests = List(ma)
    def accessors = List(g1.asInstanceOf[Accessor2[X,BooleanValue]])
  }

  implicit def accessor2formula[X<:Variable,A<:BooleanValue](g:Accessor[A] with AccessorHead[X,A])(implicit mx:Manifest[X], ma:Manifest[A]) : Formula[X] = new Term(g)(mx,ma)
  
  abstract class Formula1[X<:Variable](c1:Formula[X]) extends Formula[X] {
    def manifests = c1.manifests
    def accessors = c1.accessors
  }
  case class Not[X<:Variable](f1:Formula[X]) extends Formula1(f1) {
    def eval(x:ArrayStack[BooleanValue]) = ! f1.eval(x)
  }
  case class True[X<:Variable](f1:Formula[X]) extends Formula1(f1) { // noop, but forces implicit conversion to Term
    def eval(x:ArrayStack[BooleanValue]) = f1.eval(x)
  }
  abstract class Formula2[X<:Variable](c1:Formula[X], c2:Formula[X]) extends Formula[X] {
    def manifests = c1.manifests ++ c2.manifests
    def accessors = c1.accessors ++ c2.accessors
  }
  case class And[X<:Variable](c1:Formula[X], c2:Formula[X]) extends Formula2[X](c1,c2) {
    def eval(x:ArrayStack[BooleanValue]) = c1.eval(x) && c2.eval(x) 
  }
  case class Or[X<:Variable](c1:Formula[X], c2:Formula[X]) extends Formula2[X](c1,c2) {
    def eval(x:ArrayStack[BooleanValue]) = c1.eval(x) || c2.eval(x) 
  }
  case class Implies[X<:Variable](c1:Formula[X], c2:Formula[X]) extends Formula2[X](c1,c2) {
    def eval(x:ArrayStack[BooleanValue]) = (! c1.eval(x)) || c2.eval(x)
  }
  case class BooleanEquals[X<:Variable](c1:Formula[X], c2:Formula[X]) extends Formula2[X](c1,c2) {
    def eval(x:ArrayStack[BooleanValue]) = c1.eval(x) == c2.eval(x)
  }
  case class Forall[X<:Variable](f1:Formula[X]) extends Formula1(f1) {
    def eval(x:ArrayStack[BooleanValue]) = throw new Error("Not yet implemented") // Need to think carefully about this
  }
  case class Forsome[X<:Variable](f1:Formula[X]) extends Formula1(f1) {
    def eval(x:ArrayStack[BooleanValue]) = throw new Error("Not yet implemented") // Need to think carefully about this
  }

  trait LogicStatistics extends DotStatistics1[BooleanValue] {
    def *(w:Double) : this.type = { this.weights(0) = 0.0; this.weights(1) = Math.log(w); this }
  }
  
  
  // Application of Score and logic primitives starting with an Accessor
  object Forany {
    def apply[X<:AccessorType[X] with Variable](x2c:X#AccessorType with AccessorHead[X,X]=>Formula[X])(implicit m:Manifest[X#AccessorType]): Template with LogicStatistics = {
      type I = BooleanValue
      val accessorRoot: X#AccessorType with AccessorHead[X,X] = newAccessorUnit[X](m)
      val formula = x2c(accessorRoot)
      val manifests = formula.manifests.asInstanceOf[Seq[Manifest[I]]];
      assert(formula.accessors.length == 1)
      val accessors = formula.accessors
      val size = manifests.length
      size match {
        case 1 => new Template1[I]()(manifests(0)) with LogicStatistics {
          override def unroll1(n1:I) = { val roots = accessors(0).reverse(n1); if (!roots.isEmpty) Factor(n1) else Nil }
          def statistics(n1:I) = { val s = new ArrayStack[I]; s+=n1; Stat(Bool(formula.eval(s))) }
        }.init
        case 2 => new Template2[I,I]()(manifests(0), manifests(1)) with LogicStatistics {
          def unroll1(n1:I) = { val roots = accessors(0).reverse(n1); for (root <- roots; n2 <- accessors(1).forward(root)) yield Factor(n1,n2) }
          def unroll2(n2:I) = { val roots = accessors(1).reverse(n2); for (root <- roots; n1 <- accessors(0).forward(root)) yield Factor(n1,n2) }
          def statistics(n1:I, n2:I) = { val s = new ArrayStack[I]; s+=n2; s+=n1; Stat(Bool(formula.eval(s))) }
        }.init
        case 3 => new Template3[I,I,I]()(manifests(0), manifests(1), manifests(2)) with LogicStatistics {
          def unroll1(n1:I) = { val roots = accessors(0).reverse(n1); for (root <- roots; n2 <- accessors(1).forward(root); n3 <- accessors(2).forward(root)) yield Factor(n1,n2,n3) } 
          def unroll2(n2:I) = { val roots = accessors(1).reverse(n2); for (root <- roots; n1 <- accessors(0).forward(root); n3 <- accessors(2).forward(root)) yield Factor(n1,n2,n3) } 
          def unroll3(n3:I) = { val roots = accessors(2).reverse(n3); for (root <- roots; n1 <- accessors(0).forward(root); n2 <- accessors(1).forward(root)) yield Factor(n1,n2,n3) } 
          def statistics(n1:I, n2:I, n3:I) = { val s = new ArrayStack[I]; s+=n3; s+=n2; s+=n1; Stat(Bool(formula.eval(s))) }
        }.init
        case 4 => new Template4[I,I,I,I]()(manifests(0), manifests(1), manifests(2), manifests(3)) with LogicStatistics {
          def unroll1(n1:I) = { val roots = accessors(0).reverse(n1); for (root <- roots; n2 <- accessors(1).forward(root); n3 <- accessors(2).forward(root); n4 <- accessors(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
          def unroll2(n2:I) = { val roots = accessors(1).reverse(n2); for (root <- roots; n1 <- accessors(0).forward(root); n3 <- accessors(2).forward(root); n4 <- accessors(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
          def unroll3(n3:I) = { val roots = accessors(2).reverse(n3); for (root <- roots; n1 <- accessors(0).forward(root); n2 <- accessors(1).forward(root); n4 <- accessors(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
          def unroll4(n4:I) = { val roots = accessors(3).reverse(n4); for (root <- roots; n1 <- accessors(0).forward(root); n2 <- accessors(1).forward(root); n3 <- accessors(2).forward(root)) yield Factor(n1,n2,n3,n4) } 
          def statistics(n1:I, n2:I, n3:I, n4:I) = { val s = new ArrayStack[I]; s+=n4; s+=n3; s+=n2; s+=n1; Stat(Bool(formula.eval(s))) }
        }.init
      }
    }
  }
  object For {
    def apply[X<:AccessorType[X] with Variable](x2c:X#AccessorType=>Score[X])(implicit m:Manifest[X#AccessorType]) = {
    	val score = x2c(newAccessorUnit[X](m))
    	val manifests = score.manifests.toList.asInstanceOf[List[Manifest[CategoricalValues]]];
    	val accessors = score.accessors
    	val size = manifests.length
    	type I = CategoricalValues
    	size match {
    		case 1 => new TemplateWithDotStatistics1[I]()(manifests(0)) {
    			override def unroll1(n1:I) = { val roots = accessors(0).reverse(n1); if (!roots.isEmpty) Factor(n1) else Nil }
    		}
    		case 2 => new TemplateWithDotStatistics2[I,I]()(manifests(0), manifests(1)) {
    			def unroll1(n1:I) = { val roots = accessors(0).reverse(n1); for (root <- roots; n2 <- accessors(1).forward(root)) yield Factor(n1,n2) }
    			def unroll2(n2:I) = { val roots = accessors(1).reverse(n2); for (root <- roots; n1 <- accessors(0).forward(root)) yield Factor(n1,n2) }
    		}
    		case 3 => new TemplateWithDotStatistics3[I,I,I]()(manifests(0), manifests(1), manifests(2)) {
    			def unroll1(n1:I) = { val roots = accessors(0).reverse(n1); for (root <- roots; n2 <- accessors(1).forward(root); n3 <- accessors(2).forward(root)) yield Factor(n1,n2,n3) } 
    			def unroll2(n2:I) = { val roots = accessors(1).reverse(n2); for (root <- roots; n1 <- accessors(0).forward(root); n3 <- accessors(2).forward(root)) yield Factor(n1,n2,n3) } 
    			def unroll3(n3:I) = { val roots = accessors(2).reverse(n3); for (root <- roots; n1 <- accessors(0).forward(root); n2 <- accessors(1).forward(root)) yield Factor(n1,n2,n3) } 
    		}
    		case 4 => new TemplateWithDotStatistics4[I,I,I,I]()(manifests(0), manifests(1), manifests(2), manifests(3)) {
    			def unroll1(n1:I) = { val roots = accessors(0).reverse(n1); for (root <- roots; n2 <- accessors(1).forward(root); n3 <- accessors(2).forward(root); n4 <- accessors(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
    			def unroll2(n2:I) = { val roots = accessors(1).reverse(n2); for (root <- roots; n1 <- accessors(0).forward(root); n3 <- accessors(2).forward(root); n4 <- accessors(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
    			def unroll3(n3:I) = { val roots = accessors(2).reverse(n3); for (root <- roots; n1 <- accessors(0).forward(root); n2 <- accessors(1).forward(root); n4 <- accessors(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
    			def unroll4(n4:I) = { val roots = accessors(3).reverse(n4); for (root <- roots; n1 <- accessors(0).forward(root); n2 <- accessors(1).forward(root); n3 <- accessors(2).forward(root)) yield Factor(n1,n2,n3,n4) } 
    		}
    	}
    }
  }

  

}

