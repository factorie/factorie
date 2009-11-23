package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.{ArrayStack,HashSet}
import cc.factorie._

// I'd like to create a cc.factorie.er package and spread these class definitions across multiple files
// but I need Scala 2.8 package objects for the implicit conversions.

/** Classes and functions supporting Entity-Relationship languages for creating Templates. */
object er {
  
  // Define attributes and entities

  /** A generic Attribute */
  trait AttributeOf[E] {
    /** The entity that is described by this attribute. */
    def attributeOwner: E
    /** Print the owner of the attribute before the rest of its toString representation. */
    override def toString = attributeOwner.toString+":"+super.toString
  }
  // I considered changing this trait name because of concerns that it is too generic.  For example it might be desired for coref.
  // But now I think it is good for the "entity-relationship" package.  Users can always specify it with "er.Entity", which isn't bad.
  /** A trait for entities that have attributes.  Provides an inner trait 'Attribute' for its attribute classes. */
  trait Entity[This<:Entity[This] with Variable with GetterType[This]] extends Variable with GetterType[This] {
    this: This =>
    type EntityType = This with GetterType[This]
    def thisEntity: This = this
    /** Sub-trait of cc.factorie.er.AttributeOf that has a concrete implementation of 'attributeOwner'. */
    trait Attribute extends cc.factorie.er.AttributeOf[This] with GetterType[Attribute] {
      type GetterClass = AttributeGetter[Attribute,This]
      def attributeOwner: This = thisEntity
    }
  	class SymmetricFunction(initval:This, val get:This=>SymmetricFunction) extends RefVariable(initval) {
  		type EntityType = This with GetterType[This]
  		def this(g:This=>SymmetricFunction) = this(null.asInstanceOf[This], g)
  		override def set(newValue:This)(implicit d:DiffList) = {
  			if (value != null) get(value)._set(null.asInstanceOf[This]) // Why is this cast necessary?
  			super.set(newValue)(d)
        if (newValue != null) get(newValue)._set(thisEntity)
  		}
  		protected def _set(newValue:This)(implicit d:DiffList) = super.set(newValue)(d)
  	}
  	class Relation[B](val bwd:B=>Relation[This]) extends SetVariable[B] {
  		protected val back = new HashSet[B] // those b:B's who have relation (b,this); note that this may not have the relation (this,b)
  		def incoming: Iterable[B] = back
  		override def add(b:B)(implicit d:DiffList): Unit = if (!this.contains(b)) { bwd(b).back += thisEntity; super.add(b)(d); if (d != null) d += RelationAddDiff(b) }
  		override def remove(b:B)(implicit d:DiffList): Unit = if (this.contains(b)) { bwd(b).back -= thisEntity; super.remove(b)(d); if (d != null) d += RelationRemoveDiff(b) }
  		case class RelationAddDiff(b:B) extends Diff { def variable = Relation.this; def redo = bwd(b).back += thisEntity; def undo = bwd(b).back -= thisEntity }
  		case class RelationRemoveDiff(b:B) extends Diff { def variable = Relation.this; def redo = bwd(b).back -= thisEntity; def undo = bwd(b).back += thisEntity }
    }
  }

  
  // Getters for bi-directional relations
  
  /** A class that provides forward and reverse mappings through relations, 
      enabling creation of nested mappings by what look like normal one-way access method calls.
      For examples of its usage, see example/LogicDemo*.scala.
      Typically, post-construction, callers would immediately change the prefix, forward1m (or forward1s) and reverse1m (or reverse1s).  
      You can make the "unit" (initial) element for a chain by constructing FooGetter[Foo,Foo],
      and leaving all the above vars unchanged. */
  // TODO I want trait Getter[C1<:GetterType[C1]], but fighting with typing in ScoreNeighor0 below 
  trait Getter[C1] {
    type A
    type B //<: GetterType[B]
    type C = C1
    protected var prefix: Getter[B] = null
    protected var forward1s: B=>C = (b:B) => b.asInstanceOf[C] // Default no-op for "unit" Getter
    protected var reverse1s: C=>B = (c:C) => c.asInstanceOf[B] // Default no-op for "unit" Getter
    protected var forward1m: B=>Iterable[C] = null
    protected var reverse1m: C=>Iterable[B] = null
    private lazy val _forward: A=>Iterable[C] = { // the ways of combining prefix with different forward1* to append a link to the chain
      if (prefix == null) {
        if (forward1m == null)
          (a:A) => List(forward1s(a.asInstanceOf[B]))
        else
          (a:A) => forward1m(a.asInstanceOf[B])
      } else {
      	if (forward1m == null)
      		(a:A) => prefix.unsafeForward(a).map(forward1s).filter(e => e != null) // filter a null from forward1s
      	else
          (a:A) => prefix.unsafeForward(a).flatMap(forward1m)
      }
    }
    private lazy val _reverse: C=>Iterable[A] = { // the ways of combining prefix with different reverse1* to prepend a link to the chain
      if (prefix == null) {
        if (reverse1m == null)
          (c:C) => List(reverse1s(c)).asInstanceOf[Iterable[A]]
        else
          (c:C) => reverse1m(c).asInstanceOf[Iterable[A]]
      } else {
        if (reverse1m == null)
          (c:C) => { val c2 = reverse1s(c); if (c2 == null) Nil else prefix.unsafeReverse[A](c2) } // filter a null from reverse1s
        else
          (c:C) => reverse1m(c).flatMap(prefix.unsafeReverseFunc)
      }
    }
    def forward: A=>Iterable[C] = _forward
    def reverse: C=>Iterable[A] = _reverse
    // Ugly non-type-safe versions of some methods.  I think that with Scala 2.8 path-dependent types these will no longer be necessary
    def unsafeForward(a:Any): Iterable[C] = _forward(a.asInstanceOf[A])
    def unsafeReverse[A8](c:Any): Iterable[A8] = _reverse(c.asInstanceOf[C]).asInstanceOf[Iterable[A8]]
    def unsafeReverseFunc[B8,C8]: B8=>Iterable[C8] = _reverse.asInstanceOf[B8=>Iterable[C8]]
    /** Create a new Getter, starting from this one and appending an additional many-to-many mapping. */
    def getManyToMany[D<:GetterType[D]](fwd1:C=>Iterable[D], rev1:D=>Iterable[C])(implicit m:Manifest[D#GetterClass]): D#GetterClass with GetterHead[A,D] with GetterMiddle[C,D] = {
      val ret = newGetter[D](m).asInstanceOf[D#GetterClass with GetterHead[A,D] with GetterMiddle[C,D]]
      ret.prefix = Getter.this.asInstanceOf[Getter[C] with GetterHead[A,C]] // I'm confused as to why this is working because I don't think EntityGetters have GetterHead mixed in!
      ret.forward1m = fwd1
      ret.reverse1m = rev1
      ret
    }
    /** Create a new Getter, starting from this one and appending an additional many-to-one mapping. */
    def getManyToOne[D<:GetterType[D]](fwd1:C=>D, rev1:D=>Iterable[C])(implicit m:Manifest[D#GetterClass]): D#GetterClass with GetterHead[A,D] with GetterMiddle[C,D] = {
      val ret = newGetter[D](m).asInstanceOf[D#GetterClass with GetterHead[A,D] with GetterMiddle[C,D]]
      ret.prefix = Getter.this.asInstanceOf[Getter[C] with GetterHead[A,C]]
      ret.forward1s = fwd1
      ret.reverse1m = rev1
      ret
    } 
    /** Create a new Getter, starting from this one and appending an additional one-to-one mapping. */
    def getOneToOne[D<:GetterType[D]](fwd1:C=>D, rev1:D=>C)(implicit m:Manifest[D#GetterClass]): D#GetterClass with GetterHead[A,D] with GetterMiddle[C,D] = {
      val ret = newGetter[D](m).asInstanceOf[D#GetterClass with GetterHead[A,D] with GetterMiddle[C,D]]
      ret.prefix = Getter.this.asInstanceOf[Getter[C] with GetterHead[A,C]]
      ret.forward1s = fwd1
      ret.reverse1s = rev1
      ret
    } 
    /** Create a new Getter, starting from this one and appending an additional one-to-many mapping. */
    def getOneToMany[D<:GetterType[D]](fwd1:C=>Iterable[D], rev1:D=>C)(implicit m:Manifest[D#GetterClass]): D#GetterClass with GetterHead[A,D] with GetterMiddle[C,D] = {
      val ret = newGetter[D](m).asInstanceOf[D#GetterClass with GetterHead[A,D] with GetterMiddle[C,D]]
      ret.prefix = Getter.this.asInstanceOf[Getter[C] with GetterHead[A,C]]
      ret.forward1m = fwd1
      ret.reverse1s = rev1
      ret
    }
    // TODO If I uncomment "C with" below, I get scalac error: "illegal type selection from volatile type D".  It seems I should be able to do this, though.
    /** Create a new Getter, starting from this one and appending an additional symmetric many-to-many mapping.
        For example:  getSymmetricManyToMany[Person](p => p.mother.children.filter(p2=>p2 ne p)). */
    def getSymmetricManyToMany[D<: /*C with*/ GetterType[D]](fwd1:C=>Iterable[D])(implicit m:Manifest[D#GetterClass]): D#GetterClass with GetterHead[A,D] with GetterMiddle[C,D] = 
    	getManyToMany[D](fwd1, fwd1.asInstanceOf[D=>Iterable[C]])(m) // TODO this cast is somewhat unsafe.  Would be better to type-check the fwd1 argument
    /** Create a new Getter, starting from this one and appending an additional symmetric one-to-one mapping. 
        For example:  getSymmetricOneToOne[Person](_.spouse)*/
    def getSymmetricOneToOne[D<: /*C with*/ GetterType[D]](fwd1:C=>D)(implicit m:Manifest[D#GetterClass]): D#GetterClass with GetterHead[A,D] with GetterMiddle[C,D] = 
      getOneToOne[D](fwd1, fwd1.asInstanceOf[D=>C])(m) // TODO this cast is somewhat unsafe.  Would be better to type-check the fwd1 argument
    /** Create a new Getter, starting from this one and appending a mapping to one of its Attributes. */
    // TODO Consider [D<:Entity[C]#Attribute] instead
    def getAttribute2[D<:AttributeOf[C] with GetterType[D]](fwd1:C=>D)(implicit m:Manifest[D#GetterClass]): Getter[D] with GetterHead[A,D] with GetterMiddle[C,D] = {
      val ret = newGetter[D](m).asInstanceOf[D#GetterClass with GetterHead[A,D] with GetterMiddle[C,D]] //.asInstanceOf[]
      //val ret = new Getter[D] with GetterHead[A,D] with GetterMiddle[C,D] //.asInstanceOf[]
      //ret.prefix = Getter.this.asInstanceOf[Getter[C] with GetterHead[A,C]] // TODO Doesn't run because Getter doesn't have GetterHead
      type ThisA = A
      ret.prefix = Getter.this.asInstanceOf[Getter[C] { type A = ThisA }]
      ret.forward1s = (c:C) => fwd1(c)
      ret.reverse1s = (d:D) => d.attributeOwner
      ret
    }
     def getAttribute[D<:AttributeOf[C]](fwd1:C=>D)(implicit m:Manifest[D]): Getter[D] with GetterHead[A,D] with GetterMiddle[C,D] = {
      val ret = new Getter[D] with GetterHead[A,D] with GetterMiddle[C,D] //.asInstanceOf[]
      //ret.prefix = Getter.this.asInstanceOf[Getter[C] with GetterHead[A,C]] // TODO Doesn't run because Getter doesn't have GetterHead
      type ThisA = A
      ret.prefix = Getter.this.asInstanceOf[Getter[C] { type A = ThisA }]
      ret.forward1s = (c:C) => fwd1(c)
      ret.reverse1s = (d:D) => d.attributeOwner
      ret
    }
    /** Create a new Getter, starting from this one and appending a symmetric function.  
        This differs from getSymmetricOneToOne in that it represents a mutable one-to-one relation, whereas getSymmetricOneToOne represents an immutable relation. */
    //def getSymmetricFunction[D<:Entity[C]#SymmetricFunction](fwd1:C=>D)(implicit m:Manifest[D#EntityType#GetterClass]): Getter[C] with GetterHead[A,C] with GetterMiddle[C,C] 
    // TODO I really want to say that D == C here.  I thought the above solution would work, but I get error "EntityType" does not have member "GetterClass", when it clearly does! 
    def getSymmetricFunction[D<:GetterType[D]](fwd1:C=>RefVariable[D])(implicit m:Manifest[D#GetterClass]): Getter[D] with GetterHead[A,D] with GetterMiddle[C,D] = {
      val ret = newGetter[D](m).asInstanceOf[D#GetterClass with GetterHead[A,D] with GetterMiddle[C,D]]
      ret.prefix = Getter.this.asInstanceOf[Getter[C] with GetterHead[A,C]]
      ret.forward1s = (c:C) => fwd1(c).value.asInstanceOf[D]
      ret.reverse1s = (d:D) => fwd1(d.asInstanceOf[C]).value.asInstanceOf[C]
      ret
    }
    def getRelation[D<:GetterType[D]](fwd1:C=>Entity[C]#Relation[D], bwd1:D=>Entity[D]#Relation[C])(implicit m:Manifest[D#GetterClass]): Getter[D] with GetterHead[A,D] with GetterMiddle[C,D] = {
      val ret = newGetter[D](m).asInstanceOf[D#GetterClass with GetterHead[A,D] with GetterMiddle[C,D]]
      ret.prefix = Getter.this.asInstanceOf[Getter[C] with GetterHead[A,C]]
      ret.forward1m = (c:C) => fwd1(c).members //.asInstanceOf[Iterable[D]]
      ret.reverse1m = (d:D) => bwd1(d).incoming // asInstanceOf[C]
      ret
    } 
  }
  /** Fill in abstract type Getter.A with parameterized type.  Necessary for Scala type-inferencer. */
  trait GetterHead[A1,C1/*<:GetterType[C1]*/] extends Getter[C1] {
    type A = A1
  }
  /** Fill in abstract type Getter.B with parameterized type.  Necessary for Scala type-inferencer. */
  trait GetterMiddle[B1/*<:GetterType[B1]*/,C1/*<:GetterType[C1]*/] extends Getter[C1] {
    type B = B1
  }
  /** Typical Getter trait inherited by users, and thus D#GetterClass is often a sub-class of this. */
  trait EntityGetter[A<:Entity[A]] extends Getter[A] //with GetterHead[AnyRef,A]
  trait AttributeGetter[A<:AttributeOf[E] with GetterType[A],E] extends Getter[A] with GetterType[A] {
    //def attributeOwner: E = getOneToOne[E]((a:A)=>a.attributeOwner, ??)
  }
  // TODO? Consider avoiding the need to mix this into Entity by using instead duck typing: type WithGetterType = { type GetterClass <: Getter[_,_] }
  trait GetterType[D] {
    type GetterClass <: Getter[D] // We don't want this to specify the GetterHead because Getter.get* methods must be able to fill that in themselves
  }
  /** Construct a new Getter with tail type A. */
  def newGetter[A<:GetterType[A]](implicit m:Manifest[A#GetterClass]): A#GetterClass = {
  	val constructors = m.erasure.getConstructors
    if (constructors.size != 1) throw new Error("Getters must have only one constructor")
    val constructor = m.erasure.getConstructors.apply(0)
    val numArgs = constructor.getParameterTypes.length
    if (numArgs != 0) throw new Error("Getter constructors must not take any arguments.")
    constructor.newInstance().asInstanceOf[A#GetterClass]
  }
  /** Construct a new Getter representing the beginning of an getter chain, taking input A. */
  def newGetterUnit[A<:GetterType[A]](implicit m:Manifest[A#GetterClass]): A#GetterClass with GetterHead[A,A] = {
  	//println("GetterUnit m="+m)
  	newGetter[A](m).asInstanceOf[A#GetterClass with GetterHead[A,A]];
  }
  
  
  // Define function for scoring compatibility between getter targets with CategoricalValues
  // Example usage:  Forany[Token] { t => Score(t, t.label) }
  type ScorableValues[X] = CategoricalValues //with GetterType[X]
  type ScorableValues0 = CategoricalValues //with GetterType[CategoricalValues]
  
	case class Score[X<:Variable](sns:ScoreNeighbor0[X]*) {
  	def manifests : Seq[Manifest[_<:Variable]] = sns.flatMap(_.manifests)
  	def getters : Seq[GetterHead[X,ScorableValues0]] = sns.flatMap(_.getters)
  }
  trait ScoreNeighbor0[X<:Variable] {
  	def manifests : Iterable[Manifest[ScorableValues0]];
  	def getters : Seq[GetterHead[X,ScorableValues0]]
  }
  class ScoreNeighbor[X<:Variable,A<:ScorableValues[A]](a1:GetterHead[X,A])(implicit ma:Manifest[A]) extends ScoreNeighbor0[X] {
  	def manifests = List(ma.asInstanceOf[Manifest[ScorableValues0]])
  	def getters = List(a1.asInstanceOf[GetterHead[X,ScorableValues0]])
  }
  
  implicit def getter2scoreneighbor[X<:Variable,A<:ScorableValues[A]](a:Getter[A] with GetterHead[X,A])(implicit ma:Manifest[A]): ScoreNeighbor0[X] = 
    new ScoreNeighbor(a)(ma)
  
  
  // Define functions for clauses in first-order logic
  // Example usage: Forany[Person] { p => p.smokes ==> p.cancer }
  type FormulaValue0 = BooleanValue //with GetterType[BooleanValue]
  type FormulaValue[A] = BooleanValue //with GetterType[A]
    
  trait Formula[X<:Variable] {
    def eval(x:ArrayStack[FormulaValue0]) : Boolean
    def manifests : Seq[Manifest[_<:Variable]]
    def getters : Seq[GetterHead[X,FormulaValue0]]
    def ==>(f:Formula[X]) = Implies(this, f)
    def ^(f:Formula[X]) = And(this, f)
    def v(f:Formula[X]) = Or(this, f)
    def !: = Not(this)
    def <==>(f:Formula[X]) = BooleanEquals(this, f)
  }
  case class Term[X<:Variable,A<:FormulaValue[A]](g1:GetterHead[X,A])(implicit ma:Manifest[A]) extends Formula[X] {
    def eval(x:ArrayStack[FormulaValue0]) = x.pop.value
    def manifests = List(ma)
    def getters = List(g1.asInstanceOf[GetterHead[X,FormulaValue0]])
  }

  implicit def getter2formula[X<:Variable,A<:BooleanValue/* with GetterType[A]*/](g:Getter[A] with GetterHead[X,A])(implicit ma:Manifest[A]): Formula[X] = new Term(g)(ma)
  
  abstract class Formula1[X<:Variable](c1:Formula[X]) extends Formula[X] {
    def manifests = c1.manifests
    def getters = c1.getters
  }
  case class Not[X<:Variable](f1:Formula[X]) extends Formula1(f1) {
    def eval(x:ArrayStack[FormulaValue0]) = ! f1.eval(x)
  }
  case class True[X<:Variable](f1:Formula[X]) extends Formula1(f1) { // noop, but forces implicit conversion to Term
    def eval(x:ArrayStack[FormulaValue0]) = f1.eval(x)
  }
  abstract class Formula2[X<:Variable](c1:Formula[X], c2:Formula[X]) extends Formula[X] {
    def manifests = c1.manifests ++ c2.manifests
    def getters = c1.getters ++ c2.getters
  }
  case class And[X<:Variable](c1:Formula[X], c2:Formula[X]) extends Formula2[X](c1,c2) {
    def eval(x:ArrayStack[FormulaValue0]) = c1.eval(x) && c2.eval(x) 
  }
  case class Or[X<:Variable](c1:Formula[X], c2:Formula[X]) extends Formula2[X](c1,c2) {
    def eval(x:ArrayStack[FormulaValue0]) = c1.eval(x) || c2.eval(x) 
  }
  case class Implies[X<:Variable](c1:Formula[X], c2:Formula[X]) extends Formula2[X](c1,c2) {
    def eval(x:ArrayStack[FormulaValue0]) = (! c1.eval(x)) || c2.eval(x)
  }
  case class BooleanEquals[X<:Variable](c1:Formula[X], c2:Formula[X]) extends Formula2[X](c1,c2) {
    def eval(x:ArrayStack[FormulaValue0]) = c1.eval(x) == c2.eval(x)
  }
  case class Forall[X<:Variable](f1:Formula[X]) extends Formula1(f1) {
    def eval(x:ArrayStack[FormulaValue0]) = throw new Error("Not yet implemented") // Need to think carefully about this
  }
  case class Forsome[X<:Variable](f1:Formula[X]) extends Formula1(f1) {
    def eval(x:ArrayStack[FormulaValue0]) = throw new Error("Not yet implemented") // Need to think carefully about this
  }

  trait LogicStatistics extends DotStatistics1[BooleanValue] {
    def *(w:Double) : this.type = { this.weights(0) = 0.0; this.weights(1) = Math.log(w); this }
  }
  
  
  // Application of Score and logic primitives starting with an Getter
  object Forany {
    def apply[X<:GetterType[X] with Variable](x2c:X#GetterClass with GetterHead[X,X]=>Formula[X])(implicit m:Manifest[X#GetterClass]): Template with LogicStatistics = {
      type I = FormulaValue0
      val getterRoot: X#GetterClass with GetterHead[X,X] = newGetterUnit[X](m)
      val formula = x2c(getterRoot)
      val manifests = formula.manifests.asInstanceOf[Seq[Manifest[I]]];
      //assert(formula.getters.length == 1, formula.getters.length)
      val getters = formula.getters
      val size = manifests.length
      size match {
        case 1 => new Template1[I]()(manifests(0)) with LogicStatistics {
          override def unroll1(n1:I) = { val roots = getters(0).reverse(n1); if (!roots.isEmpty) Factor(n1) else Nil }
          def statistics(n1:I) = { val s = new ArrayStack[I]; s+=n1; Stat(Bool(formula.eval(s))) }
        }.init
        case 2 => new Template2[I,I]()(manifests(0), manifests(1)) with LogicStatistics {
          def unroll1(n1:I) = { val roots = getters(0).reverse(n1); for (root <- roots; n2 <- getters(1).forward(root)) yield Factor(n1,n2) }
          def unroll2(n2:I) = { val roots = getters(1).reverse(n2); for (root <- roots; n1 <- getters(0).forward(root)) yield Factor(n1,n2) }
          def statistics(n1:I, n2:I) = { val s = new ArrayStack[I]; s+=n2; s+=n1; Stat(Bool(formula.eval(s))) }
        }.init
        case 3 => new Template3[I,I,I]()(manifests(0), manifests(1), manifests(2)) with LogicStatistics {
          def unroll1(n1:I) = { val roots = getters(0).reverse(n1); for (root <- roots; n2 <- getters(1).forward(root); n3 <- getters(2).forward(root)) yield Factor(n1,n2,n3) } 
          def unroll2(n2:I) = { val roots = getters(1).reverse(n2); for (root <- roots; n1 <- getters(0).forward(root); n3 <- getters(2).forward(root)) yield Factor(n1,n2,n3) } 
          def unroll3(n3:I) = { val roots = getters(2).reverse(n3); for (root <- roots; n1 <- getters(0).forward(root); n2 <- getters(1).forward(root)) yield Factor(n1,n2,n3) } 
          def statistics(n1:I, n2:I, n3:I) = { val s = new ArrayStack[I]; s+=n3; s+=n2; s+=n1; Stat(Bool(formula.eval(s))) }
        }.init
        case 4 => new Template4[I,I,I,I]()(manifests(0), manifests(1), manifests(2), manifests(3)) with LogicStatistics {
          def unroll1(n1:I) = { val roots = getters(0).reverse(n1); for (root <- roots; n2 <- getters(1).forward(root); n3 <- getters(2).forward(root); n4 <- getters(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
          def unroll2(n2:I) = { val roots = getters(1).reverse(n2); for (root <- roots; n1 <- getters(0).forward(root); n3 <- getters(2).forward(root); n4 <- getters(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
          def unroll3(n3:I) = { val roots = getters(2).reverse(n3); for (root <- roots; n1 <- getters(0).forward(root); n2 <- getters(1).forward(root); n4 <- getters(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
          def unroll4(n4:I) = { val roots = getters(3).reverse(n4); for (root <- roots; n1 <- getters(0).forward(root); n2 <- getters(1).forward(root); n3 <- getters(2).forward(root)) yield Factor(n1,n2,n3,n4) } 
          def statistics(n1:I, n2:I, n3:I, n4:I) = { val s = new ArrayStack[I]; s+=n4; s+=n3; s+=n2; s+=n1; Stat(Bool(formula.eval(s))) }
        }.init
      }
    }
  }
  object For {
    def apply[X<:GetterType[X] with Variable](x2c:X#GetterClass with GetterHead[X,X]=>Score[X])(implicit m:Manifest[X#GetterClass]) = {
    	val score = x2c(newGetterUnit[X](m))
    	val manifests = score.manifests.toList.asInstanceOf[List[Manifest[ScorableValues0]]];
    	val getters = score.getters
    	val size = manifests.length
    	type I = ScorableValues0
    	size match {
    		case 1 => new TemplateWithDotStatistics1[I]()(manifests(0)) {
    			override def unroll1(n1:I) = { val roots = getters(0).reverse(n1); if (!roots.isEmpty) Factor(n1) else Nil }
    		}
    		case 2 => new TemplateWithDotStatistics2[I,I]()(manifests(0), manifests(1)) {
    			def unroll1(n1:I) = { val roots = getters(0).reverse(n1); for (root <- roots; n2 <- getters(1).forward(root)) yield Factor(n1,n2) }
    			def unroll2(n2:I) = { val roots = getters(1).reverse(n2); for (root <- roots; n1 <- getters(0).forward(root)) yield Factor(n1,n2) }
    		}
    		case 3 => new TemplateWithDotStatistics3[I,I,I]()(manifests(0), manifests(1), manifests(2)) {
    			def unroll1(n1:I) = { val roots = getters(0).reverse(n1); for (root <- roots; n2 <- getters(1).forward(root); n3 <- getters(2).forward(root)) yield Factor(n1,n2,n3) } 
    			def unroll2(n2:I) = { val roots = getters(1).reverse(n2); for (root <- roots; n1 <- getters(0).forward(root); n3 <- getters(2).forward(root)) yield Factor(n1,n2,n3) } 
    			def unroll3(n3:I) = { val roots = getters(2).reverse(n3); for (root <- roots; n1 <- getters(0).forward(root); n2 <- getters(1).forward(root)) yield Factor(n1,n2,n3) } 
    		}
    		case 4 => new TemplateWithDotStatistics4[I,I,I,I]()(manifests(0), manifests(1), manifests(2), manifests(3)) {
    			def unroll1(n1:I) = { val roots = getters(0).reverse(n1); for (root <- roots; n2 <- getters(1).forward(root); n3 <- getters(2).forward(root); n4 <- getters(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
    			def unroll2(n2:I) = { val roots = getters(1).reverse(n2); for (root <- roots; n1 <- getters(0).forward(root); n3 <- getters(2).forward(root); n4 <- getters(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
    			def unroll3(n3:I) = { val roots = getters(2).reverse(n3); for (root <- roots; n1 <- getters(0).forward(root); n2 <- getters(1).forward(root); n4 <- getters(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
    			def unroll4(n4:I) = { val roots = getters(3).reverse(n4); for (root <- roots; n1 <- getters(0).forward(root); n2 <- getters(1).forward(root); n3 <- getters(2).forward(root)) yield Factor(n1,n2,n3,n4) } 
    		}
    	}
    }
  }

  

}

