/* Copyright (C) 2008-2009 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0.
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.{ArrayStack,HashSet,HashMap,ListBuffer}
import cc.factorie._

// I'd like to create a cc.factorie.er package and spread these class definitions across multiple files
// but I need Scala 2.8 package objects for the implicit conversions.
  
// TODO There are a disturbing number of casts in this file.  
// It could benefit from a serious sweep to get rid of as many of these as possible. 

/** Classes and functions supporting Entity-Relationship languages for creating Templates. 
    @author Andrew McCallum
    @since 0.8
*/
object er {
  
  type VariableWithGetter[D] = Variable { type GetterType <: Getter[D] }
  type HasGetterType[D] = { type GetterType <: Getter[D] }

  // Define attributes and entities

  /** A generic Attribute */
  trait AttributeOf[E] extends Variable {
    /** The entity that is described by this attribute. */
    def attributeOwner: E
    /** Print the owner of the attribute before the rest of its toString representation. */
    override def toString = attributeOwner.toString+":"+super.toString
  }
  // I considered changing this trait name because of concerns that it is too generic.  For example it might be desired for coref.
  // But now I think it is good for the "entity-relationship" package.  Users can always specify it with "er.Entity", which isn't bad.
  // TODO Note that it is hard to subclass one of these, which seems sad.  
  //  For example, users might want to subclass the pre-packaged entities in cc.factorie.application.  Think about this some more.
  /** A trait for entities that have attributes.  Provides an inner trait 'Attribute' for its attribute classes. */
  trait Entity[This<:Entity[This] with Variable] extends Variable {
    this: This =>
    //type VariableType = This
    type EntityType = This
    def thisEntity: This = this
    /** Sub-trait of cc.factorie.er.AttributeOf that has a concrete implementation of 'attributeOwner'. */
    trait Attribute extends cc.factorie.er.AttributeOf[This] {
      //type VariableType <: Attribute
      class GetterClass extends AttributeGetter[Attribute,This]
      def attributeOwner: This = thisEntity
    }
    /** Consider removing this.  Not sure if it should go here or outside the Entity. */
    class SymmetricFunction(initval:This, val get:This=>SymmetricFunction) extends RefVariable(initval) {
      type EntityType = This //with GetterType[This]
      def this(g:This=>SymmetricFunction) = this(null.asInstanceOf[This], g)
      override def set(newValue:This)(implicit d:DiffList) = {
        if (value != null) get(value)._set(null.asInstanceOf[This]) // Why is this cast necessary?
        super.set(newValue)(d)
        if (newValue != null) get(newValue)._set(thisEntity)
      }
      protected def _set(newValue:This)(implicit d:DiffList) = super.set(newValue)(d)
    }
  }

  /** Representing a directed relationship from <code>src</code> to <code>dst</code>.  
      Its boolean value indicates whether the relationship is actually present or not. 
      A Relationship is also an "Entity" so that Relationships can have Attributes. 
      Upon custruction the initial value of this variable must be false so that when Relation.add 
      sets its value to true, the change gets put on the DiffList. */
  // TODO Is this last sentence a bad idea?  I could avoid it by spliting 'AttributeHolding' out of Entity.
  // Note that the current state allows there to be "Relations among Relationships"... hmmm!  Might this be useful?
  class Relationship[A<:Entity[A],B<:Entity[B]](val src:A, val dst:B) extends BoolVariable with Entity[Relationship[A,B]] {
    type GetterType <: RelationshipGetter[A,B]
    class GetterClass extends RelationshipGetter[A,B]
    override def toString = printName+"("+src+","+dst+","+value+")" // TODO For some reason this is having no effect, so I re-override below
  }
  
  /** Represents a many-to-many relation.
      Example usage:  object friend extends Relation[Person,Person] */
  // TODO Only binary relations for now, but it wouldn't be hard to add n-ary relations.
  class Relation[A<:Entity[A],B<:Entity[B]] extends Variable {
    type SrcType = A
    type DstType = B
    private val a2rs = new HashMap[A,ListBuffer[RelationshipType]] // a to collection of Relationships
    private val b2rs = new HashMap[B,ListBuffer[RelationshipType]] // b to collection of Relationships
    private val ab2r = new HashMap[(A,B),RelationshipType] // (a,b) to the corresponding Relationship
    class Relationship(src:A, dst:B) extends cc.factorie.er.Relationship(src,dst) {
      //type GetterClass = RelationshipGetter[A,B];
      def relation = Relation.this
      override def printName = Relation.this.printName
      override def toString = printName+"("+src+","+dst+","+value+")"
    }
    type RelationshipType = Relationship
    def newRelationship(a:A, b:B) : RelationshipType =  new Relationship(a,b)
    protected def addRelationship(r:RelationshipType) = {
      val pair = (r.src, r.dst)
      assert(!ab2r.contains(pair))
      //println("Relationship add "+r.src+" "+r.dst)
      a2rs.getOrElseUpdate(r.src, new ListBuffer[RelationshipType]) += r
      b2rs.getOrElseUpdate(r.dst, new ListBuffer[RelationshipType]) += r
      ab2r(pair) = r
    }
    protected def removeRelationship(r:RelationshipType) = {
      a2rs(r.src) -= r
      b2rs(r.dst) -= r
      ab2r -= ((r.src,r.dst))
    }
    protected def addEntry(pair:(A,B)) : RelationshipType = { val r = newRelationship(pair._1, pair._2); addRelationship(r); r }
    protected def removeEntry(pair:(A,B)) : RelationshipType = { val r = ab2r(pair); removeRelationship(r); r }
    /** Add a new Relationship between a and b, but leave its value as false. */
    protected def add(a:A, b:B, value:Boolean)(implicit d:DiffList): RelationshipType = { val r = addEntry((a,b)); r.set(value); if (d != null) d += RelationAddDiff(r); r }
    /** Add a new Relationship between a and b, and set its value to true. */
    def add(a:A, b:B)(implicit d:DiffList): RelationshipType = add(a,b,true)
    /** Remove a Relationship between a and b, and set its value to false.  
        Note that the Relationship variable does not actually get removed from this Relation, 
        it is merely set to false, which causes it to be skipped in various queries. 
        This supports that ability to specify a Relationship variable as the target of inference. */
    def remove(a:A, b:B)(implicit d:DiffList):RelationshipType = { val r = removeEntry((a,b)); r.set(false); if (d != null) d += RelationRemoveDiff(r); r }
    // TODO If we are ever tempted to prune our tables by actually removing false Relationships, (which is a good idea)
    // be very careful to create a mechanism to preserve attributes of the removed relationships, or alternatively make the user
    // aware that relationship attributes will not be preserved once purged, and note that the DiffList must handle this case correctly.
    def size = ab2r.size
    def elements = ab2r.values.filter(_.value)
    def asCollection = new Collection[RelationshipType] {
      def size = Relation.this.size
      def elements = Relation.this.elements
    }
    /** Return the Relationship between a and b, creating as necessary.  If it is created, its initial value will be false. */
    def get(a:A,b:B): RelationshipType = ab2r.getOrElse((a,b), addEntry((a,b))) // TODO verify in the newly-created case that r.value is false
    def getFromSrc(a:A): Iterable[RelationshipType] = a2rs.getOrElse(a, Nil).filter(r => r.value)
    def getFromDst(b:B): Iterable[RelationshipType] = b2rs.getOrElse(b, Nil).filter(r => r.value)
    //def forward: A=>Iterable[B] = (a:A) => get2(a).map(_.dst)
    //def reverse: B=>Iterable[A] = (b:B) => get1(b).map(_.src)
    /** Set the value of the relationship from a to b to true.  If not already present, create it. */
    def apply(a:A,b:B): RelationshipType = { val r = ab2r.getOrElse((a,b), add(a,b)(null)); r := true; r }
    //def apply(g1:Getter[_,A],g2:Getter[_,B]) : Formula = new ... 
    def +=(pair:(A,B)) = add(pair._1, pair._2)(null)
    def ++=(abs:Iterable[(A,B)]) = abs.foreach(pair => +=(pair))
    def -=(pair:(A,B)) = remove(pair._1, pair._2)(null)
    def --=(abs:Iterable[(A,B)]) = abs.foreach(pair => -=(pair))
    def contains(a:A,b:B) = ab2r.contains((a,b))
    protected def stringPrefix ="Relation"
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
  class ItemizedRelation[A<:ItemizedObservation[A] with Entity[A],B<:ItemizedObservation[B] with Entity[B]](implicit ma:Manifest[A], mb:Manifest[B]) extends Relation[A,B] with Variable with IterableSettings {
    def settings = new SettingIterator {
      var i = -1
      val domainb = Domain[B](mb) 
      val a = Domain[A](ma).randomValue // randomly pick a src
      val max = domainb.size - 1 // we will iterate over all possible changes to dst's
      def hasNext = i < max
      private def set(d:DiffList) : Unit = { val b = domainb.get(i); if (contains(a,b)) remove(a,b)(d) else add(a,b)(d) }
      def next(difflist:DiffList) = { i += 1; val d = newDiffList; set(d); d }
      def reset = i = -1
    }
  }

  /*class SymmetricFunction[A>:Null<:Entity[A]] /*extends GetterType[A]*/ {
    //type GetterClass = SymmetricFunctionGetter[A]
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
  }*/


  
  // Getters for bi-directional relations
  
  /** A class that provides forward and reverse mappings through relations, 
      enabling creation of nested mappings by what look like normal one-way access method calls.
      For examples of its usage, see example/LogicDemo*.scala.
      Typically, post-construction, callers would immediately change the prefix, forward1m (or forward1s) and reverse1m (or reverse1s).  
      You can make the "unit" (initial) element for a chain by constructing FooGetter[Foo,Foo],
      and leaving all the above vars unchanged.
      A Getter may be declared as an inner class, in which case its constructor takes a hidden argument which is a pointer to the outer instance.
      However, in that case the Getter will be constructed with a null argument.  
      It is up to you not to use the outer instance in your implementation of Getter subclasses. */
  // TODO I want trait Getter[C1<:GetterType[C1]], but fighting with typing in ScoreNeighor0 below 
  trait Getter[C1/*<:HasGetterType[C1]*/] {
    type A
    type B
    type C = C1
    type CG = C1 { type GetterType <: Getter[C1] }
    private var prefix: Getter[B] = null
    def getPrefix: Getter[B] = prefix // TODO fix this non-standard naming
    private var forward1s: B=>C = (b:B) => b.asInstanceOf[C] // Default no-op for "unit" Getter
    private var reverse1s: C=>B = (c:C) => c.asInstanceOf[B] // Default no-op for "unit" Getter
    private var forward1m: B=>Iterable[C] = null
    private var reverse1m: C=>Iterable[B] = null
    // Sometimes the neighbors of a Template that results from a Getter must include not only the final C, 
    // but also some intermediate Variables.  The next three lines provide a place to put that information  
    type ExtraNeighborType = BooleanValue // really Relationship[_,C]
    var extraManifest: Manifest[ExtraNeighborType] = null
    var extraGetter: Getter[ExtraNeighborType] = null
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
    def getManyToMany[D<:{type GetterType<:Getter[D]}](fwd1:C=>Iterable[D], rev1:D=>Iterable[C])(implicit m:Manifest[D]): D#GetterType { type A = Getter.this.A; type B = Getter.this.C } = {
      initManyToMany[D](
        newGetter[D](m), 
        fwd1, 
        rev1)
    }
    def initManyToMany[D<:{type GetterType<:Getter[D]}](getter:Getter[D]/*D#GetterType*/, fwd1:C=>Iterable[D], rev1:D=>Iterable[C]): D#GetterType { type A = Getter.this.A; type B = Getter.this.C } = {
      type ThisA = A
      type ThisC = C
      val ret = getter.asInstanceOf[Getter[D] { type A = Getter.this.A; type B = Getter.this.C }]
      ret.prefix = Getter.this.asInstanceOf[Getter[C] {type A = ThisA}]
      ret.forward1m = fwd1
      ret.reverse1m = rev1
      //ret.asInstanceOf[D#GetterType]
      getter.asInstanceOf[D#GetterType { type A = Getter.this.A; type B = Getter.this.C }]
    }
    /** Create a new Getter, starting from this one and appending an additional many-to-one mapping. */
    def getManyToOne[D<:{type GetterType<:Getter[D]}](fwd1:C=>D, rev1:D=>Iterable[C])(implicit m:Manifest[D]): D#GetterType { type A = Getter.this.A; type B = Getter.this.C } = {
      initManyToOne[D](newGetter[D](m), fwd1, rev1)
    } 
    def initManyToOne[D<:{type GetterType<:Getter[D]}](getter:Getter[D]/*D#GetterType*/, fwd1:C=>D, rev1:D=>Iterable[C]): D#GetterType { type A = Getter.this.A; type B = Getter.this.C } = {
      type ThisA = A
      type ThisC = C
      val ret = getter.asInstanceOf[D#GetterType { type A = ThisA; type B = ThisC }]
      ret.prefix = Getter.this.asInstanceOf[Getter[C] {type A = ThisA} ]
      ret.forward1s = fwd1
      ret.reverse1m = rev1
      ret
    }
    /** Create a new Getter, starting from this one and appending an additional one-to-one mapping. */
    def getOneToOne[D<:{type GetterType<:Getter[D]}](fwd1:C=>D, rev1:D=>C)(implicit m:Manifest[D]): D#GetterType { type A = Getter.this.A; type B = Getter.this.C } = {
      initOneToOne[D](newGetter[D](m), fwd1, rev1)
    } 
    def initOneToOne[D<:{type GetterType<:Getter[D]}/*,DG<:D#GetterType*/](getter:Getter[D]/*D#GetterType*/, fwd1:C=>D, rev1:D=>C): D#GetterType { type A = Getter.this.A; type B = Getter.this.C } = {
      type ThisA = A
      type ThisC = C
      val ret = getter.asInstanceOf[D#GetterType { type A = ThisA; type B = ThisC }]
      ret.prefix = Getter.this.asInstanceOf[Getter[C] { type A = ThisA }]
      ret.forward1s = fwd1
      ret.reverse1s = rev1
      ret
    }
    /** Create a new Getter, starting from this one and appending an additional one-to-many mapping. */
    def getOneToMany[D<:{type GetterType<:Getter[D]}](fwd1:C=>Iterable[D], rev1:D=>C)(implicit m:Manifest[D]): D#GetterType { type A = Getter.this.A; type B = Getter.this.C } = {
      initOneToMany[D](newGetter[D](m), fwd1, rev1)
    }
    def initOneToMany[D<:{type GetterType<:Getter[D]}](getter:Getter[D]/*D#GetterType*/, fwd1:C=>Iterable[D], rev1:D=>C): D#GetterType { type A = Getter.this.A; type B = Getter.this.C } = {
      type ThisA = A
      type ThisC = C
      val ret = getter.asInstanceOf[D#GetterType{ type A = ThisA; type B = ThisC }]
      ret.prefix = Getter.this.asInstanceOf[Getter[C] { type A = ThisA }]
      ret.forward1m = fwd1
      ret.reverse1s = rev1
      ret
    }    
    // TODO If I uncomment "C with" below, I get scalac error: "illegal type selection from volatile type D".  It seems I should be able to do this, though.
    /** Create a new Getter, starting from this one and appending an additional symmetric many-to-many mapping.
        For example:  getSymmetricManyToMany[Person](p => p.mother.children.filter(p2=>p2 ne p)). */
    def getSymmetricManyToMany[D<:{type GetterType<:Getter[D]}](fwd1:C=>Iterable[D]): D#GetterType { type A = Getter.this.A; type B = Getter.this.C } = 
      initManyToMany[D](newGetter[D](this.getClass), fwd1, fwd1.asInstanceOf[D=>Iterable[C]])
    /** Create a new Getter, starting from this one and appending an additional symmetric one-to-one mapping. 
        For example:  getSymmetricOneToOne[Person](_.spouse)*/
    def getSymmetricOneToOne[D<:{type GetterType<:Getter[D]}](fwd1:C=>D): D#GetterType { type A = Getter.this.A; type B = Getter.this.C } = 
      initOneToOne[D](newGetter[D](this.getClass), fwd1, fwd1.asInstanceOf[D=>C])
    /** Create a new Getter, starting from this one and appending a mapping to one of its Attributes. */
    def getAttribute[D<:AttributeOf[C]](fwd1:C=>D): Getter[D] { type A = Getter.this.A; type B = Getter.this.C } = {
      type ThisA = A
      type ThisC = C
      val ret = new Getter[D] { type A = ThisA; type B = ThisC }
      //ret.prefix = Getter.this.asInstanceOf[Getter[C] with GetterHead[A,C]] // TODO Doesn't run because Getter doesn't have GetterHead
      ret.prefix = Getter.this.asInstanceOf[Getter[C] { type A = ThisA }]
      ret.forward1s = (c:C) => fwd1(c)
      ret.reverse1s = (d:D) => d.attributeOwner
      ret
    }
    def getOneWay[D<:Variable](fwd1:C=>D): Getter[D] { type A = Getter.this.A; type B = Getter.this.C } = {
      type ThisA = A
      type ThisC = C
      val ret = new Getter[D] { type A = ThisA; type B = ThisC }
      ret.prefix = Getter.this.asInstanceOf[Getter[C] { type A = ThisA }]
      ret.forward1s = (c:C) => fwd1(c)
      ret.reverse1m = (d:D) => Nil // throw new Error("This type shouldn't have changed.")
      ret
    }
    /** Create a new Getter, starting from this one and appending a symmetric function.  
        This differs from getSymmetricOneToOne in that it represents a mutable one-to-one relation, whereas getSymmetricOneToOne represents an immutable relation. */
    //def getSymmetricFunction[D<:Entity[C]#SymmetricFunction](fwd1:C=>D)(implicit m:Manifest[D#EntityType#GetterClass]): Getter[C] with GetterHead[A,C] with GetterMiddle[C,C] 
    // TODO I really want to say that D == C here.  I thought the above solution would work, but I get error "EntityType" does not have member "GetterClass", when it clearly does! 
    def getSymmetricFunction[D<:{type GetterType<:Getter[D]}](fwd1:C=>RefVariable[D]): D#GetterType { type A = Getter.this.A; type B = Getter.this.C } = {
      type ThisA = A
      type ThisC = C
      val ret = newGetter[D](this.getClass).asInstanceOf[D#GetterType { type A = ThisA; type B = ThisC }]
      ret.prefix = Getter.this.asInstanceOf[Getter[C] { type A = ThisA }]
      ret.forward1s = (c:C) => fwd1(c).value
      ret.reverse1s = (d:D) => fwd1(d.asInstanceOf[ret.B]).value.asInstanceOf[ret.B]
      ret
    }
    /** Create a new Getter, starting from this one as the 'src' of a relation, and appending a Getter for the 'dst' of the relation. */
    def getRelationSrc[R<:Relation[D,C],D<:{type GetterType<:Getter[D]}](r:R)(implicit m:Manifest[D], mr:Manifest[R#RelationshipType]): D#GetterType { type A = Getter.this.A; type B = Getter.this.C } = {
      type ThisA = A
      type ThisC = C
      val ret = newGetter[D](m).asInstanceOf[D#GetterType { type A = ThisA; type B = ThisC }]
      ret.prefix = Getter.this.asInstanceOf[Getter[C] { type A = ThisA }]
      ret.forward1m = (c:C) => r.getFromDst(c).map(_.src)
      ret.reverse1m = (d:D) => r.getFromSrc(d).map(_.dst)
      type ExtraD = R#RelationshipType
      //val myExtraGetter = new Getter[ExtraD] with GetterHead[A,ExtraD] with GetterMiddle[C,ExtraD] {}
      val myExtraGetter = new Getter[ExtraD] { type A = ThisA; type B = ThisC }
      myExtraGetter.prefix = Getter.this.asInstanceOf[Getter[C] { type A = ThisA }]
      myExtraGetter.forward1m = (c:C) => r.getFromDst(c)
      myExtraGetter.reverse1m = (d:ExtraD) => if (d.value) List(d.dst) else Nil //{ val dst = d.dst; if (dst.asInstanceOf[BooleanValue].value) List(dst) else Nil } // If the Relationship is false, don't follow the link
      ret.extraManifest = mr.asInstanceOf[Manifest[ExtraNeighborType]]
      ret.extraGetter = myExtraGetter.asInstanceOf[Getter[ExtraNeighborType]]
      ret
    }
    /** Create a new Getter, starting from this one as the 'dst' of a relation, and appending a Getter for the 'src' of the relation. */
    def getRelationDst[R<:Relation[C,D],D<:{type GetterType<:Getter[D]}](r:R)(implicit m:Manifest[D], mr:Manifest[R#RelationshipType]): D#GetterType { type A = Getter.this.A; type B = Getter.this.C } = {
      type ThisA = A
      type ThisC = C
      val ret = newGetter[D](m).asInstanceOf[D#GetterType { type A = ThisA; type B = ThisC }]
      ret.prefix = Getter.this.asInstanceOf[Getter[C] { type A = ThisA }]
      ret.forward1m = (c:C) => r.getFromSrc(c).map(_.dst)
      ret.reverse1m = (d:D) => r.getFromDst(d).map(_.src)
      type ExtraD = R#RelationshipType
      val myExtraGetter = new Getter[ExtraD] { type A = ThisA; type B = ThisC }
      myExtraGetter.prefix = Getter.this.asInstanceOf[Getter[C] { type A = ThisA }]
      myExtraGetter.forward1m = (c:C) => r.getFromSrc(c)
      myExtraGetter.reverse1m = (d:ExtraD) => if (d.value) List(d.src) else Nil //{ val src = d.src; if (src.asInstanceOf[BooleanValue].value) List(src) else Nil } // If the Relationship is false, don't follow the link
      ret.extraManifest = mr.asInstanceOf[Manifest[ExtraNeighborType]]
      ret.extraGetter = myExtraGetter.asInstanceOf[Getter[ExtraNeighborType]]
      ret
    }
    /*
    def getRelationshipFromDst[R<:Relation[D,C],D<:GetterType[D]](r:R)(implicit m:Manifest[R#RelationshipType#GetterClass]): Getter[R] with GetterHead[A,R] with GetterMiddle[C,R] = {
      val ret = newGetter[R#RelationshipType](m).asInstanceOf[R#RelationshipType#GetterClass with GetterHead[A,R] with GetterMiddle[C,R]]
      ret.prefix = Getter.this.asInstanceOf[Getter[C] with GetterHead[A,C]]
      ret.forward1m = (c:C) => r.getFromDst(c)
      ret.reverse1m = (d:D) => r.getFromSrc(d)
      ret
    }
    def getRelationshipFromSrc[R<:Relation[C,D],D<:GetterType[D]](r:R)(implicit m:Manifest[D#GetterClass]): Getter[D] with GetterHead[A,D] with GetterMiddle[C,D] = {
      val ret = newGetter[D](m).asInstanceOf[D#GetterClass with GetterHead[A,D] with GetterMiddle[C,D]]
      ret.prefix = Getter.this.asInstanceOf[Getter[C] with GetterHead[A,C]]
      ret.forward1m = (c:C) => r.getFromSrc(c)
      ret.reverse1m = (d:D) => r.getFromDst(d)
      ret
    }
    */
  }
  /** Fill in abstract type Getter.A with parameterized type.  Necessary for Scala type-inferencer. */
  //trait GetterHead[A1,C1/*<:GetterType[C1]*/] extends Getter[C1] { type A = A1 }
  type GetterHead[A1,C1] = Getter[C1] { type A = A1 }
  /** Fill in abstract type Getter.B with parameterized type.  Necessary for Scala type-inferencer. */
  //trait GetterMiddle[B1/*<:GetterType[B1]*/,C1/*<:GetterType[C1]*/] extends Getter[C1] { type B = B1 }
  /** Typical Getter trait inherited by users, and thus D#GetterClass is often a sub-class of this. */
  trait EntityGetter[A<:Entity[A]] extends Getter[A]
  trait AttributeGetter[A<:AttributeOf[E] /*with GetterType[A]*/,E] extends Getter[A] {
    //def attributeOwner: E = getOneToOne[E]((a:A)=>a.attributeOwner, (e:E)=>e.??)
  }
  class RelationshipGetter[A<:Entity[A],B<:Entity[B]] extends EntityGetter[Relationship[A,B]] {
    //def getSrc(implicit m:Manifest[A]): Getter  // TODO we can't go backwards from a source to an individual relationship; it would have to map to all matching the src.
  }
  // TODO? Consider avoiding the need to mix this into Entity by using instead duck typing: type WithGetterType = { type GetterClass <: Getter[_,_] }
  //trait GetterType[D] { type GetterClass <: Getter[D] }
  /** Construct a new Getter with tail type A. */
  def newGetter[V<:{type GetterType <: Getter[V]}](implicit m:Manifest[V]): V#GetterType = {
    newGetter[V](m.erasure)
  }
  def newGetter[V<:{type GetterType <: Getter[V]}](variableClass:Class[_]): V#GetterType = {
    val getterClass = getGetterClass(variableClass)
    val constructors = getterClass.getConstructors
    if (constructors.size != 1) throw new Error("Getters must have only one constructor; class="+getterClass+" has "+constructors.size+".  You can get this error if you failed to correctly set 'type GetterClass'.")
    val constructor = constructors.apply(0)
    val numArgs = constructor.getParameterTypes.length
    if (numArgs == 0) {
      constructor.newInstance().asInstanceOf[V#GetterType]
    } /*else if (numArgs == 1) {
      val args = new Array[Object](1)
      args(0) = null
      constructor.newInstance(args).asInstanceOf[A#GetterClass] // TODO Yipes, passing null here, when expecting a pointer to the outer instance!  OK as long as the user isn't relying on the outer instance.  Can we get it somehow?
    } */ else {
      val msg = new StringBuffer
      msg.append("Getter constructors must not take any arguments.\nInstead "+getterClass.getName+" takes "+numArgs+" argument(s): ")
      constructor.getParameterTypes.foreach(t => msg.append(t.getName+" "))
      msg.append("\nYou will get an unexpected single argument when you declare a Getter as an inner class;\nthe solution is to declare all Getters in packages, not inner classes.")
      throw new Error(msg.toString)
    }
  }
  /** Find the (sub)class of Getter to use for constructing a getter for variable class c. */
  private def getGetterClass(c:Class[_]) : Class[_] = {
    val debug = false
    if (debug) println("getGetterClass "+c)
    // First check this class to see if it specifies the GetterClass
    val classes = c.getDeclaredClasses()
    val index = if (classes == null) -1 else classes.findIndexOf(c=>c.getName.endsWith("$GetterClass"))
    //if (debug) println("  $GetterClass index="+index+"  classes "+classes.toList)
    if (index >= 0) {
      if (debug) println("getGetterClass   returning "+classes(index).getSuperclass)
      return classes(index).getSuperclass
    }
    // Next check the superclass and interfaces/traits; choose the most specific (subclass of) Domain
    val candidateGetterClasses = new ListBuffer[Class[_]]
    val sc = c.getSuperclass
    if (sc != null && sc != classOf[java.lang.Object]) 
      candidateGetterClasses += getGetterClass(sc)
    val interfaces = c.getInterfaces.elements
    while (interfaces.hasNext) {
      val dc = getGetterClass(interfaces.next)
      if (dc != null) candidateGetterClasses += dc
    }
    if (candidateGetterClasses.size > 0) {
      // Find the most specific subclass of the first domain class found
      var dc = candidateGetterClasses.first
      candidateGetterClasses.foreach(dc2 => if (dc.isAssignableFrom(dc2)) dc = dc2)
      if (debug) println("getGetterClass "+c+" specific="+dc)
      return dc
    } else
      null
  }

  /** Construct a new Getter representing the beginning of an getter chain, taking input A. */
  def newGetterUnit[X<:{type GetterType <: Getter[X]}](implicit m:Manifest[X]): X#GetterType { type A = X /* ; type B = X */ } = {
    //println("GetterUnit m="+m)
    newGetter[X](m).asInstanceOf[X#GetterType { type A = X }];
  }
  /** A immutable boolean observation variable that satisfies the type requirements necessary to be returned by a getter.
      Useful for implementing simple binary tests on observations.  Since its value will never change, you do not have
      to provide a rev1 function when creating a getter for it with getOneToOne. */
  class BooleanObservationWithGetter(f:Boolean) extends BooleanObservation(f) with Entity[BooleanObservationWithGetter] {
    type GetterType = BooleanObservationGetter
    class GetterClass extends BooleanObservationGetter
  }
  class BooleanObservationGetter extends EntityGetter[BooleanObservationWithGetter]
  
  
  
  
  
  
  // Define function for scoring compatibility between getter targets with CategoricalValues
  // Example usage:  Forany[Token] { t => Score(t, t.label) }
    
    
  type ScorableValues[X] = CategoricalValues //with GetterType[X]
  type ScorableValues0 = CategoricalValues //with GetterType[CategoricalValues]
  
  case class Score[X<:Variable](sns:ScoreNeighbor0[X]*) {
    def manifests : Seq[Manifest[_<:Variable]] = sns.flatMap(_.manifests)
    //def getters : Seq[GetterHead[X,ScorableValues0]] = sns.flatMap(_.getters)
    def getters : Seq[Getter[ScorableValues0] {type A = X}] = sns.flatMap(_.getters)
  }
 case class SparseScore[X<:Variable](override val sns:ScoreNeighbor0[X]*) extends Score(sns:_*)
  trait ScoreNeighbor0[X<:Variable] {
    def manifests : Iterable[Manifest[ScorableValues0]];
    //def getters : Seq[GetterHead[X,ScorableValues0]]
    def getters : Seq[Getter[ScorableValues0] {type A = X}]
  }
  //class ScoreNeighbor[X<:Variable,A<:ScorableValues[A]](a1:GetterHead[X,A])(implicit ma:Manifest[A]) extends ScoreNeighbor0[X]
  class ScoreNeighbor[X<:Variable,A<:ScorableValues[A]](a1:Getter[A] /*with GetterHead[X,A]*/)(implicit ma:Manifest[A]) extends ScoreNeighbor0[X] {
    def manifests = List(ma.asInstanceOf[Manifest[ScorableValues0]])
    //def getters = List(a1.asInstanceOf[GetterHead[X,ScorableValues0]])
    def getters = List(a1.asInstanceOf[Getter[ScorableValues0] {type A = X}])
  }
  
  implicit def getter2scoreneighbor[X<:Variable,Y<:ScorableValues[Y]](a:Getter[Y] { type A = X})(implicit ma:Manifest[Y]): ScoreNeighbor0[X] = { 
    //println("getter2scoreneighbor ma="+ma+" a="+a)
    new ScoreNeighbor[X,Y](a)(ma)
  }
  
  object Foreach {
    def apply[X<:Variable {type GetterType <: Getter[X]}](x2c:X#GetterType { type A = X}=>Score[X])(implicit m:Manifest[X]) = {
      val score = x2c(newGetterUnit[X](m))
      val manifests = score.manifests.toList.asInstanceOf[List[Manifest[ScorableValues0]]];
      val getters = score.getters
      val size = manifests.length
      type I = ScorableValues0
      if (classOf[SparseScore[X]].isAssignableFrom(score.getClass)) {
        size match {
          case 1 => new TemplateWithDotStatistics1[I]()(manifests(0)) with SparseWeights {
            override def unroll1(n1:I) = { val roots = getters(0).reverse(n1); if (!roots.isEmpty) Factor(n1) else Nil }
          }
          case 2 => new TemplateWithDotStatistics2[I,I]()(manifests(0), manifests(1)) with SparseWeights {
            def unroll1(n1:I) = { val roots = getters(0).reverse(n1); for (root <- roots; n2 <- getters(1).forward(root)) yield Factor(n1,n2) }
            def unroll2(n2:I) = { val roots = getters(1).reverse(n2); for (root <- roots; n1 <- getters(0).forward(root)) yield Factor(n1,n2) }
          }
          case 3 => new TemplateWithDotStatistics3[I,I,I]()(manifests(0), manifests(1), manifests(2)) with SparseWeights {
            def unroll1(n1:I) = { val roots = getters(0).reverse(n1); for (root <- roots; n2 <- getters(1).forward(root); n3 <- getters(2).forward(root)) yield Factor(n1,n2,n3) } 
            def unroll2(n2:I) = { val roots = getters(1).reverse(n2); for (root <- roots; n1 <- getters(0).forward(root); n3 <- getters(2).forward(root)) yield Factor(n1,n2,n3) } 
            def unroll3(n3:I) = { val roots = getters(2).reverse(n3); for (root <- roots; n1 <- getters(0).forward(root); n2 <- getters(1).forward(root)) yield Factor(n1,n2,n3) } 
          }
          case 4 => new TemplateWithDotStatistics4[I,I,I,I]()(manifests(0), manifests(1), manifests(2), manifests(3)) with SparseWeights {
            def unroll1(n1:I) = { val roots = getters(0).reverse(n1); for (root <- roots; n2 <- getters(1).forward(root); n3 <- getters(2).forward(root); n4 <- getters(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
            def unroll2(n2:I) = { val roots = getters(1).reverse(n2); for (root <- roots; n1 <- getters(0).forward(root); n3 <- getters(2).forward(root); n4 <- getters(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
            def unroll3(n3:I) = { val roots = getters(2).reverse(n3); for (root <- roots; n1 <- getters(0).forward(root); n2 <- getters(1).forward(root); n4 <- getters(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
            def unroll4(n4:I) = { val roots = getters(3).reverse(n4); for (root <- roots; n1 <- getters(0).forward(root); n2 <- getters(1).forward(root); n3 <- getters(2).forward(root)) yield Factor(n1,n2,n3,n4) } 
          }
        }
      } else {
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

    
  
  
    
  
  // Define functions for clauses in first-order logic
  // Example usage: Forany[Person] { p => p.smokes ==> p.cancer }
    
    
  /** The values of leaves of the formula tree.  
      For the common case of a BooleanTerm it is a BooleanValue (which inherits from DiscreteValue). */
  type FormulaArg = DiscreteValue //with GetterType[BooleanValue]
  //type FormulaValue[A] = BooleanValue //with GetterType[A];
  /** The collection of arguments to the boolean expression; the variables neighboring the factor.  
      Using CategoricalVariable instead of BooleanValue enables mixed use of BooleanValue and other DiscreteValues, as in IntExpression. */
  type FormulaArgs = ArrayStack[FormulaArg]

    
  /** Could also be known as a BooleanExpression. */
  trait Formula[X<:Variable] {
    def eval(x:FormulaArgs) : Boolean
    def manifests : List[Manifest[_<:FormulaArg]]
    def getters : List[GetterHead[X,FormulaArg]]
    def ==>(f:Formula[X]) = Implies(this, f)
    def ^(f:Formula[X]) = And(this, f)
    def v(f:Formula[X]) = Or(this, f)
    def unary_! = Not(this) // provides usage "!a"
    def <==>(f:Formula[X]) = BooleanEquals(this, f)
  }
  // TODO Why is this a case class?
  case class BooleanTerm[X<:Variable,A<:FormulaArg](g1:Getter[A]{type A=X})(implicit ma:Manifest[A]) extends Formula[X] {
    var extraNeighborCount = 0
    var manifests = ma.asInstanceOf[Manifest[FormulaArg]] :: Nil
    var getters = g1.asInstanceOf[GetterHead[X,FormulaArg]] :: Nil
    // this BooleanTerm has one value (of the last getter) needed for evaluation of the Formula, 
    // but it may have other mutable variables in its chain of getters, in particular a Relation.
    // We need to make sure that these variables will be neighbors in the Template created from this Formula, in the correct order
    var g: Getter[_] = g1
    //println("Term ma "+ma)
    while (g.getPrefix != null) { 
      if (g.extraManifest != null) {
        //println("Term adding extraManifest "+g.extraManifest)
        manifests = g.extraManifest.asInstanceOf[Manifest[FormulaArg]] :: manifests
        getters = g.extraGetter.asInstanceOf[GetterHead[X,FormulaArg]] :: getters
      }
      g = g.getPrefix
    }
    def eval(x:FormulaArgs): Boolean = {
      if (extraNeighborCount != 0) for (i <- 0 until extraNeighborCount) x.pop
      x.pop.asInstanceOf[BooleanValue].value
    }
  }

  /*abstract class Term2[X<:Variable,A<:FormulaArg,B<:FormulaArg](g1:GetterHead[X,A], g2:Getter0[X,B])(implicit ma:Manifest[A], mb:Manifest[B]) extends Formula[X] {
    // def eval(x:Args) is missing
    def manifests = List(ma,mb)
    val getters = {
      val pos1 = g1.argPosition; val pos2 = g2.argPosition; val max = if (pos1 > pos2) pos1 else pos2
      val a = new Array[Seq[Getter0[Variable,CategoricalVariable]]](max)
      for (i <- 0 until max)
        if (g1.argPosition == i && g2.argPosition == i)
          a(i) = List(g1.asInstanceOf[Getter0[Variable,CategoricalVariable]], g2.asInstanceOf[Getter0[Variable,CategoricalVariable]])
        else if (g1.argPosition == i)
          a(i) = List(g1.asInstanceOf[Getter0[Variable,CategoricalVariable]])
        else if (g2.argPosition == i)
          a(i) = List(g2.asInstanceOf[Getter0[Variable,CategoricalVariable]])
      a
    }
  }*/

  implicit def getter2formula[X<:Variable,Y<:BooleanValue/* with GetterType[A]*/](g:Getter[Y] {type A = X})(implicit ma:Manifest[Y]): Formula[X] = new BooleanTerm(g)(ma)
  
  abstract class Formula1[X<:Variable](c1:Formula[X]) extends Formula[X] {
    def manifests = c1.manifests
    def getters = c1.getters
  }
  case class Not[X<:Variable](f1:Formula[X]) extends Formula1(f1) {
    def eval(x:FormulaArgs) = ! f1.eval(x)
  }
  case class True[X<:Variable](f1:Formula[X]) extends Formula1(f1) { // noop, but forces implicit conversion to BooleanTerm
    def eval(x:FormulaArgs) = f1.eval(x)
  }
  abstract class Formula2[X<:Variable](c1:Formula[X], c2:Formula[X]) extends Formula[X] {
    def manifests = c1.manifests ++ c2.manifests
    def getters = c1.getters ++ c2.getters
  }
  case class And[X<:Variable](c1:Formula[X], c2:Formula[X]) extends Formula2[X](c1,c2) {
    def eval(x:FormulaArgs) = c1.eval(x) && c2.eval(x) 
  }
  case class Or[X<:Variable](c1:Formula[X], c2:Formula[X]) extends Formula2[X](c1,c2) {
    def eval(x:FormulaArgs) = c1.eval(x) || c2.eval(x) 
  }
  case class Implies[X<:Variable](c1:Formula[X], c2:Formula[X]) extends Formula2[X](c1,c2) {
    def eval(x:FormulaArgs) = (! c1.eval(x)) || c2.eval(x)
  }
  case class BooleanEquals[X<:Variable](c1:Formula[X], c2:Formula[X]) extends Formula2[X](c1,c2) {
    def eval(x:FormulaArgs) = c1.eval(x) == c2.eval(x)
  }
  case class Forall[X<:Variable](f1:Formula[X]) extends Formula1(f1) {
    def eval(x:FormulaArgs) = throw new Error("Not yet implemented") // Need to think carefully about this
  }
  case class Forsome[X<:Variable](f1:Formula[X]) extends Formula1(f1) {
    def eval(x:FormulaArgs) = throw new Error("Not yet implemented") // Need to think carefully about this
  }

  
  /** An expression whose value is an integer.  Type argument X is the type of the root of the expression. */
  trait IntExpression[X<:Variable] {
    def eval(x:FormulaArgs) : Int
    def manifests : List[Manifest[_<:FormulaArg]]
    def getters : List[GetterHead[X,FormulaArg]]
    def ===(f:IntExpression[X]) = IntEquals(this, f)
    def >(f:IntExpression[X]) = GreaterThan(this, f)
    def <(f:IntExpression[X]) = LessThan(this, f)
    // To avoid implicit conversion ambiguity, none of the operator names here should conflict with the operator names of Formula
  }

  case class IntTerm[X<:Variable,A<:FormulaArg](g1:GetterHead[X,A])(implicit ma:Manifest[A]) extends IntExpression[X] {
    def eval(x:FormulaArgs): Int = x.pop.intValue
    var manifests = List(ma.asInstanceOf[Manifest[FormulaArg]])
    var getters = List(g1.asInstanceOf[GetterHead[X,FormulaArg]])
    /*val getters = {
      val pos = g.argPosition
      val a = new Array[Seq[Getter0[Variable,CategoricalVariable]]](pos+1)
      a(pos+1) = List(g.asInstanceOf[Getter0[Variable,CategoricalVariable]]) // TODO can we get rid of the cast with covariant typing?
      a
    }*/
  } 
  
  implicit def getter2IntTerm[X<:Variable,A<:FormulaArg](g:GetterHead[X,A])(implicit ma:Manifest[A]): IntExpression[X] = new IntTerm(g)(ma)

  abstract class IntIntExpression2[X<:Variable](c1:IntExpression[X], c2:IntExpression[X]) extends IntExpression[X] {
    def manifests = c1.manifests ++ c2.manifests
    def getters = c1.getters ++ c2.getters
  }
  abstract class IntBoolExpression2[X<:Variable](c1:IntExpression[X], c2:IntExpression[X]) extends Formula[X] {
    def manifests = c1.manifests ++ c2.manifests
    def getters = c1.getters ++ c2.getters
    /*val getters = {
      val pos1 = c1.getters.size; val pos2 = c2.getters.size; val max = if (pos1 > pos2) pos1 else pos2
      val a = new Array[Seq[Getter0[Variable,CategoricalVariable]]](max+1)
      for (i <- 0 until max)
        if (c1.getters.length > i && c2.getters.length > i)
          a(i) = c1.getters(i) ++ c2.getters(i)
        else if (c1.getters.length > i)
          a(i) = c1.getters(i)
        else
          a(i) = c2.getters(i)
      a
    }*/
  }
  case class IntEquals[X<:Variable](c1:IntExpression[X], c2:IntExpression[X]) extends IntBoolExpression2[X](c1,c2) {
    def eval(x:FormulaArgs) = c1.eval(x) == c2.eval(x) 
  }
  case class GreaterThan[X<:Variable](c1:IntExpression[X], c2:IntExpression[X]) extends IntBoolExpression2[X](c1,c2) {
    def eval(x:FormulaArgs) = c1.eval(x) > c2.eval(x) 
  }
  case class LessThan[X<:Variable](c1:IntExpression[X], c2:IntExpression[X]) extends IntBoolExpression2[X](c1,c2) {
    def eval(x:FormulaArgs) = c1.eval(x) < c2.eval(x) 
  }
  
  
  
  /** The form of Template statistics used by a logical Formula. */
  trait LogicStatistics extends DotStatistics1[BooleanValue] {
    // Should a non-zero weight instead be spread across each of the two possibilities?
    def *(w:Double) : this.type = { this.weights(0) = 0.0; this.weights(1) = Math.log(w); this }
  }
  
  
  /** Create a Formula starting from a Getter */
  object Forany {
    def apply[X<:Variable {type GetterType <: Getter[X]}](x2c:X#GetterType {type A = X}=>Formula[X])(implicit m:Manifest[X]): Template with LogicStatistics = {
      type I = FormulaArg
      val getterRoot: X#GetterType { type A = X } = newGetterUnit[X](m)
      val formula = x2c(getterRoot)
      val manifests = formula.manifests.asInstanceOf[Seq[Manifest[I]]];
      //assert(formula.getters.length == 1, formula.getters.length)
      val getters = formula.getters
      val size = manifests.length
      size match {
        case 1 => new Template1[I]()(manifests(0)) with LogicStatistics {
          override def unroll1(n1:I) = { val roots = getters(0).reverse(n1); if (!roots.isEmpty) Factor(n1) else Nil }
          def statistics(n1:I) = { val s = new FormulaArgs; s+=n1; Stat(Bool(formula.eval(s))) }
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
    /*
  object Forany2 {
    def apply[X1<:Variable,X2<:Variable](x2c:(Arg[X1],Arg[X2])=>Formula) : Template with LogicStatistics = {
      type I = SingleIndexedVariable
      val formula = x2c (Arg[X1](0), Arg[X2](1))
      val manifests = formula.manifests.asInstanceOf[Seq[Manifest[I]]];
      assert(formula.getters.length == 2)
      val gettersPerArg = formula.getters
      val size = manifests.length
      size match {
        case 1 => new Template1[I]()(manifests(0)) with LogicStatistics {
          // TODO Is this case possible?
          override def unroll1(n1:I) = { if (gettersPerArg.exists(!_.apply(0).reverse(n1).isEmpty)) Factor(n1) else Nil }
          def statistics(n1:I) = { val s = new ArrayStack[SingleIndexedVariable]; s+=n1; Stat(Bool(formula.eval(s))) }
        }.init
        case 2 => new Template2[I,I]()(manifests(0), manifests(1)) with LogicStatistics {
          //val argPositions = new Array[Int](2) ....
          def unroll1(n1:I) = { for (getters <- gettersPerArg; root <- getters(0).reverse(n1); n2 <- getters(1).forward(root)) yield Factor(n1,n2) }
          def unroll2(n2:I) = { for (getters <- gettersPerArg; root <- getters(1).reverse(n2); n1 <- getters(0).forward(root)) yield Factor(n1,n2) }
          def statistics(n1:I, n2:I) = { val s = new ArrayStack[SingleIndexedVariable]; s+=n2; s+=n1; Stat(Bool(formula.eval(s))) }
        }.init
        case 3 => new Template3[I,I,I]()(manifests(0), manifests(1), manifests(2)) with LogicStatistics {
          def unroll1(n1:I) = { for (getters <- gettersPerArg; root <- getters(0).reverse(n1); n2 <- getters(1).forward(root); n3 <- getters(2).forward(root)) yield Factor(n1,n2,n3) }
          def unroll2(n2:I) = { for (getters <- gettersPerArg; root <- getters(1).reverse(n2); n1 <- getters(0).forward(root); n3 <- getters(2).forward(root)) yield Factor(n1,n2,n3) }
          def unroll3(n3:I) = { for (getters <- gettersPerArg; root <- getters(2).reverse(n3); n1 <- getters(0).forward(root); n2 <- getters(1).forward(root)) yield Factor(n1,n2,n3) }
          def statistics(n1:I, n2:I, n3:I) = { val s = new ArrayStack[SingleIndexedVariable]; s+=n3; s+=n2; s+=n1; Stat(Bool(formula.eval(s))) }
        }.init
      }
    }
  }
   */


  

}

