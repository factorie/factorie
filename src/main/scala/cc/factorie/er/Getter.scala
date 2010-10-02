/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */



package cc.factorie.er
import cc.factorie._
import scala.collection.mutable.{ArrayStack,HashSet,HashMap,ListBuffer}


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
  type ExtraNeighborType = BooleanVar // really Relationship[_,C]
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
      newGetter[D](m.erasure), 
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
    initManyToOne[D](newGetter[D](m.erasure), fwd1, rev1)
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
    initOneToOne[D](newGetter[D](m.erasure), fwd1, rev1)
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
    initOneToMany[D](newGetter[D](m.erasure), fwd1, rev1)
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
  def getSymmetricManyToMany[D<:{type GetterType<:Getter[D]}](fwd1:C=>Iterable[D])(implicit m:Manifest[D]): D#GetterType { type A = Getter.this.A; type B = Getter.this.C } = 
    initManyToMany[D](newGetter[D](m.erasure), fwd1, fwd1.asInstanceOf[D=>Iterable[C]])
  /** Create a new Getter, starting from this one and appending an additional symmetric one-to-one mapping. 
   For example:  getSymmetricOneToOne[Person](_.spouse)*/
  def getSymmetricOneToOne[D<:{type GetterType<:Getter[D]}](fwd1:C=>D)(implicit m:Manifest[D]): D#GetterType { type A = Getter.this.A; type B = Getter.this.C } = 
    initOneToOne[D](newGetter[D](m.erasure), fwd1, fwd1.asInstanceOf[D=>C])
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
  def getSymmetricFunction[D<:{type GetterType<:Getter[D]}](fwd1:C=>RefVariable[D])(implicit m:Manifest[D]): D#GetterType { type A = Getter.this.A; type B = Getter.this.C } = {
    type ThisA = A
    type ThisC = C
    val ret = newGetter[D](m.erasure).asInstanceOf[D#GetterType { type A = ThisA; type B = ThisC }]
    ret.prefix = Getter.this.asInstanceOf[Getter[C] { type A = ThisA }]
    ret.forward1s = (c:C) => fwd1(c).value
    ret.reverse1s = (d:D) => fwd1(d.asInstanceOf[ret.B]).value.asInstanceOf[ret.B]
    ret
  }
  /** Create a new Getter, starting from this one as the 'src' of a relation, and appending a Getter for the 'dst' of the relation. */
  def getRelationSrc[R<:Relation[D,C2],D<:Entity[D]{type GetterType<:Getter[D]},C2<:C with Entity[C2]](r:R)(implicit m:Manifest[D], mr:Manifest[R#RelationshipType]): D#GetterType { type A = Getter.this.A; type B = Getter.this.C } = {
    type ThisA = A
    type ThisC = C
    val ret = newGetter[D](m.erasure).asInstanceOf[D#GetterType { type A = ThisA; type B = ThisC }]
    ret.prefix = Getter.this.asInstanceOf[Getter[C] { type A = ThisA }]
    ret.forward1m = (c:C) => r.getFromDst(c.asInstanceOf[C2]).map(_.src)
    ret.reverse1m = (d:D) => r.getFromSrc(d).map(_.dst)
    type ExtraD = R#RelationshipType
    //val myExtraGetter = new Getter[ExtraD] with GetterHead[A,ExtraD] with GetterMiddle[C,ExtraD] {}
    val myExtraGetter = new Getter[ExtraD] { type A = ThisA; type B = ThisC }
    myExtraGetter.prefix = Getter.this.asInstanceOf[Getter[C] { type A = ThisA }]
    myExtraGetter.forward1m = (c:C) => r.getFromDst(c.asInstanceOf[C2])
    myExtraGetter.reverse1m = (d:ExtraD) => if (d.value) List(d.dst) else Nil //{ val dst = d.dst; if (dst.asInstanceOf[BooleanValue].value) List(dst) else Nil } // If the Relationship is false, don't follow the link
    ret.extraManifest = mr.asInstanceOf[Manifest[ExtraNeighborType]]
    ret.extraGetter = myExtraGetter.asInstanceOf[Getter[ExtraNeighborType]]
    ret
  }
     /** Create a new Getter, starting from this one as the 'dst' of a relation, and appending a Getter for the 'src' of the relation. */
  def getRelationDst[R<:Relation[C2,D],C2<:C with Entity[C2],D<:Entity[D]{type GetterType<:Getter[D]}](r:R)(implicit m:Manifest[D], mr:Manifest[R#RelationshipType]): D#GetterType { type A = Getter.this.A; type B = Getter.this.C } = {
    type ThisA = A
    type ThisC = C
    val ret = newGetter[D](m.erasure).asInstanceOf[D#GetterType { type A = ThisA; type B = ThisC }]
    ret.prefix = Getter.this.asInstanceOf[Getter[C] { type A = ThisA }]
    ret.forward1m = (c:C) => r.getFromSrc(c.asInstanceOf[C2]).map(_.dst)
    ret.reverse1m = (d:D) => r.getFromDst(d).map(_.src)
    type ExtraD = R#RelationshipType
    val myExtraGetter = new Getter[ExtraD] { type A = ThisA; type B = ThisC }
    myExtraGetter.prefix = Getter.this.asInstanceOf[Getter[C] { type A = ThisA }]
    myExtraGetter.forward1m = (c:C) => r.getFromSrc(c.asInstanceOf[C2])
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



/** A immutable boolean observation variable that satisfies the type requirements necessary to be returned by a getter.
 Useful for implementing simple binary tests on observations.  Since its value will never change, you do not have
 to provide a rev1 function when creating a getter for it with getOneToOne. */
class BooleanObservationWithGetter(f:Boolean) extends BooleanObservation(f) with Entity[BooleanObservationWithGetter] {
  type GetterType = BooleanObservationGetter
  class GetterClass extends BooleanObservationGetter
}
class BooleanObservationGetter extends EntityGetter[BooleanObservationWithGetter]
