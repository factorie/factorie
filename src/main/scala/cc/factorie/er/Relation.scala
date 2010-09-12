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

/** Representing a directed relationship from <code>src</code> to <code>dst</code>.  
 Its boolean value indicates whether the relationship is actually present or not. 
 A Relationship is also an "Entity" so that Relationships can have Attributes. 
 Upon custruction the initial value of this variable must be false so that when Relation.add 
 sets its value to true, the change gets put on the DiffList. */
// TODO Is this last sentence a bad idea?  I could avoid it by spliting 'AttributeHolding' out of Entity.
// Note that the current state allows there to be "Relations among Relationships"... hmmm!  Might this be useful?
class Relationship[A<:Entity[A],B<:Entity[B]](val src:A, val dst:B) extends BooleanVariable with Entity[Relationship[A,B]] {
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
  val noRelationships = ListBuffer.empty[RelationshipType]
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
  def iterator = ab2r.valuesIterator.filter(_.booleanValue)
  def asIterable = new Iterable[RelationshipType] {
    override def size = Relation.this.size
    def iterator = Relation.this.iterator
  }
  /** Return the Relationship between a and b, creating as necessary.  If it is created, its initial value will be false. */
  def get(a:A,b:B): RelationshipType = ab2r.getOrElse((a,b), addEntry((a,b))) // TODO verify in the newly-created case that r.value is false
  def getFromSrc(a:A): Iterable[RelationshipType] = a2rs.getOrElse(a, noRelationships).filter(r => r.booleanValue)
  def getFromDst(b:B): Iterable[RelationshipType] = b2rs.getOrElse(b, noRelationships).filter(r => r.booleanValue)
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
    def variable = if (r.booleanValue) r else null
    def redo = addRelationship(r)
    def undo = removeRelationship(r)
  } 
  case class RelationRemoveDiff(r:RelationshipType) extends Diff {
    def variable = if (r.booleanValue) r else null
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

/*class SymmetricFunction[A>:Null<:Entity[A]] //extends GetterType[A]
{
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
  def iterator = a2t.iterator.filter(pair=>pair._1.hashCode < pair._2.hashCode)
  def get(a:A) = a2t.getOrElse(a, null)
  def apply(a1:A, a2:A) = this.add(a1,a2)
  def forward1 = (a:A) => a2t.getOrElse(a,null)
  def reverse1 = (a:A) => a2t.getOrElse(a,null)
  //protected override def stringPrefix ="SymmetricFunction" 
}*/



