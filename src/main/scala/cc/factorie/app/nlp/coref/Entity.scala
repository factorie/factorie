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

package cc.factorie.app.nlp.coref
import cc.factorie._
import cc.factorie.app.nlp._
import scala.collection.mutable.{ArrayBuffer,ListBuffer}

/** A trait for entities (and mentions, sub-entities and super-entities) in a coreference problem.
    Abstract classes here are string, superEntityRef, subEntities, _addSubEntity and _removeSubEntity. */
trait Entity {
  def string: String
  def superEntityRef: EntityRef
  def superEntity: Entity = { val ref = superEntityRef; if (superEntityRef eq null) null else superEntityRef.dst }
  def setSuperEntity(e:Entity)(implicit d:DiffList): Unit = superEntityRef.set(e)
  def subEntities: Iterable[Entity]
  // Next two methods should only be called in EntityRef
  def _addSubEntity(e:Entity)(implicit d:DiffList): Unit
  def _removeSubEntity(e:Entity)(implicit d:DiffList): Unit
  /** Recursively descend sub-entities to return only the Mentions */
  def mentions: Seq[Mention] = {
    var result = new ListBuffer[Mention]
    for (entity <- subEntities) {
      entity match {
        case mention:Mention => result += mention
        case _ => {}
      }
      result ++= entity.mentions
    }
    result
  }
  def mentionsOfClass[A<:Mention](cls:Class[A]): Seq[A] = {
    var result = new ListBuffer[A]
    for (entity <- subEntities) {
      if (cls.isAssignableFrom(entity.getClass))
        result += entity.asInstanceOf[A]
      result ++= entity.mentionsOfClass[A](cls)
    }
    result
  }
  def mentionsOfClass[A<:Mention](implicit m:Manifest[A]): Seq[A] = mentionsOfClass[A](m.erasure.asInstanceOf[Class[A]])
}

trait Mention extends Entity {
  // Just aliases for nicer names
  def entity: Entity = superEntity
  def entityRef: EntityRef = superEntityRef
  def setEntity(e:Entity)(implicit d:DiffList): Unit = setSuperEntity(e)
}

class PairwiseLabel(val m1:PairwiseMention, val m2:PairwiseMention, b:Boolean) extends LabelVariable(b) with BooleanVar {
  def other(m:PairwiseMention): Option[PairwiseMention] = if(m == m1) Some(m2) else if (m == m2) Some(m1) else None
}
trait PairwiseMention extends TokenSpanMention {
  val edges = new ArrayBuffer[PairwiseLabel]
  var _head: Token = null
  def headToken: Token = _head
}
abstract class PairwiseTemplate extends Template3[PairwiseMention, PairwiseMention, PairwiseLabel] with Statistics2[BooleanValue,CorefAffinity] {
  def statistics(v:Values): Stat = {
    val mention1 = v._1
    val mention2 = v._2
    val coref: Boolean = v._3.booleanValue
    new Stat(null, null)
  }
}
abstract class PairwiseTransitivityTemplate extends Template3[PairwiseLabel,PairwiseLabel,PairwiseLabel] with Statistics1[BooleanValue] {
  //def unroll1(p:PairwiseBoolean) = for (m1 <- p.edge.src.edges; m2 <- p.edge.dst.edges; if (m1)
}

// In the world view with sub-entities and super-entities, a mention is a kind of entity with no sub-entities
trait TokenSpanMention extends TokenSpan with Mention {
  def superEntityRef: EntityRef = attr[EntityRef]
  //def setSuperEntity(e:Entity)(implicit d:DiffList): Unit = entityRef.setDst(e)
  private def _subEntities: SubEntities = attr[SubEntities]
  private def _ensuredSubEntities: SubEntities = attr.getOrElseUpdate[SubEntities](new SubEntities)
  def subEntities: Iterable[Entity] = { val se = _subEntities; if (se eq null) Nil else se }
  def _addSubEntity(e:Entity)(implicit d:DiffList): Unit = _ensuredSubEntities.add(e)
  def _removeSubEntity(e:Entity)(implicit d:DiffList): Unit = _ensuredSubEntities.remove(e)
  def sentence = this(0).sentence
}

class SubEntities extends SetVariable[Entity]


class EntityVariable(var initialCanonicalString:String) extends SetVariable[Entity] with Entity with Attr {
  //def entityRef: EntityRef = attr[EntityRef]
  val canonical = new StringVariable(initialCanonicalString)
  def canonicalString: String = canonical.value
  def string = canonicalString
  //def mentions: scala.collection.Set[Entity] = value
  val superEntityRef = new EntityRef(this, null)
  def subEntities = value
  def _addSubEntity(e:Entity)(implicit d:DiffList): Unit = add(e)
  def _removeSubEntity(e:Entity)(implicit d:DiffList): Unit = remove(e)

  //def closestPreviousMention(position:Int): Entity = mentions.filter(_.start < position).toSeq.sortBy(- _.start).head
  override def toString = "Entity("+string+":"+mentions.toSeq.size+")"
}

class EntityRef(theSrc:Entity, initialDst:Entity) extends ArrowVariable(theSrc, initialDst) {
  if (dst ne null) dst._addSubEntity(src)(null)
  override def set(e:Entity)(implicit d:DiffList): Unit = {
    if (e ne dst) {
      if (dst ne null) dst._removeSubEntity(src)
      e._addSubEntity(src)
      super.set(src, e)
    }
  }
  final def mention = src
  final def entity = dst
}

// Everything below here will be moved to a cc.factorie.example soon.

object CorefAffinityDimensionDomain extends EnumDomain {
  val Bias, ExactMatch, SuffixMatch, EntityContainsMention, EditDistance2, EditDistance4, NormalizedEditDistance9, NormalizedEditDistance5, Singleton = Value
}
object CorefAffinityDomain extends CategoricalVectorDomain[String] {
  override lazy val dimensionDomain = CorefAffinityDimensionDomain
}
class CorefAffinity extends BinaryFeatureVectorVariable[String] {
  def domain = CorefAffinityDomain
}

class EntityMentionModel extends TemplateModel(
  new Template1[EntityRef] with DotStatistics1[CorefAffinity#Value] {
    override def statisticsDomains = List(CorefAffinityDimensionDomain)
    //println("*** EntityMentionModel index="+CorefAffinityDomain.dimensionDomain.index("ExactMatch"))
    weights(CorefAffinityDimensionDomain.Bias) = -1
    weights(CorefAffinityDimensionDomain.ExactMatch) = 10
    weights(CorefAffinityDimensionDomain.SuffixMatch) = 2
    weights(CorefAffinityDimensionDomain.EntityContainsMention) = 3
    weights(CorefAffinityDimensionDomain.EditDistance2) = 4
    weights(CorefAffinityDimensionDomain.NormalizedEditDistance9) = -10
    weights(CorefAffinityDimensionDomain.NormalizedEditDistance5) = -2
    weights(CorefAffinityDimensionDomain.Singleton) = -1
    def statistics(values:Values) = {
      val mention: Entity = values._1._1
      val entity: Entity = values._1._2
      val affinity = new CorefAffinity
      if (mention.string == entity.string) affinity += CorefAffinityDimensionDomain.ExactMatch
      if (mention.string.takeRight(4) == entity.string.takeRight(4)) affinity += CorefAffinityDimensionDomain.SuffixMatch
      if (entity.string.contains(mention.string)) affinity += CorefAffinityDimensionDomain.EntityContainsMention
      val editDistance = entity.string.editDistance(mention.string)
      val normalizedEditDistance = editDistance / entity.string.length
      if (editDistance <= 2) affinity += CorefAffinityDimensionDomain.EditDistance2
      if (editDistance <= 4) affinity += CorefAffinityDimensionDomain.EditDistance4
      if (normalizedEditDistance > .5) affinity += CorefAffinityDimensionDomain.NormalizedEditDistance5
      if (normalizedEditDistance > .9) affinity += CorefAffinityDimensionDomain.NormalizedEditDistance9
      if (entity.subEntities.size == 1) affinity += CorefAffinityDimensionDomain.Singleton
      val result = Stat(affinity.value)
      val str = result.toString
      //println("### EntityMentionModel Stat="+str)
      result
    }
  }
)

class PairwiseModel extends TemplateModel(
  new Template1[EntityRef] with DotStatistics1[CorefAffinity#Value] {
    override def statisticsDomains = List(CorefAffinityDimensionDomain)
    //println("*** PairwiseModel index="+CorefAffinityDomain.dimensionDomain.index("ExactMatch"))
    weights(CorefAffinityDimensionDomain.Bias) = -1
    weights(CorefAffinityDimensionDomain.ExactMatch) = 10
    weights(CorefAffinityDimensionDomain.SuffixMatch) = 2
    weights(CorefAffinityDimensionDomain.EntityContainsMention) = 3
    weights(CorefAffinityDimensionDomain.EditDistance2) = 4
    weights(CorefAffinityDimensionDomain.NormalizedEditDistance9) = -10
    weights(CorefAffinityDimensionDomain.NormalizedEditDistance5) = -2
    weights(CorefAffinityDimensionDomain.Singleton) = -1
    def statistics(values:Values) = {
      val mention: Entity = values._1._1
      val entity: Entity = values._1._2
      val affinity = new CorefAffinity
      if (mention.string == entity.string) affinity += CorefAffinityDimensionDomain.ExactMatch
      if (mention.string.takeRight(4) == entity.string.takeRight(4)) affinity += CorefAffinityDimensionDomain.SuffixMatch
      if (entity.string.contains(mention.string)) affinity += CorefAffinityDimensionDomain.EntityContainsMention
      val editDistance = entity.string.editDistance(mention.string)
      val normalizedEditDistance = editDistance / entity.string.length
      if (editDistance <= 2) affinity += CorefAffinityDimensionDomain.EditDistance2
      if (editDistance <= 4) affinity += CorefAffinityDimensionDomain.EditDistance4
      if (normalizedEditDistance > .5) affinity += CorefAffinityDimensionDomain.NormalizedEditDistance5
      if (normalizedEditDistance > .9) affinity += CorefAffinityDimensionDomain.NormalizedEditDistance9
      if (entity.subEntities.size == 1) affinity += CorefAffinityDimensionDomain.Singleton
      val result = Stat(affinity.value)
      val str = result.toString
      //println("### PairwiseModel Stat="+str)
      result
    }
  }
)

/*class PairwiseModel2 extends TemplateModel(
  new Template4[EntityRef2,EntityRef2,Mention,Mention] with DotStatistics1[DiscreteVectorValue] {
    def unroll1 (er:EntityRef2) = for (other <- er.value.mentions; if (other.entityRef.value == er.value)) yield 
        if (er.mention.hashCode > other.hashCode) Factor(er, other.entityRef, er.mention, other.entityRef.mention)
        else Factor(er, other.entityRef, other.entityRef.mention, er.mention)
    def unroll2 (er:EntityRef2) = Nil // symmetric
    def unroll3 (mention:Mention) = throw new Error
    def unroll4 (mention:Mention) = throw new Error
    def statistics (values:Values) = throw new Error // Stat(new MentionAffinity(values._3, values._4).value)
  }
)*/


