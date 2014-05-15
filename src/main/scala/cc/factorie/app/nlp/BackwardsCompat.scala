package cc.factorie.app.nlp

import cc.factorie.variable._
import cc.factorie.app.nlp.phrase._
import cc.factorie.app.nlp.coref._
import cc.factorie.util.Attr
import scala.reflect.ClassTag
import scala.collection.mutable.{ArrayBuffer, ListBuffer, HashMap, LinkedHashMap}

object RelationArgFeaturesDomain extends CategoricalDomain[String]

class ArgFeatures(val arg: PairwiseMention, val first: Boolean) extends BinaryFeatureVectorVariable[String] {
  def domain = RelationArgFeaturesDomain

  def compute() = {
    this += "BIAS"
    // TODO compute relation features using "first" and "arg"
    // TODO convert Lexicons (from refectorie.proj.jntinf) to app.chain.Lexicon
    for (tok <- arg.tokens) {
      this += "POS_" + tok.posTag.categoryValue
      if (tok.string(0).isLower)
        this += "STEM_" + tok.string.replaceAll("\\s+", " ").take(5)
      //for (lex <- Lexicons.getMemberships(tok.string.replaceAll("\\s+", " ").toLowerCase))
      //  this += "TOK-LEX-" + lex
    }
//      for (lex <- Lexicons.getMemberships(arg.phrase.replaceAll("\\s+", " ").toLowerCase))
//        this += "PHRASE-LEX-" + lex
    this += "HEAD_POS_" + arg.headToken.posTag.categoryValue
//      for (lex <- Lexicons.getMemberships(arg.headToken.string.replaceAll("\\s+", " ").toLowerCase))
//        this += "HEAD_LEX-" + lex
  }
}


class RelationMention(val arg1: PairwiseMention, val arg2: PairwiseMention, val relationType: String, val relationSubType: Option[String]) extends ArrowVariable(arg1, arg2) with Attr {
    val arg1Features = new ArgFeatures(arg1, true)
    val arg2Features = new ArgFeatures(arg2, false)
//    val features = new Features(this)

//    def computeFeatures() = {
//      //arg1Features.compute
//      //arg2Features.compute
//      features.compute
//      FeatureNormalizer += features
//    }
  }

trait TokenSpanMention extends TokenSpan with Entity

class RelationMentions extends SetVariable[RelationMention]

/** The string-valued name for an entity. */
class EntityName(val entity:Entity, s:String) extends StringVariable(s)

/** An entity represented by a string-valued name. */
class NamedEntity(s:String) extends Entity {
  attr += new EntityName(this, s)
  def name = attr[EntityName]
  @deprecated("Use name.value instead") def string = name.value
}


@deprecated("Use NamedEntity instead.") class EntityVariable(s:String) extends NamedEntity(s)

/** A pointer from a child Entity (or Mention) to its parent Entity in an coreference problem. */
class EntityRef(theSrc:Entity, initialDst:Entity) extends ArrowVariable(theSrc, initialDst) {
  if (dst ne null) dst._addChildEntity(src)(null)
  override def set(e:Entity)(implicit d:DiffList): Unit = {
    if (e ne dst) {
      val old = dst
      if (dst ne null) {
        dst._removeChildEntity(src)
        dst.removedChildHook(src)
      }
      super.set(e)
      src.changedParentEntityHook(old,e)
      if(e!=null){
        e._addChildEntity(src)
        e.addedChildHook(src)
      }
    }
  }
  final def mention = src
  final def entity = dst
}

trait PairwiseMention extends TokenSpan with Entity {
  val edges = new ArrayBuffer[PairwiseLabel]
  var _head: Token = null
  def headToken: Token = _head
  // this is done for cases where the mention is split across multiple sentence in error (e.g. in reACE: george w. | bush)
  override def sentence = if (_head ne null) headToken.sentence else super.sentence
}

class ChildEntities(val entity:Entity) extends SetVariable[Entity]

/**An attribute that knows what entity it belongs to*/
trait EntityAttr extends Var {
  def entity: Entity
}

/** A trait for entities (and mentions, sub-entities and parent-entities) in a coreference problem.
    Abstract classes here are string, parentEntityRef, childEntities, _addChildEntity and _removeChildEntity. */
trait Entity extends Attr {
  @deprecated("Will be removed.  Entities are not guaranteed to have string names.") def string: String
  def id: Any = this // Override to make some persistent id
  attr += new EntityRef(this,null)
  def initializeAttributesOfStructure():Unit = {}
  def removedChildHook(entity:Entity)(implicit d:DiffList)={}
  def addedChildHook(entity:Entity)(implicit d:DiffList)={}
  def changedParentEntityHook(oldEntity:Entity,newEntity:Entity)(implicit d:DiffList)={}
  /** Ensure that we return a non-null value. */
  private def _ensuredChildEntities: ChildEntities = {
    var result = childEntities
    if (result eq null) {
      result = new ChildEntities(this)
      attr += result
    }
    result
  }
  def childEntities: ChildEntities = attr[ChildEntities]
  // The following two methods will work even if childEntities is null
  def childEntitiesSize: Int = { val se = childEntities; if (se eq null) 0 else se.size }
  def childEntitiesIterator: Iterator[Entity] = { val se = childEntities; if (se eq null) Iterator.empty else se.iterator }
  //the other iterator will break if you call setParentEntity(someEntity)(d) on a child while you are iterating over it, a common use case in MCMC.
  def safeChildEntitiesSeq: Seq[Entity]={val r = new ArrayBuffer[Entity];for(e<-childEntitiesIterator)r += e;r}
  // Next two methods should only be called in EntityRef
  def _addChildEntity(e:Entity)(implicit d:DiffList): Unit = _ensuredChildEntities.add(e)
  def _removeChildEntity(e:Entity)(implicit d:DiffList): Unit = _ensuredChildEntities.remove(e)
  def parentEntityRef: EntityRef = attr[EntityRef]
  def parentEntity: Entity = { val ref = parentEntityRef; if (ref eq null) null else parentEntityRef.dst }
  def parentEntityOption: Option[Entity] = { val ref = parentEntityRef; if (ref eq null) None else if (ref.value eq null) None else Some(ref.dst) }
  final def setParentEntity(e:Entity)(implicit d:DiffList): Unit = parentEntityRef.set(e) // Just a convenient alias
  def isConnected: Boolean = (parentEntity ne null) || childEntitiesSize > 0 || isObserved
  //def entityRoot: Entity = { val s = parentEntity; if (s eq null) this else this.entityRoot }©
  def entityRoot: Entity = if (isRoot) this else parentEntity.entityRoot
  def isRoot:Boolean = parentEntityRef == null || parentEntityRef.dst == null
  def isLeaf:Boolean = childEntitiesSize==0
  var isObserved:Boolean = false
  //var treatAsObserved:Boolean=false
  /** Recursively descend sub-entities and return only those matching criterion */
  def filterDescendants(test:Entity=>Boolean): Seq[Entity] = childEntitiesIterator.filter(test).toSeq
  def descendantsOfClass[A<:Entity](cls:Class[A]): Seq[A] = {
    var result = new ListBuffer[A]
    result += this.asInstanceOf[A]
    for (entity <-childEntitiesIterator) {
      //if (cls.isAssignableFrom(entity.getClass))
      //  result += entity.asInstanceOf[A]
      result ++= entity.descendantsOfClass[A](cls)
    }
    result
  }
  def numLeaves:Int = {
    var result = 0
    incIfLeaf(this)
    def incIfLeaf(e:Entity):Unit ={
      if(e.isLeaf)result += 1
      for(c <- e.childEntitiesIterator)incIfLeaf(c)
    }
    result
  }

  /*
  def descendantsOfClass[A<:Entity](cls:Class[A],result:ListBuffer[A] = new ListBuffer[A]): Seq[A] = {
    if(cls.isAssignableFrom(entityRoot.getClass))result += this.asInstanceOf[A]
    for (entity <-childEntitiesIterator) {
      result ++= entity.descendantsOfClass[A](cls,result)
    }
    result
  }
  */
  def descendantsOfClass[A<:Entity](implicit m:ClassTag[A]): Seq[A] = descendantsOfClass[A](m.runtimeClass.asInstanceOf[Class[A]])
  final def depth:Int = if(parentEntity eq null)0 else 1 + parentEntity.depth
  //def depth:Int = depth(0)
  //def depth(d:Int):Int = if(parentEntity==null)0 else parentEntity.depth(d+1)
  def getAncestor(numGenerationsBack:Int):Entity = if(numGenerationsBack==0)this else getAncestor(numGenerationsBack - 1)
}

class PairwiseLabel(val m1:PairwiseMention, val m2:PairwiseMention, b:Boolean) extends LabeledBooleanVariable(b) {
  def other(m:PairwiseMention): Option[PairwiseMention] = if(m == m1) Some(m2) else if (m == m2) Some(m1) else None
}