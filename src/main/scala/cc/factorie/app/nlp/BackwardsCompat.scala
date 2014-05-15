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

trait DebugableTemplate {
  protected var _debug: Boolean = false
  def debugOn() = _debug = true
  def debugOff() = _debug = false
  def name: String
  def debug(score: Double): String = score + " (" + name + ")"
}

/** Basic trait for doing operations with bags of words */
trait BagOfWords {
  // extends scala.collection.Map[String,Double]{
  //def empty: This
  def size: Int
  def asHashMap: HashMap[String, Double]
  def apply(word: String): Double
  def iterator: Iterator[(String, Double)]
  def l2Norm: Double
  def l1Norm: Double
  def *(that: BagOfWords): Double
  def deductedDot(that: BagOfWords, deduct: BagOfWords): Double
  def cosineSimilarityINEFFICIENT(that: BagOfWords, deduct: BagOfWords): Double = {
    //println("  (1) bag: "+that)
    //println("  (1) that   : "+that.l2Norm)
    //println("  (1) that bf: "+that.l2NormBruteForce)
    that.removeBag(deduct)
    //println("  (2) bag: "+that)
    //println("  (2) that   : "+that.l2Norm)
    //println("  (2) that bf: "+that.l2NormBruteForce)
    val result = cosineSimilarity(that)
    that.addBag(deduct)
    //println("  (3) bag: "+that)
    //println("  (3) that   : "+that.l2Norm)
    //println("  (3) that bf: "+that.l2NormBruteForce)
    result
  }
  def cosineSimilarity(that: BagOfWords, deduct: BagOfWords): Double = {
    //val smaller = if(this.size<that.size)this else that
    //val larger = if(that.size<this.size)this else that
    val numerator: Double = this.deductedDot(that, deduct)
    if (numerator != 0.0) {
      val thatL2Norm = Math.sqrt(deduct.l2Norm * deduct.l2Norm + that.l2Norm * that.l2Norm - 2 * (deduct * that))
      val denominator: Double = this.l2Norm * thatL2Norm
      if (denominator == 0.0 || denominator != denominator) 0.0 else numerator / denominator
    } else 0.0
  }
  def cosineSimilarity(that: BagOfWords): Double = {
    val numerator: Double = this * that
    val denominator: Double = this.l2Norm * that.l2Norm
    if (denominator == 0.0 || denominator != denominator) 0.0 else numerator / denominator
  }
  def +=(s: String, w: Double = 1.0): Unit
  def -=(s: String, w: Double = 1.0): Unit
  def ++=(that: BagOfWords): Unit = for ((s, w) <- that.iterator) this +=(s, w)
  def --=(that: BagOfWords): Unit = for ((s, w) <- that.iterator) this -=(s, w)
  def contains(s: String): Boolean
  def l2NormBruteForce: Double = {
    var result = 0.0
    for ((k, v) <- iterator)
      result += v * v
    scala.math.sqrt(result)
  }
  def addBag(that: BagOfWords): Unit
  def removeBag(that: BagOfWords): Unit
}

class SparseBagOfWords(initialWords: Iterable[String] = null, initialBag: Map[String, Double] = null) extends BagOfWords {
  var variable: BagOfWordsVariable = null
  protected var _l2Norm = 0.0
  protected var _l1Norm = 0.0
  protected var _bag = new LinkedHashMap[String, Double]
  //TODO: try LinkedHashMap
  /*
  def buildFrom(_bag:HashMap[String,Double], _l1Norm:Double, _l2Norm:Double) ={
    this._bag = _bag
    this._l1Norm= _l1Norm
    this._l2Norm= _l2Norm
  }*/
  def clear(): Unit = {
    _l2Norm = 0.0
    _l1Norm = 0.0
    _bag = new LinkedHashMap[String, Double]
  }
  //def underlying = _bag
  def sizeHint(n: Int) = _bag.sizeHint(n)
  if (initialWords != null) for (w <- initialWords) this +=(w, 1.0)
  if (initialBag != null) for ((k, v) <- initialBag) this +=(k, v)
  def l2Norm = scala.math.sqrt(_l2Norm)
  def l1Norm = _l1Norm
  def asHashMap: HashMap[String, Double] = {
    val result = new HashMap[String, Double]; result ++= _bag; result
  }
  override def toString = _bag.toString()
  def apply(s: String): Double = _bag.getOrElse(s, 0.0)
  def contains(s: String): Boolean = _bag.contains(s)
  def size = _bag.size
  def iterator = _bag.iterator
  def *(that: BagOfWords): Double = {
    if (that.size < this.size) return that * this
    var result = 0.0
    for ((k, v) <- iterator) result += v * that(k)
    result
  }
  def deductedDot(that: BagOfWords, deduct: BagOfWords): Double = {
    var result = 0.0
    if (deduct eq this) for ((k, v) <- iterator) result += v * (that(k) - v)
    else for ((k, v) <- iterator) result += v * (that(k) - deduct(k))
    result
  }
  /*
  override def ++=(that:BagOfWords):Unit = for((s,w) <- that.iterator){
    _bag.sizeHint(this.size+that.size)
    this += (s,w)
  }*/
  //def --=(that:BagOfWords):Unit = for((s,w) <- that.iterator)this -= (s,w)
  def +=(s: String, w: Double = 1.0): Unit = {
    if (w != 0.0) {
      //if(w!=1.0)println("  add: "+w)
      _l1Norm += w
      _l2Norm += w * w + 2 * this(s) * w
      _bag(s) = _bag.getOrElse(s, 0.0) + w
    }
  }
  def -=(s: String, w: Double = 1.0): Unit = {
    if (w != 0.0) {
      _l1Norm -= w
      _l2Norm += w * w - 2.0 * this(s) * w
      //if(w!=1.0)println("  remove: "+w)
      if (withinEpsilon(w, _bag(s))) _bag.remove(s)
      else _bag(s) = _bag.getOrElse(s, 0.0) - w
    }
  }
  @inline final def withinEpsilon(v1: Double, v2: Double, epsilon: Double = 0.000001): Boolean = if (v1 == v2) true else math.abs(v1 - v2) <= epsilon
  def addBag(that: BagOfWords) = {
    //that match{case t:SparseBagOfWords=>t.sizeHint(this.size+that.size)}
    for ((k, v) <- that.iterator) this +=(k, v)
  }
  def removeBag(that: BagOfWords) = for ((k, v) <- that.iterator) this -=(k, v)
}

trait BagOfWordsVar extends Var with Iterable[(String, Double)] {
  type Value <: SparseBagOfWords
}
class BagOfWordsVariable(initialWords: Iterable[String] = Nil, initialMap: Map[String, Double] = null) extends BagOfWordsVar /*with VarAndValueGenericDomain[BagOfWordsVariable,SparseBagOfWords]*/ {
  // Note that the returned value is not immutable.
  type Value = SparseBagOfWords
  def value = _members
  def clear() = _members.clear
  protected val _members: SparseBagOfWords = {
    val result = new SparseBagOfWords(initialWords)
    if (initialMap != null) for ((k, v) <- initialMap) result +=(k, v)
    result.variable = this
    result
  }
  def members: SparseBagOfWords = _members
  def iterator = _members.iterator
  override def size = _members.size
  def contains(x: String) = _members.contains(x)
  def accept(): Unit = {}
  //_members.incorporateBags
  def add(x: String, w: Double = 1.0)(implicit d: DiffList): Unit = {
    if (d != null) d += new BagOfWordsVariableAddStringDiff(x, w)
    _members +=(x, w)
  }
  def remove(x: String, w: Double = 1.0)(implicit d: DiffList): Unit = {
    if (d != null) d += new BagOfWordsVariableRemoveStringDiff(x, w)
    _members -=(x, w)
  }
  def add(x: BagOfWords)(implicit d: DiffList): Unit = {
    if (d != null) d += new BagOfWordsVariableAddBagDiff(x)
    _members.addBag(x)
  }
  def remove(x: BagOfWords)(implicit d: DiffList): Unit = {
    if (d != null) d += new BagOfWordsVariableRemoveBagDiff(x)
    _members.removeBag(x)
  }
  final def +=(x: String, w: Double = 1.0): Unit = add(x, w)(null)
  final def -=(x: String, w: Double = 1.0): Unit = remove(x, w)(null)
  final def +=(x: BagOfWords): Unit = add(x)(null)
  final def -=(x: BagOfWords): Unit = remove(x)(null)
  final def ++=(xs: Iterable[String]): Unit = xs.foreach(add(_)(null))
  final def --=(xs: Iterable[String]): Unit = xs.foreach(remove(_)(null))
  final def ++=(xs: HashMap[String, Double]): Unit = for ((k, v) <- xs) add(k, v)(null)
  final def --=(xs: HashMap[String, Double]): Unit = for ((k, v) <- xs) remove(k, v)(null)
  case class BagOfWordsVariableAddStringDiff(added: String, w: Double) extends Diff {
    // Console.println ("new SetVariableAddDiff added="+added)
    def variable: BagOfWordsVariable = BagOfWordsVariable.this
    def redo() = _members +=(added, w)
    def undo() = _members -=(added, w)
    override def toString = "BagOfWordsVariableAddStringDiff of " + added + " to " + BagOfWordsVariable.this
  }
  case class BagOfWordsVariableRemoveStringDiff(removed: String, w: Double) extends Diff {
    //        Console.println ("new SetVariableRemoveDiff removed="+removed)
    def variable: BagOfWordsVariable = BagOfWordsVariable.this
    def redo() = _members -=(removed, w)
    def undo() = _members +=(removed, w)
    override def toString = "BagOfWordsVariableRemoveStringDiff of " + removed + " from " + BagOfWordsVariable.this
  }
  case class BagOfWordsVariableAddBagDiff(added: BagOfWords) extends Diff {
    // Console.println ("new SetVariableAddDiff added="+added)
    def variable: BagOfWordsVariable = BagOfWordsVariable.this
    def redo() = _members.addBag(added)
    def undo() = _members.removeBag(added)
    override def toString = "BagOfWordsVariableAddBagDiff of " + added + " to " + BagOfWordsVariable.this
  }
  case class BagOfWordsVariableRemoveBagDiff(removed: BagOfWords) extends Diff {
    //        Console.println ("new SetVariableRemoveDiff removed="+removed)
    def variable: BagOfWordsVariable = BagOfWordsVariable.this
    def redo() = _members.removeBag(removed)
    def undo() = _members.addBag(removed)
    override def toString = "BagOfWordsVariableRemoveBagDiff of " + removed + " from " + BagOfWordsVariable.this
  }
}
