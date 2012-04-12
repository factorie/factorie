package cc.factorie.app.bib
import cc.factorie._
import cc.factorie.app.nlp.coref._
import db.mongo._
import scala.collection.mutable.{HashMap, ArrayBuffer}
import com.mongodb.{DB, Mongo}

class AuthorFLNameCanopy(val entity:AuthorEntity) extends CanopyAttribute[AuthorEntity] {
  def canopyName:String=(initial(entity.entityRoot.asInstanceOf[AuthorEntity].fullName.firstName)+entity.entityRoot.asInstanceOf[AuthorEntity].fullName.lastName).toLowerCase
  //def canopyName:String=(initial(entity.fullName.firstName)+entity.fullName.lastName).toLowerCase
  def initial(s:String):String = if(s!=null && s.length>0)s.substring(0,1) else ""
}
class PaperTitleCanopy(val entity:PaperEntity) extends CanopyAttribute[PaperEntity]{
  def cleanTitle(s:String) = s.toLowerCase.replaceAll("[^a-z0-9 ]","").replaceAll(" +"," ")
  def canopyName:String = cleanTitle(entity.entityRoot.asInstanceOf[PaperEntity].title.value)
}
/**Attributes specific to REXA authors*/
class FullName(val entity:Entity,f:String,m:String,l:String,su:String=null) extends SeqVariable[String](Seq(f,m,l,su)) with EntityAttr {
  def setFirst(s:String)(implicit d:DiffList) = update(0,s)
  def setMiddle(s:String)(implicit d:DiffList) = update(1,s)
  def setLast(s:String)(implicit d:DiffList) = update(2,s)
  def setSuffix(s:String)(implicit d:DiffList) = update(3,s)
  def setFullName(that:FullName)(implicit d:DiffList) = {
    if(firstName!=that.firstName)setFirst(that.firstName)(null)
    if(middleName!=that.middleName)setMiddle(that.middleName)(null)
    if(lastName!=that.lastName)setLast(that.lastName)(null)
  }
  def firstName = value(0)
  def middleName = value(1)
  def lastName = value(2)
  def suffix = value(3)
  def domain = GenericDomain
  override def toString:String = {
    val result = new StringBuffer
    if(firstName!=null && firstName.length>0)result.append(firstName+" ")
    if(middleName!=null && middleName.length>0)result.append(middleName+" ")
    if(lastName!=null && lastName.length>0)result.append(lastName+" ")
    if(suffix!=null && suffix.length>0)result.append(suffix)
    result.toString.trim
  }
}
class Title(val entity:Entity,title:String) extends StringVariable(title) with EntityAttr
class Year(val entity:Entity,year:Int) extends IntegerVariable(year) with EntityAttr
class VenueName(val entity:Entity,venueName:String) extends StringVariable(venueName) with EntityAttr
class BagOfTopics(val entity:Entity, topicBag:Map[String,Double]=null) extends BagOfWordsVariable(Nil, topicBag) with EntityAttr
class BagOfVenues(val entity:Entity, venues:Map[String,Double]=null) extends BagOfWordsVariable(Nil, venues) with EntityAttr
class BagOfCoAuthors(val entity:Entity,coAuthors:Map[String,Double]=null) extends BagOfWordsVariable(Nil, coAuthors) with EntityAttr
class BagOfKeywords(val entity:Entity,keywords:Map[String,Double]=null) extends BagOfWordsVariable(Nil,keywords) with EntityAttr
class BagOfEmails(val entity:Entity,keywords:Map[String,Double]=null) extends BagOfWordsVariable(Nil,keywords) with EntityAttr
/**Entity variables*/
/**An entity with the necessary variables/coordination to implement hierarchical coreference.*/

class PaperEntity(s:String="DEFAULT",isMention:Boolean=false) extends HierEntity(isMention) with HasCanopyAttributes[PaperEntity] with Prioritizable{
  attr += new Title(this,s)
  attr += new Year(this,-1)
  attr += new VenueName(this,"")
  def title = attr[Title]
  def year = attr[Year]
  def venueName = attr[VenueName]
  def string = title.toString
  var authors = new ArrayBuffer[AuthorEntity]
  def propagateAddBagsUp()(implicit d:DiffList):Unit = {throw new Exception("not implemented")}
  def propagateRemoveBagsUp()(implicit d:DiffList):Unit = {throw new Exception("not implemented")}
}
class AuthorEntity(f:String="DEFAULT",m:String="DEFAULT",l:String="DEFAULT", isMention:Boolean = false) extends HierEntity(isMention) with HasCanopyAttributes[AuthorEntity] with Prioritizable{
  var _id = java.util.UUID.randomUUID.toString+""
  override def id = _id
  priority = scala.math.exp(random.nextDouble)
  canopyAttributes += new AuthorFLNameCanopy(this)
  attr += new FullName(this,f,m,l)
  attr += new BagOfTopics(this)
  attr += new BagOfVenues(this)
  attr += new BagOfCoAuthors(this)
  attr += new BagOfKeywords(this)
  def fullName = attr[FullName]
  def bagOfTopics = attr[BagOfTopics]
  def bagOfVenues = attr[BagOfVenues]
  def bagOfCoAuthors = attr[BagOfCoAuthors]
  def bagOfKeywords = attr[BagOfKeywords]
  def string = f+" "+m+" "+l
  var paper:PaperEntity = null
  def defaultCanopy = canopyAttributes.head.canopyName
}

/**Handles loading/storing issues generic to entity collections with canopies, ids, priorities, and hierarchical structure*/
trait EntityCollection[E<:Entity with HasCanopyAttributes[E] with Prioritizable, C<:EntityCubbie]{
  private var _id2cubbie:HashMap[Any,C] = null
  protected def newEntityCubbie:C
  protected def newEntity:E
  protected def fetchFromCubbie(c:C,e:E):Unit
  protected def register(entityCubbie:C) = _id2cubbie += entityCubbie.id->entityCubbie
  protected def wasLoadedFromDB(entity:E) = _id2cubbie.contains(entity.id)
  protected def putEntityInCubbie(e:E) = _id2cubbie(e.id)
  protected def entityCubbieColl:MutableCubbieCollection[C]
  protected def changePriority(e:E):Unit =e.priority = scala.math.exp(e.priority - random.nextDouble)
  implicit def entity2cubbie(e:E):C = {val ec=newEntityCubbie;ec.store(e);ec}
  def finishLoadAndReturnEntities(entityCubbies:Iterable[C]) = {
    for(cubbie <- entityCubbies){
      val entity:E = newEntity
      fetchFromCubbie(cubbie,entity) 
      register(cubbie)
    }
  }
  def reset:Unit ={
    _id2cubbie = new HashMap[Any,C]
  }
  def store(entitiesToStore:Iterable[E]):Unit ={
    val deleted = new ArrayBuffer[E]
    val updated = new ArrayBuffer[E]
    val created = new ArrayBuffer[E]
    for(e <- updated ++ created)changePriority(e)
    for(e<-entitiesToStore){
      if(e.isConnected){
        if(wasLoadedFromDB(e))updated += e
        else created += e
      }
      if(!e.isConnected && wasLoadedFromDB(e))deleted += e
      //else if(!e.isConnected && wasLoadedFromDB(e))entityCubbieColl += {val ec=newEntityCubbie;ec.store(e);ec}
    }
    removeEntities(deleted)
    for(e <- updated)entityCubbieColl.updateDelta(_id2cubbie.getOrElse(e.id,null).asInstanceOf[C],putEntityInCubbie(e))
    for(e <- created)entityCubbieColl += e
  }
  /**This is database specific for example because mongo has a specialized _id field*/
  protected def removeEntities(entitiesToRemove:Seq[E]):Unit
  def assembleEntities(toAssemble:Seq[C],id2entity:Any=>E,cubbie2entity:C=>E):Unit ={
    for(c<-toAssemble){
      val child = cubbie2entity(c)
      val parent = if(c.parentRef.isDefined)id2entity(c.parentRef.value) else null.asInstanceOf[E]
      child.setParentEntity(parent)(null)
    }
  }
}
abstract class MongoEntityCollection[E<:Entity with HasCanopyAttributes[E] with Prioritizable,C<:EntityCubbie](val name:String, mongoDB:DB) extends EntityCollection[E,C]{
  import MongoCubbieImplicits._
  import MongoCubbieConverter._
  protected val entityColl = mongoDB.getCollection(name)
  protected val entityCubbieColl = new MongoCubbieCollection(entityColl,() => newEntityCubbie,(a:C) => Seq(Seq(a.canopies),Seq(a.inferencePriority),Seq(a.parentRef))) with LazyCubbieConverter[C]
  protected def removeEntities(deleted:Seq[E]):Unit = for(e <- deleted)entityCubbieColl.remove(_.idIs(entity2cubbie(e).id))
}
trait BibDatabase{
  def authorColl:EntityCollection[AuthorEntity,AuthorCubbie]
  def paperColl:EntityCollection[PaperEntity,PaperCubbie]
}
abstract class BibMongoDatabase(mongoServer:String="localhost",mongoPort:Int=27017,mongoDBName:String="rexa2-cubbie") extends BibDatabase{
  import MongoCubbieImplicits._
  import MongoCubbieConverter._
  protected val mongoConn = new Mongo(mongoServer,mongoPort)
  protected val mongoDB = mongoConn.getDB(mongoDBName)
  val authorColl = new MongoEntityCollection[AuthorEntity,AuthorCubbie]("authors",mongoDB){
    def newEntityCubbie:AuthorCubbie = new AuthorCubbie
    def newEntity:AuthorEntity = new AuthorEntity
    def fetchFromCubbie(authorCubbie:AuthorCubbie,author:AuthorEntity):Unit = authorCubbie.fetch(author)
  }
  val paperColl = new MongoEntityCollection[PaperEntity,PaperCubbie]("papers",mongoDB){
    def newEntityCubbie:PaperCubbie = new PaperCubbie
    def newEntity:PaperEntity = new PaperEntity
    def fetchFromCubbie(paperCubbie:PaperCubbie,paper:PaperEntity):Unit = paperCubbie.fetch(paper)
  }
}

/**Basic trait for doing operations with bags of words*/
trait BagOfWords{ // extends scala.collection.Map[String,Double]{
  //def empty: This
  def size:Int
  def asHashMap:HashMap[String,Double]
  def apply(word:String):Double
  def iterator:Iterator[(String,Double)]
  def l2Norm:Double
  def l1Norm:Double
  def *(that:BagOfWords):Double
  def cosineSimilarity(that:BagOfWords,deduct:BagOfWords):Double ={
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
  def cosineSimilarity(that:BagOfWords):Double = {
    val numerator:Double = this * that
    val denominator:Double = this.l2Norm*that.l2Norm
    if(denominator==0.0 || denominator != denominator) 0.0 else numerator/denominator
  }
  def +=(s:String,w:Double=1.0):Unit
  def -=(s:String,w:Double=1.0):Unit
  def ++=(that:BagOfWords):Unit = for((s,w) <- that.iterator)this += (s,w)
  def --=(that:BagOfWords):Unit = for((s,w) <- that.iterator)this -= (s,w)
  def contains(s:String):Boolean
  def l2NormBruteForce:Double = {
    var result=0.0
    for((k,v) <- iterator)
      result += v*v
    scala.math.sqrt(result)
  }
  def addBag(that:BagOfWords):Unit
  def removeBag(that:BagOfWords):Unit
}

class SparseBagOfWords(initialWords:Iterable[String]=null,initialBag:Map[String,Double]=null) extends BagOfWords{
  protected var _l2Norm = 0.0
  protected var _l1Norm = 0.0
  protected var _bag = new HashMap[String,Double]
  if(initialWords!=null)for(w<-initialWords)this += (w,1.0)
  if(initialBag!=null)for((k,v)<-initialBag)this += (k,v)
  def l2Norm = _l2Norm
  def l1Norm = _l1Norm
  def asHashMap:HashMap[String,Double] = {val result = new HashMap[String,Double];result ++= _bag;result}
  override def toString = _bag.toString
  def apply(s:String):Double = _bag.getOrElse(s,0.0)
  def contains(s:String):Boolean = _bag.contains(s)
  def size = _bag.size
  def iterator = _bag.iterator
  def *(that:BagOfWords) : Double = {
    if(that.size<this.size)return that * this
    var result = 0.0
    for((k,v) <- iterator)result += v*that(k)
    result
  }
  def += (s:String, w:Double=1.0):Unit ={
    _l1Norm += w
    _l2Norm += w*w + 2*this(s)*w
    _bag(s) = _bag.getOrElse(s,0.0) + w
  }
  def -= (s:String, w:Double=1.0):Unit ={
    _l1Norm -= w
    _l2Norm += w*w - 2.0*this(s)*w
    if(w == _bag(s))_bag.remove(s)
    else _bag(s) = _bag.getOrElse(s,0.0) - w
  }
  def addBag(that:BagOfWords) = for((k,v) <- that.iterator) this += (k,v)
  def removeBag(that:BagOfWords) = for((k,v) <- that.iterator)this -= (k,v)
}
trait BagOfWordsVar extends Variable with VarAndValueGenericDomain[BagOfWordsVar,SparseBagOfWords] with Iterable[(String,Double)]
class BagOfWordsVariable(initialWords:Iterable[String]=Nil,initialMap:Map[String,Double]=null) extends BagOfWordsVar with VarAndValueGenericDomain[BagOfWordsVariable,SparseBagOfWords] {
  // Note that the returned value is not immutable.
  def value = _members
  private val _members:SparseBagOfWords = {
    val result = new SparseBagOfWords(initialWords)
    if(initialMap!=null)for((k,v) <- initialMap)result += (k,v)
    result
  }
  def members: SparseBagOfWords = _members
  def iterator = _members.iterator
  override def size = _members.size
  def contains(x:String) = _members.contains(x)
  def accept:Unit ={} //_members.incorporateBags
  def add(x:String,w:Double=1.0)(implicit d:DiffList):Unit = {
    if(d!=null) d += new BagOfWordsVariableAddStringDiff(x,w)
    _members += (x,w)
  }
  def remove(x:String,w:Double = 1.0)(implicit d:DiffList):Unit = {
    if(d!=null) d += new BagOfWordsVariableRemoveStringDiff(x,w)
    _members -= (x,w)
  }
  def add(x:BagOfWords)(implicit d: DiffList): Unit =  {
    if (d != null) d += new BagOfWordsVariableAddBagDiff(x)
    _members.addBag(x)
  }
  def remove(x: BagOfWords)(implicit d: DiffList): Unit = {
    if (d != null) d += new BagOfWordsVariableRemoveBagDiff(x)
    _members.removeBag(x)
  }
  final def += (x:String,w:Double=1.0):Unit = add(x,w)(null)
  final def -= (x:String,w:Double=1.0):Unit = remove(x,w)(null)
  final def +=(x:BagOfWords): Unit = add(x)(null)
  final def -=(x:BagOfWords): Unit = remove(x)(null)
  final def ++=(xs:Iterable[String]): Unit = xs.foreach(add(_)(null))
  final def --=(xs:Iterable[String]): Unit = xs.foreach(remove(_)(null))
  final def ++=(xs:HashMap[String,Double]): Unit = for((k,v)<-xs)add(k,v)(null)
  final def --=(xs:HashMap[String,Double]): Unit = for((k,v)<-xs)remove(k,v)(null)
  case class BagOfWordsVariableAddStringDiff(added: String,w:Double) extends Diff {
    // Console.println ("new SetVariableAddDiff added="+added)
    def variable: BagOfWordsVariable = BagOfWordsVariable.this
    def redo = _members += (added,w)
    def undo = _members -= (added,w)
    override def toString = "BagOfWordsVariableAddStringDiff of " + added + " to " + BagOfWordsVariable.this
  }
  case class BagOfWordsVariableRemoveStringDiff(removed: String,w:Double) extends Diff {
    //        Console.println ("new SetVariableRemoveDiff removed="+removed)
    def variable: BagOfWordsVariable = BagOfWordsVariable.this
    def redo = _members -= (removed,w)
    def undo = _members += (removed,w)
    override def toString = "BagOfWordsVariableRemoveStringDiff of " + removed + " from " + BagOfWordsVariable.this
  }
  case class BagOfWordsVariableAddBagDiff(added:BagOfWords) extends Diff {
    // Console.println ("new SetVariableAddDiff added="+added)
    def variable: BagOfWordsVariable = BagOfWordsVariable.this
    def redo = _members.addBag(added)
    def undo = _members.removeBag(added)
    override def toString = "BagOfWordsVariableAddBagDiff of " + added + " to " + BagOfWordsVariable.this
  }
  case class BagOfWordsVariableRemoveBagDiff(removed: BagOfWords) extends Diff {
    //        Console.println ("new SetVariableRemoveDiff removed="+removed)
    def variable: BagOfWordsVariable = BagOfWordsVariable.this
    def redo = _members.removeBag(removed)
    def undo = _members.addBag(removed)
    override def toString = "BagOfWordsVariableRemoveBagDiff of " + removed + " from " + BagOfWordsVariable.this
  }
}
