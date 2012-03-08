package cc.factorie.example

import cc.factorie.app.nlp.coref._
import scala.collection.mutable.{HashSet,HashMap,ArrayBuffer}
import cc.factorie._
import cc.factorie.app.nlp.coref._
import cc.factorie.db.mongo._
import com.mongodb.{BasicDBList, BasicDBObject, DBCursor, DBObject, DBCollection, Mongo}
import cc.factorie.util.CubbieRefs
//import example.Coref3.MyEntityCubbie

/**
 * Consider a set-up where cosine distance is computed between arrays of bags of words, and each bag is a variable that can go on the diff list.
 */

object Coref3 {
  /**Attributes/Features of entities*/
  class EntityExists(val entity:Entity,initialValue:Boolean) extends BooleanVariable(initialValue)
  class IsEntity(val entity:Entity,initialValue:Boolean) extends BooleanVariable(initialValue)
  class IsMention(val entity:Entity,initialValue:Boolean) extends BooleanVariable(initialValue)
  class Bow(val entity:Entity) extends SetVariable[String] {
    def this(e:Entity, ss:Iterable[String]) = { this(e); ss.foreach(this.add(_)(null)) }
  }
  class TrueBow(val entity:Entity,ss:Iterable[String]=Nil) extends BagOfWordsVariable(ss)
  /**Entity variables*/
  var nextId = -1
  var entityCount = 0
  class MyEntity(s:String, b:Iterable[String] = Nil, isMention:Boolean=false) extends NamedEntity(s){
    override val id = { entityCount += 1; entityCount } //new IntCubbie(id)
    //val idCubbie = new IntSlot
    isObserved=isMention
    attr += new Bow(this, b)
    attr += new TrueBow(this,b)
    attr += new EntityExists(this,this.isConnected)
    attr += new IsEntity(this,this.isRoot)
    attr += new IsMention(this,this.isObserved)
    removedChildHooks += ((removed:Entity, d:DiffList) => {attr[EntityExists].set(this.isConnected)(d)})
    addedChildHooks += ((added:Entity, d:DiffList) => {attr[EntityExists].set(this.isConnected)(d)})
    changedSuperEntityHooks += ((oldSuper:Entity,newSuper:Entity, d:DiffList) => {attr[IsEntity].set(this.isRoot)(d);attr[EntityExists].set(this.isConnected)(d);})
    /**Should only be called once context of the entire tree this entity belongs to has been connected together.*/
    override def initializeAttributesOfStructure:Unit={
      attr[EntityExists].set(this.isConnected)(null)
      attr[IsEntity].set(this.isRoot)(null)
    }
  }
  // Cubbie storage
  class MyEntityCubbie extends NamedEntityCubbie {
    _map = new HashMap[String,Any]
    val bow = StringListSlot("bow")
    val bowVar = CubbieSlot("bowVar", () => new BagOfWordsCubbie)
    val isMention = BooleanSlot("isMention")
    //val name = StringSlot("name")
    /**TODO: where to set these? They are determined by the graph structure which is incomplete at the time this cubbie is loaded.
    attr += new EntityExists(this,this.isConnected)
    attr += new IsEntity(this,this.isRoot)
    attr += new IsMention(this,this.isObserved)
   */
    override def newEntity:NamedEntity = new MyEntity("UNINITIALIZED")
    override def newEntityCubbie:NamedEntityCubbie = new NamedEntityCubbie
    override def finishStoreEntity(e:Entity): Unit = {
      super.finishStoreEntity(e)
      bow := e.attr[Bow].value.toSeq
      bowVar := new BagOfWordsCubbie().store(e.attr[TrueBow].value)
      isMention := e.attr[IsMention].booleanValue
    }
    override def finishFetchEntity(e:Entity): Unit = {
      super.finishFetchEntity(e)
      //println("Beig called twice? "+e.id)
      //e.attr += new Bow(e, Nil) //this is necessary because e is an Entity not a "MyEntity"
      e.attr[EntityName].set(name.value)(null)
      e.attr[Bow] ++= bow.value
      if(e.attr[TrueBow].size==0)e.attr[TrueBow] ++= bowVar.value.fetch
      e.attr[IsMention].set(isMention.value)(null)
      e.isObserved=isMention.value
    }
  }
  class BagOfWordsCubbie extends Cubbie{
    _map = new HashMap[String,Any]
    val words = StringListSlot("words")
    val weights = DoubleListSlot("weights")
    def store(bag:BagOfWords):BagOfWordsCubbie ={
      words := bag.iterator.map(_._1).toSeq
      weights := bag.iterator.map(_._2).toSeq
      this
    }
    def fetch:HashMap[String,Double] ={
      //val result = new BagOfWordsVariable
      val result = new HashMap[String,Double]
      val wordSeq = words.value
      val weightSeq = weights.value
      println("WORDS: "+words)
      println("WEIGHTS: "+weights)
      for(i<-0 until wordSeq.size)result += wordSeq(i) -> weightSeq(i)
      println("RESULT: "+result)
      result
    }
  }

  class HierCorefModel extends TemplateModel(
    new TemplateWithStatistics3[EntityRef,EntityName,EntityName] {
      def unroll1(er:EntityRef) = if(er.dst!=null)Factor(er, er.src.attr[EntityName], er.dst.attr[EntityName]) else Nil
      def unroll2(childName:EntityName) = Factor(childName.entity.superEntityRef, childName, childName.entity.superEntity.attr[EntityName])
      def unroll3(parentName:EntityName) = for (e <- parentName.entity.subEntities) yield Factor(e.superEntityRef, e.attr[EntityName], parentName)
      def score(s:Stat): Double = {
        val childString = s._2
        val parentString = s._3
        var result = -cc.factorie.app.strings.editDistance(childString, parentString)
        result
      }
    },
    // compatibility between parent/child bows
  /*
    new TemplateWithStatistics3[EntityRef,Bow,Bow] {
      def unroll1(er:EntityRef) = if(er.dst!=null)Factor(er, er.src.attr[Bow], er.dst.attr[Bow]) else Nil
      def unroll2(childBow:Bow) = Factor(childBow.entity.superEntityRef, childBow, childBow.entity.superEntity.attr[Bow])
      def unroll3(parentBow:Bow) = for (e <- parentBow.entity.subEntities) yield Factor(e.superEntityRef, e.attr[Bow], parentBow)
      def score(s:Stat): Double = {
        val childBow = s._2
        val parentBow = s._3
        childBow.intersect(parentBow).size
      }
    },
    */
    new TemplateWithStatistics3[EntityRef,TrueBow,TrueBow] {
      def unroll1(er:EntityRef) = if(er.dst!=null)Factor(er, er.src.attr[TrueBow], er.dst.attr[TrueBow]) else Nil
      def unroll2(childBow:TrueBow) = if(childBow.entity.superEntity!=null)Factor(childBow.entity.superEntityRef, childBow, childBow.entity.superEntity.attr[TrueBow]) else Nil
      def unroll3(parentBow:TrueBow) = for(e<-parentBow.entity.subEntities) yield Factor(e.superEntityRef,e.attr[TrueBow],parentBow)
      def score(s:Stat): Double = {
        val childBow = s._2
        val parentBow = s._3
        //childBow.intersect(parentBow).size
        var result = childBow.cosineSimilarity(parentBow)
        println("result: "+result)
        println("  bag1:"+childBow)
        println("  bag2:"+parentBow)
        result
      }
    },


    //structural priors
    new TemplateWithStatistics3[EntityExists,IsEntity,IsMention]{
      val entityExistenceCost = 8.0
      val subEntityExistenceCost = 0.5
      def unroll1(exists:EntityExists) = Factor(exists,exists.entity.attr[IsEntity],exists.entity.attr[IsMention])
      def unroll2(isEntity:IsEntity) = Factor(isEntity.entity.attr[EntityExists],isEntity,isEntity.entity.attr[IsMention])
      def unroll3(isMention:IsMention) = throw new Exception("An entitie's status as a mention should never change.")
      //Factor(isMention.entity.attr[EntityExists],isMention.entity.attr[IsEntity],isMention)
      def score(s:Stat):Double ={
        val exists:Boolean = s._1.booleanValue
        val isEntity:Boolean = s._2.booleanValue
        val isMention:Boolean = s._3.booleanValue
        var result = 0.0
        if(exists && isEntity) result -= entityExistenceCost
        if(exists && !isEntity && !isMention)result -= subEntityExistenceCost
        result
      }
    }
  )
  class HierarchicalCorefSampler(model:HierCorefModel) extends SettingsSampler[Null](model, null) {
    protected var entities:ArrayBuffer[Entity] = null
    protected var deletedEntities:ArrayBuffer[Entity] = null
    def getEntities = entities.filter(_.isConnected)
    def getDeletedEntities = {
      performMaintenance
      deletedEntities
    }
    def infer(numSamples:Int):Unit ={}
    /**Returns a random entity that 'exists'*/
    def nextEntity:Entity={
      var tries = 4
      var e:Entity = null
      while({tries-= 1;tries} >= 0 && (e==null || !e.isConnected)){e = entities(random.nextInt(entities.size));if(tries==1)performMaintenance}
      e
    }
    def setEntities(ents:Iterable[Entity]) = {entities = new ArrayBuffer[Entity];entities ++= ents;deletedEntities = new ArrayBuffer[Entity]}
    /**Garbage collects all the deleted entities from the master list of entities*/
    def performMaintenance:Unit = {val cleanEntities = new ArrayBuffer[Entity];cleanEntities ++= entities.filter(_.isConnected);deletedEntities ++= entities.filter(!_.isConnected);entities=cleanEntities}
    /**This function randomly generates a list of jumps/proposals to choose from.*/
    def settings(c:Null) : SettingIterator = new SettingIterator {
      val changes = new scala.collection.mutable.ArrayBuffer[(DiffList)=>Unit];
      val entity1 = nextEntity
      val entity2 = nextEntity
      if (entity1.entityRoot.id != entity2.entityRoot.id) { //sampled nodes refer to different entities
        if(!isMention(entity1)){
          changes += {(d:DiffList) => mergeLeft(entity1,entity2)(d)} //what if entity2 is a mention?
          if(entity1.id != entity1.entityRoot.id) //avoid adding the same jump to the list twice
            changes += {(d:DiffList) => mergeLeft(entity1.entityRoot,entity2)(d)}
        }
        if(entity1.superEntity==null && entity2.superEntity==null)
          changes += {(d:DiffList) => mergeUp(entity1,entity2)(d)}
      } else { //sampled nodes refer to same entity
        changes += {(d:DiffList) => splitRight(entity1,entity2)(d)}
        changes += {(d:DiffList) => splitRight(entity2,entity1)(d)}
        if(entity1.superEntity != null && !entity1.isObserved)
          changes += {(d:DiffList) => {collapse(entity1)(d)}}
      }
      changes += {(d:DiffList) => {}} //give the sampler an option to reject all other proposals
      var i = 0
      def hasNext = i < changes.length
      def next(d:DiffList) = { val d = new DiffList; changes(i).apply(d); i += 1; d }
      def reset = i = 0
    }
    /**Removes an intermediate node in the tree, merging that nodes children to their grandparent.*/
    def collapse(entity:Entity)(implicit d:DiffList):Unit ={
      if(entity.superEntity==null)throw new Exception("Can't collapse a node that is the root of a tree.")
      //println("Collapse")
      //println("  e.children="+entity.subEntitiesSize)
      val oldParent = entity.superEntity
      entity.subEntitiesIterator.foreach(_.setSuperEntity(entity.superEntity)(d))
      entity.setSuperEntity(null)(d)
     // println("  super.size="+oldParent.subEntitiesSize)
    }
    /**Peels off the entity "right", does not really need both arguments unless we want to error check.*/
    def splitRight(left:Entity,right:Entity)(implicit d:DiffList):Unit ={
      val oldParent = right.superEntity
      right.setSuperEntity(null)(d)
      propagateRemoveBag(right,oldParent)(d)
      structurePreservationForEntityThatLostSubEntity(oldParent)(d)
    }
    /**Jump function that proposes merge: entity1<----entity2*/
    def mergeLeft(entity1:Entity,entity2:Entity)(implicit d:DiffList):Unit ={
      val oldParent = entity2.superEntity
      entity2.setSuperEntity(entity1)(d)
      propagateBagUp(entity2)(d)
      propagateRemoveBag(entity2,oldParent)(d)
      structurePreservationForEntityThatLostSubEntity(oldParent)(d)
    }
    /**Jump function that proposes merge: entity1--->NEW-SUPER-ENTITY<---entity2 */
    def mergeUp(e1:Entity,e2:Entity)(implicit d:DiffList):Entity = {
      val name = e1.attr[EntityName].value
      val result = new MyEntity(name)
      val oldParent1 = e1.superEntity
      val oldParent2 = e2.superEntity
      e1.setSuperEntity(result)(d)
      e2.setSuperEntity(result)(d)
      result.attr[TrueBow].add(e1.attr[TrueBow].value)(d)
      result.attr[TrueBow].add(e2.attr[TrueBow].value)(d)
      propagateRemoveBag(e1,oldParent1)(d)
      propagateRemoveBag(e2,oldParent2)(d)
      structurePreservationForEntityThatLostSubEntity(oldParent1)(d)
      structurePreservationForEntityThatLostSubEntity(oldParent2)(d)
      result
    }
    /**Ensure that chains are not created in our tree. No dangling sub-entities either.*/
    def structurePreservationForEntityThatLostSubEntity(e:Entity)(implicit d:DiffList):Unit ={
      if(e!=null && e.subEntitiesSize<=1){
        for(subEntity <- e.subEntities)
          subEntity.setSuperEntity(e.superEntity)
        e.setSuperEntity(null)(d)
      }
    }
    def propagateBagUp(entity:Entity)(implicit d:DiffList):Unit ={
      var e = entity.superEntity
      while(e!=null){
        e.attr[TrueBow].add(entity.attr[TrueBow].value)(d)
        e = e.superEntity
      }
    }
    def propagateRemoveBag(parting:Entity,formerParent:Entity)(implicit d:DiffList):Unit ={
      var e = formerParent
      while(e!=null){
        e.attr[TrueBow].remove(parting.attr[TrueBow].value)
        e = e.superEntity
      }
    }
    /**Identify entities that are created by accepted jumps so we can add them to our master entity list.*/
    override def proposalHook(proposal:Proposal) = {
      val newEntities = new HashSet[Entity]
      proposal.diff.undo //an entity that does not exit in the current world is one that was newly created by the jump
      for(diff<-proposal.diff){
        diff.variable match{
          case sub:SubEntities => if(!sub.entity.isConnected)newEntities += sub.entity
          case _ => {}
        }
      }
      proposal.diff.redo
      for(diff<-proposal.diff){
        diff.variable match{
          case bag:TrueBow => bag.accept
          case _ => {}
        }
      }
      entities ++= newEntities
      if(proposal.diff.size>0){
        println("\nNEW WORLD")
        printEntities(entities)
      }
    }
  }
  def isMention(e:Entity):Boolean = e.isObserved



/*
  def testBags:Unit = {
    val d = new DiffList
    val bag1 = new BagOfWordsHashMap(Seq("ICML", "NIPS", "UAI", "AAAI", "KDD"))
    val bag2 = new BagOfWordsHashMap(Seq("ACL","EMNLP","HLT","KDD"))
    val bagVar = new BagOfWordsVariable[BagOfWordsProposeAndDecide]
    bagVar.add(bag1)(d)
    bagVar.add(bag2)(d)

    printBag(bag1,"Bag1")
    printBag(bag2,"Bag2")
    printBag(bagVar.value,"BagCombineDo")
    d.undo
    printBag(bagVar.value,"BagCombineUndo")
    d.redo
    printBag(bagVar.value,"BagCombineRedo")
    for(diff <- d){
      diff.variable match{
        case bag:BagOfWordsVariable[BagOfWordsProposeAndDecide] => {
          bag.accept
          println("accepting bag")
        }
      }
    }
    printBag(bagVar.value,"BagCombineAccept")
    def printBag(b:BagOfWordsHashMap,str:String):Unit ={
      println(str)
      println("  l2a: "+b.l2Norm)
      println("  l2b: "+b.l2Norm)
    }
  }
  */

  def testBags:Unit ={
    val bag1 = new ComposableBagOfWords(Seq("ICML", "NIPS", "UAI", "AAAI", "KDD"))
    val bag2 = new ComposableBagOfWords(Seq("ICML","ACL","EMNLP","HLT","KDD"))
    val bag3 = new ComposableBagOfWords(Seq("KDD"))
    val combined = new ComposableBagOfWords(Seq("IJCAI"))
    printBag(combined,"CombinedInitial")
    combined.addBag(bag1)
    printBag(combined,"addedBag1")
    combined.addBag(bag2)
    printBag(combined,"addedBag2")
    combined.removeBag(bag3)
    printBag(combined,"removedBag3")
    combined.incorporateBags
    printBag(combined,"incorporated all")
    combined.addBag(bag3)
    printBag(combined,"add1")
    combined.removeBag(bag2)
    printBag(combined,"rem1")
    combined.removeBag(bag1)
    printBag(combined,"undoAll")
    combined.incorporateBags
    printBag(combined,"undoAllIncorporated")
    def printBag(b:ComposableBagOfWords,str:String):Unit ={
      println(str+":"+b.getCombinedBag)
      println("  l2a: "+b.l2Norm)
      println("  l2b: "+b.l2NormBruteForce)
    }
  }

  object Entities {
    import MongoCubbieConverter._
    private var cache = new CubbieRefs
    private val mongoConn = new Mongo("localhost", 27017)
    private val mongoDB = mongoConn.getDB("mongocubbie-test")
    private val coll = mongoDB.getCollection("persons")
    coll.drop()
    val entities = new MongoCubbieCollection(coll, () => new MyEntityCubbie)
    /*
    def getEntity(eid:Any): Option[MyEntity] = {
      val result = entities.query(_.id.set(eid)).map(MongoCubbieConverter.toCubbie(_)).toSeq
      if(result.size==1)Some(result(0))
      else None
    }*/
    def ++= (es:Iterable[MyEntity]) = for(ec<-es.map((new MyEntityCubbie).storeNamedEntity(_)))entities += ec
    def getAll:Seq[Entity] = {
      //val result = entities.map(MongoCubbieConverter.toCubbie(_))
     // result
      val result = for(ec <- entities) yield {
        val e = ec.fetchEntity(cache)
        ec.finishFetchEntity(e)
        e
      }
      result.toSeq
    }
    def nextBatch:Seq[Entity] ={
      val result = getAll
      result.map(_.initializeAttributesOfStructure)
      result.toSeq
    }
  }

  def populateDB:Unit = {
    val mentions = for (t <- data) yield new MyEntity(t._1, t._2,true)
    Entities ++= mentions
    println("ENTITIES: "+Entities.entities.size)
  }

  def main(args:Array[String]): Unit = {
    testBags
    populateDB
    val mentions = Entities.nextBatch
    for(m <- mentions){
      println(this.entityString(m))
      println("   *properties:  (exists?="+m.isConnected+" mention?="+m.isObserved+" #children:"+m.subEntitiesSize+")")
    }
    println("Coref mentions: "+data)
    println("Number of mentions: "+mentions.size)
    val model = new HierCorefModel
    val predictor = new HierarchicalCorefSampler(model)
    predictor.setEntities(mentions)
    predictor.process(2000)
    //println("Entities:\n"+predictor.getEntities)
    println("\nPRINTING ENTITIES")
    printEntities(predictor.getEntities)
    println("Inferred entities: "+predictor.getEntities.size)
    // priority queue
    // get next n entities from db, and their canopy
    // how much of tree substructure to retrieve, how to represent the "fringe"
  }
  
  val data = List(
    ("Andrew McCallum", List("nips", "icml", "acl")),
    ("Andrew MacCallum", List("acl", "emnlp")),
    ("Angrew McCallum", List("emnlp", "kdd")),
    ("McCallum", List("kdd")),
    ("A. McCallum", List("uai")),
    ("Michael Wick", List("kdd", "uai")),
    ("Mike Wick", List("kdd", "nips")),
    ("Michael Andrew Wick", List("icml", "nips")),
    ("Wick", List("siam", "kdd")),
    ("Wick", List("uai"))
  )
  def printEntities(entities:Seq[Entity]):Unit = {
    var count = 0
    for(e <- entities.filter((e:Entity) => {e.isRoot && e.isConnected})){
      println(entityString(e)+"\n")
      count += 1
    }
    println("Printed " + count + " entities.")
  }
  def entityString(e:Entity):String = {
    val result = new StringBuffer
    entityString(e,result)
    result.toString
  }
  protected def entityString(e:Entity, result:StringBuffer, depth:Int=0):Unit = {
    for(i<-0 until depth)result.append("   ")
    if(e.isRoot){
      result.append("EntityRoot["+e.attr[EntityName].value+"]")
      //result.append("(exists?="+e.isConnected+" mention?="+e.isObserved+" #children:"+e.subEntitiesSize+")")
    }else if(e.isObserved){
      result.append("Mention["+e.attr[EntityName].value+"]")
    }else{
      result.append("SubEntity["+e.attr[EntityName].value+"]")
    }
    result.append("{"+e.attr[TrueBow].value.toString+"}")
    result.append("\n")
    for(subEntity <- e.subEntitiesIterator)entityString(subEntity,result,depth+1)
  }
}

/**Basic trait for doing operations with bags of words*/
trait BagOfWords{ // extends scala.collection.Map[String,Double]{
  //def empty: This
  def size:Int
  def apply(word:String):Double
  def iterator:Iterator[(String,Double)]
  def l2Norm:Double
  def *(that:BagOfWords):Double
  def cosineSimilarity(that:BagOfWords):Double = {
    val numerator:Double = this * that
    val denominator:Double = this.l2Norm*that.l2Norm
    numerator/denominator
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
}
trait ProposeAndCombineBags extends BagOfWords{
  def addBag(that:BagOfWords):Unit
  def removeBag(that:BagOfWords):Unit
  def incorporateBags:Unit
}
/**Efficient implementation for bags of words where proposals temporarily hypothesize new bags, and these proposals are efficiently undone/redone.*/
class ComposableBagOfWords(initialWords:Iterable[String]=null,initialBag:Map[String,Double]=null) extends ProposeAndCombineBags{
  val _bag = new HashMap[String,Double]
  var addBag:Option[BagOfWords] = None
  var removeBag:Option[BagOfWords] = None
  protected var _l2Norm2 = 0.0

  if(initialWords!=null)for(w<-initialWords)this += (w,1.0)
  if(initialBag!=null)for((k,v)<-initialBag)this += (k,v)
  override def toString = getCombinedBag.toString //_bag.toString
  def incorporateBags:Unit = {
    addBag.foreach(this ++= _)
    removeBag.foreach(this --= _)
    addBag = None
    removeBag = None
  }
  @deprecated def discardBags:Unit ={
    addBag = None
    removeBag = None
  }
  def addBag(that:BagOfWords):Unit ={
    if(removeBag!=None && removeBag.get.eq(that))removeBag=None
    else if(addBag == None)addBag = Some(that)
    else this ++= that
    if(this eq that)throw new Exception("Can't add myself.")
  }
  def removeBag(that:BagOfWords):Unit ={
    if(addBag!=None && addBag .get.eq(that))addBag=None
    else if(removeBag == None)removeBag = Some(that)
    else this --= that
  }
  def getCombinedBag:HashMap[String,Double] ={
    val combined = new HashMap[String,Double]
    for((k,v)<-this.iterator)combined(k) = combined.getOrElse(k,0.0)+v
    for(bag<-addBag)for((k,v)<-bag.iterator)combined(k) = combined.getOrElse(k,0.0)+v
    for(bag<-removeBag)for((k,v)<-bag.iterator)combined(k) = combined.getOrElse(k,0.0)-v
    combined
  }
  def size = {var r =_bag.size;for(b<-addBag)r+=b.size;for(b<-removeBag)r+=b.size;r}
  def iterator = _bag.iterator //this is technical wrong, oh well. Tricky to think about the semantics of this in the context of "removeBag"
  def apply(s:String):Double = _bag.getOrElse(s,0.0)
  def contains(s:String):Boolean = _bag.contains(s)
  def *(that:BagOfWords):Double = {
    println("BAG: "+that)
    println("   this: "+this)
    //if(this.size > that.size)return that * this
    var result = 0.0
    for((word,weight) <- iterator)
      result += weight * that(word)
    for(bag<-addBag)result += bag  * this
    for(bag<-removeBag)result -= bag * this
    result
  }
  def +=(s:String,w:Double=1.0):Unit ={
    _l2Norm2 += w*w + 2*this(s)*w
    _bag(s) = _bag.getOrElse(s,0.0)+w
  }
  def -=(s:String,w:Double=1.0):Unit ={
    _l2Norm2 +=  w*w - 2*this(s)*w
    if(_bag(s)==w)_bag.remove(s)
    else _bag(s) = _bag(s) - w
  }
  def l2Norm:Double ={
    var result = _l2Norm2
    if(addBag!=None && this.eq(addBag.get))throw new Exception("Cannot add myself.")
    val addL2Norm2 = getOptionBagL2Norm2(addBag)
    val removeL2Norm2 = getOptionBagL2Norm2(removeBag)
    result += addL2Norm2 + removeL2Norm2 + 2 * (this * addBag - this * removeBag  - mult (addBag,removeBag))
    scala.math.sqrt(result)
  }
  protected def getOptionBagL2Norm2(bag:Option[BagOfWords]):Double = if(bag==None)0.0 else bag.get.l2Norm * bag.get.l2Norm
  protected def *(that:Option[BagOfWords]):Double = if(that==None)0.0 else that.get * this
  protected def mult(b1:Option[BagOfWords],b2:Option[BagOfWords]):Double = if(b1==None || b2==None)0.0 else b1.get * b2.get
  override def l2NormBruteForce:Double ={
    var result = 0.0
    val combined = getCombinedBag
    for((k,v)<-combined)result += v*v
    //System.out.println("BRUTE FORCE: "+this.iterator.toSeq.toString)
    //println("  v: "+result)
    scala.math.sqrt(result)
  }
}
trait BagOfWordsVar extends Variable with VarAndValueGenericDomain[BagOfWordsVar,ProposeAndCombineBags] with Iterable[(String,Double)]
class BagOfWordsVariable(initialWords:Iterable[String]=Nil) extends BagOfWordsVar with VarAndValueGenericDomain[BagOfWordsVariable,ProposeAndCombineBags] {
  // Note that the returned value is not immutable.
  def value = _members
  private val _members:ProposeAndCombineBags = new ComposableBagOfWords(initialWords)
  def members: ProposeAndCombineBags = _members
  def iterator = _members.iterator
  override def size = _members.size
  def contains(x:String) = _members.contains(x)
  def accept:Unit = _members.incorporateBags
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


/**Efficient implementation for bags of words where proposals temporarily hypothesize new bags, and these proposals are efficiently undone/redone.*/
/*
trait BagOfWordsProposeAndDecide extends BagOfWords{
  var addBags = new HashSet[BagOfWords]
  var removeBags = new HashSet[BagOfWords]

  def proposeAddBag(moreWords:BagOfWords):Unit = addBags += moreWords
  def proposeRemoveBag(lessWords:BagOfWords):Unit = removeBags += lessWords
  def rejectProposal:Unit = {addBags = new HashSet[BagOfWords];removeBags = new HashSet[BagOfWords]}
  def acceptProposal:Unit = {
    addBags.foreach(this ++= _)
    removeBags.foreach(this --= _)
    addBags = new HashSet[BagOfWords]
    removeBags = new HashSet[BagOfWords]
  }
  /*
  abstract override def *(that:BagOfWords):Double ={
    var result = that * this
    addBags.foreach(result += _ * this)
    removeBags.foreach(result += _ * this)
    result
  }
  abstract override def l2Norm:Double ={
    val myL2Norm = l2Norm
    var result = myL2Norm * myL2Norm
    addBags.foreach((bow:BagOfWords)=>{val l2=bow.l2Norm;result += l2*l2+bow*this})
    removeBags.foreach((bow:BagOfWords)=>{val l2=bow.l2Norm;result += (l2*l2-bow*this)})
    scala.math.sqrt(result)
    result
  }
  */
}

class BagOfWordsHashMap(initialBag:Iterable[String] = null) extends BagOfWordsProposeAndDecide{
  private val _bag = new HashMap[String,Double]
  if(initialBag!=null)for(w<-initialBag)_bag += w->1.0
  private var _l2Norm2:Double = 0.0
  def size = _bag.size
  def iterator = _bag.iterator
  def apply(word:String):Double = _bag.getOrElse(word,0.0)
  override def l2Norm:Double = {
    var result = _l2Norm2
    addBags.foreach((bow:BagOfWords)=>{val l2=bow.l2Norm;result += l2*l2+bow*this})
    removeBags.foreach((bow:BagOfWords)=>{val l2=bow.l2Norm;result += l2*l2-bow*this})
    scala.math.sqrt(result)
  }
  def *(that:BagOfWords):Double = {
    if(this.size > that.size)return that * this
    var result = 0.0
    for((word,weight) <- iterator)
      result += weight * that(word)
    addBags.foreach(result += _ * this)
    removeBags.foreach(result += _ * this)
    result
  }
  def +=(s:String,w:Double=1.0):Unit ={
    _bag(s) = _bag.getOrElse(s,0.0)+w
     _l2Norm2 += w*w + 2*this(s)*w
  }
  def -=(s:String,w:Double=1.0):Unit ={
    if(_bag(s)==w)_bag.remove(s)
    else _bag(s) = _bag(s) - w
    _l2Norm2 +=  w*w - 2*this(s)*w
  }
  //def ++=(that:BagOfWords):Unit = for((s,w) <- that.iterator)this += (s,w)
  //def --=(that:BagOfWords):Unit = for((s,w) <- that.iterator)this -= (s,w)
  def contains(s:String):Boolean = _bag.contains(s)
}


trait BagOfWordsVar extends Variable with VarAndValueGenericDomain[BagOfWordsVar,BagOfWordsHashMap] with Iterable[(String,Double)]
class BagOfWordsVariable[BagOfWordsProposeAndDecide] extends BagOfWordsVar with VarAndValueGenericDomain[BagOfWordsVariable[BagOfWordsProposeAndDecide],BagOfWordsHashMap] {
  // Note that the returned value is not immutable.
  def value = _members
  private val _members = new BagOfWordsHashMap
  def members: BagOfWordsHashMap = _members
  def iterator = _members.iterator
  override def size = _members.size
  def contains(x:String) = _members.contains(x)
  def accept:Unit = _members.acceptProposal
  def expose:BagOfWordsHashMap = _members
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
    _members.proposeAddBag(x)
  }
  def remove(x: BagOfWords)(implicit d: DiffList): Unit = {
    if (d != null) d += new BagOfWordsVariableRemoveBagDiff(x)
    _members.proposeRemoveBag(x)
  }

  final def +=(x:BagOfWords): Unit = add(x)(null)
  final def -=(x:BagOfWords): Unit = remove(x)(null)
  final def ++=(xs:Iterable[String]): Unit = xs.foreach(add(_)(null))
  final def --=(xs:Iterable[String]): Unit = xs.foreach(remove(_)(null))
  case class BagOfWordsVariableAddStringDiff(added: String,w:Double) extends Diff {
    // Console.println ("new SetVariableAddDiff added="+added)
    def variable: BagOfWordsVariable[BagOfWordsProposeAndDecide] = BagOfWordsVariable.this
    def redo = _members += (added,w)
    def undo = _members -= (added,w)
    override def toString = "BagOfWordsVariableAddStringDiff of " + added + " to " + BagOfWordsVariable.this
  }
  case class BagOfWordsVariableRemoveStringDiff(removed: String,w:Double) extends Diff {
    //        Console.println ("new SetVariableRemoveDiff removed="+removed)
    def variable: BagOfWordsVariable[BagOfWordsProposeAndDecide] = BagOfWordsVariable.this
    def redo = _members -= (removed,w)
    def undo = _members += (removed,w)
    override def toString = "BagOfWordsVariableRemoveStringDiff of " + removed + " from " + BagOfWordsVariable.this
  }
  case class BagOfWordsVariableAddBagDiff(added:BagOfWords) extends Diff {
    // Console.println ("new SetVariableAddDiff added="+added)
    def variable: BagOfWordsVariable[BagOfWordsProposeAndDecide] = BagOfWordsVariable.this
    def redo = _members.addBags += added
    def undo = _members.addBags -= added
    override def toString = "BagOfWordsVariableAddBagDiff of " + added + " to " + BagOfWordsVariable.this
  }
  case class BagOfWordsVariableRemoveBagDiff(removed: BagOfWords) extends Diff {
    //        Console.println ("new SetVariableRemoveDiff removed="+removed)
    def variable: BagOfWordsVariable[BagOfWordsProposeAndDecide] = BagOfWordsVariable.this
    def redo = _members.removeBags += removed
    def undo = _members.removeBags -= removed
    override def toString = "BagOfWordsVariableRemoveBagDiff of " + removed + " from " + BagOfWordsVariable.this
  }

}
*/

/*
trait MultisetVar[A,Double] extends Variable with VarAndValueGenericDomain[MultisetVar[A,Double],scala.collection.Map[A,B]] with Iterable[(A,Double)]
class MultisetVariable[A,Double]() extends MultisetVar[A,Double] with VarAndValueGenericDomain[MultisetVariable[A,Double],scala.collection.Map[A,Double]] {
  // Note that the returned value is not immutable.
  def value = _members
  private val _members = new HashMap[A,Double]
  def members: scala.collection.Map[A,Double] = _members
  def iterator = _members.iterator
  override def size = _members.size
  def contains(x:A) = _members.contains(x)
  def add(x:A)(implicit d: DiffList): Unit = if (!_members.contains(x)) {
    if (d != null) d += new MultisetVariableAddDiff(x)
    inc(x)
  }
  def remove(x: A)(implicit d: DiffList): Unit = if (_members.contains(x)) {
    if (d != null) d += new MultisetVariableRemoveDiff(x)
    dec(x)
  }
  protected def inc(x:A):Unit = _members(x) = _members.getOrElse(x,0.0)+1
  protected def dec(x:A):Unit = if(_members(x)==1)_members.remove(x) else _members(x) = _members.getOrElse(x,0.0)-1

  final def +=(x:A): Unit = add(x)(null)
  final def -=(x:A): Unit = remove(x)(null)
  final def ++=(xs:Iterable[A]): Unit = xs.foreach(add(_)(null))
  final def --=(xs:Iterable[A]): Unit = xs.foreach(remove(_)(null))
  case class MultisetVariableAddDiff(added: A) extends Diff {
    // Console.println ("new SetVariableAddDiff added="+added)
    def variable: MultisetVariable[A] = MultisetVariable.this
    def redo = inc(added)
    def undo = dec(added)
    override def toString = "SetVariableAddDiff of " + added + " to " + MultisetVariable.this
  }
  case class MultisetVariableRemoveDiff(removed: A) extends Diff {
    //        Console.println ("new SetVariableRemoveDiff removed="+removed)
    def variable: MultisetVariable[A] = MultisetVariable.this
    def redo = dec(removed)
    def undo = inc(removed)
    override def toString = "MultisetVariableRemoveDiff of " + removed + " from " + MultisetVariable.this
  }
}
*/
