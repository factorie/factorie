package cc.factorie.example

import cc.factorie._
import cc.factorie.app.nlp.coref._
import cc.factorie.db.mongo._
import cc.factorie.db.mongo.{LazyCubbieConverter, MongoCubbieCollection, MongoCubbieConverter, MongoCubbieImplicits}
import collection.mutable.{ArrayBuffer, HashSet, HashMap}
import com.mongodb.Mongo

object Coref4{
  /**The model: entities and their attribute variables and a hierarchical model*/
  class Name(val entity:Entity,s:String) extends StringVariable(s) with EntityAttr
  class Bow(val entity:Entity, topicBag:Map[String,Double]=null) extends BagOfWordsVariable(Nil, topicBag) with EntityAttr
  class MyEntity(fullName:String="DEFAULT", keywords:Iterable[String]=Seq.empty[String],isMention:Boolean = false) extends HierEntity(isMention){
    var _id = java.util.UUID.randomUUID.toString+""
    override def id = _id
    def string:String = name.value
    attr += new Name(this,fullName)
    attr += new Bow(this)
    def name = attr[Name]
    def bow = attr[Bow]
    for(keyword <- keywords)bow += keyword
  }
  class SimpleHierModel extends TemplateModel(
    new ChildParentTemplateWithStatistics[Name]{
      override def score(s:Stat):Double = {val childName=s._2;val parentName=s._3;-childName.editDistance(parentName).toDouble}
    },
    new ChildParentTemplateWithStatistics[Bow]{
      override def score(s:Stat):Double = s._2.cosineSimilarity(s._3)
    },
    new StructuralPriorsTemplate(8.0,0.25)
  )
  /**The database: entity cubbies and database for persistence*/
  class MyEntityCubbie(entity:MyEntity=null) extends HierEntityCubbie{
    protected var _entity:MyEntity = entity
    val name = new StringSlot("name")
    val bow = new CubbieSlot("bow", () => new BagOfWordsCubbie)
    val children = new InverseSlot("children",(a:MyEntityCubbie) => a.entityRef)
    if(entity!=null)storeEntity(entity)
    def fetchMyEntity:MyEntity = fetchEntity(null).asInstanceOf[MyEntity]
    override def newEntity:Entity = new MyEntity
    override def newEntityCubbie:EntityCubbie = new MyEntityCubbie
    override def finishFetchEntity(e:Entity) : Unit ={
      super.finishFetchEntity(e)
      e.attr[Name].set(name.value)(null)
      e.attr[Bow] ++= bow.value.fetch
      _entity=e.asInstanceOf[MyEntity]
    }
    override def finishStoreEntity(e:Entity) : Unit = {
      super.finishStoreEntity(e)
      name := e.attr[Name].value
      bow := new BagOfWordsCubbie().store(e.attr[Bow].value)
      this.id=e.id
    }
    def getEntity:MyEntity=_entity
  }
  class EntityDatabase(mongoServer:String="localhost",mongoPort:Int=27017,mongoDBName:String="hier-demo"){
    import MongoCubbieImplicits._
    import MongoCubbieConverter._
    protected val mongoConn = new Mongo(mongoServer,mongoPort)
    protected val mongoDB = mongoConn.getDB(mongoDBName)
    protected val coll = mongoDB.getCollection("people")
    protected val entities = new MongoCubbieCollection(coll,() => new MyEntityCubbie,(a:MyEntityCubbie) => Seq(Seq(a.entityRef))) with LazyCubbieConverter[MyEntityCubbie]
    def drop:Unit = coll.drop
    def store(data:List[(String,List[String])]):Unit ={
      val result = new ArrayBuffer[MyEntity]
      for(datum <- data)result += new MyEntity(datum._1,datum._2,true)
      for(entity <- result)entities += new MyEntityCubbie(entity)
    }
    def load:Seq[MyEntity] = {
      val result = (for(cubbie <- entities)yield{cubbie.fetchMyEntity;cubbie.getEntity}).toSeq
      println("entities size: "+entities.size)
      println("result size: "+result.size)
      result
    }
  }
  /**Sampler*/
  class MyEntitySampler(model:TemplateModel) extends HierCorefSampler[MyEntity](model){
    def newEntity = new MyEntity
    override def mergeUp(e1:MyEntity,e2:MyEntity)(implicit d:DiffList):MyEntity = {
      val oldParent1 = e1.parentEntity
      val oldParent2 = e2.parentEntity
      val result = newEntity
      e1.setParentEntity(result)(d)
      e2.setParentEntity(result)(d)
      result.attr[Name].set(e1.attr[Name].value)
      result.attr[Bow].add(e1.attr[Bow].value)(d)
      result.attr[Bow].add(e2.attr[Bow].value)(d)
      structurePreservationForEntityThatLostChild(oldParent1)(d)
      structurePreservationForEntityThatLostChild(oldParent2)(d)
      result
    }
    def sampleAttributes(entity:MyEntity)(implicit d:DiffList) = {
      val representative = entity.childEntities.sampleUniformly(random)
      entity.attr[Name].set(representative.attr[Name].value)
      entity.attr[Dirty].reset
      if(entity.parentEntity != null)entity.parentEntity.attr[Dirty].++()(d)
    }
  }

  def main(args:Array[String]) = {
    val database = new EntityDatabase()
    val model = new SimpleHierModel
    val sampler = new MyEntitySampler(model)
    database.drop
    database.store(data)
    sampler.setEntities(database.load)
    sampler.process(1000)
    printEntities(sampler.getEntities)
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
      println(entityString(e))
      count += 1
    }
    println("Printed " + count + " entities.")
  }
  def entityString(e:Entity):String = {
    if(e==null)return "null"
    val result = new StringBuffer
    entityString(e,result)
    result.toString
  }
  /**Methods for pretty printing entities*/
  protected def entityString(e:Entity, result:StringBuffer, depth:Int=0):Unit = {
    for(i<-0 until depth)result.append("   ")
    //result.append(e.id+"-")
    if(e.isRoot){
      result.append("EntityRoot["+e.attr[Name].value+"]")
      //result.append("(exists?="+e.isConnected+" mention?="+e.isObserved+" #children:"+e.subEntitiesSize+")")
    }else if(e.isObserved){
      result.append("Mention["+e.attr[Name].value+"]")
      //result.append(" Title="+e.asInstanceOf[AuthorEntity].paper.title)
    }else{
      result.append("SubEntity["+e.attr[Name].value+"]")
      if(e.childEntitiesSize==0)result.append("-SUBENTITY ERROR")//throw new Exception("ERROR SUB ENTITY IS EMPTY")
      //if(e.subEntitySize==1)throw new Exception("ERROR SUB ENTITY HAS ONE CHILD)
    }
    result.append("{"+bagToString(e.attr[Bow].value)+"}")
    result.append("\n")
    for(childEntity <- e.childEntitiesIterator)entityString(childEntity,result,depth+1)
  }
  def bagToString(bag:BagOfWords,k:Int=8):String = {
    val map = new HashMap[String,Double]
    for((k,v) <- bag.iterator)map += k -> v
    topk(map,k)
  }
  def topk(bag:HashMap[String,Double], k:Int=18) : String ={
    val result = new StringBuffer
    val sorted = bag.toList.sortBy(_._2).reverse.take(k)
    for(i<-0 until sorted.length){
      result.append(sorted(i)._1+" -> "+sorted(i)._2)
      if(i<sorted.length-1)
        result.append(", ")
    }
    result.toString
  }
}