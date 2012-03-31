package cc.factorie.example

import bibtex._
import bibtex.parser._
import bibtex.dom._
import bibtex.expansions._
import cc.factorie.app.nlp.coref._
import scala.collection.mutable.{HashSet,HashMap,ArrayBuffer}
import cc.factorie._
import cc.factorie.app.nlp.coref._
import cc.factorie.db.mongo._
import com.mongodb.{BasicDBList, BasicDBObject, DBCursor, DBObject, DBCollection, Mongo}
import cc.factorie.util.CubbieRefs
import java.io.{InputStreamReader, FileInputStream, BufferedReader, File}
import javax.xml.parsers.{DocumentBuilder, DocumentBuilderFactory}
import org.w3c.dom.{Node, NodeList, Document}

trait HasCanopyAttributes[T<:Entity]{
  val canopyAttributes = new ArrayBuffer[CanopyAttribute[T]]
}
trait CanopyAttribute[T<:Entity]{def entity:T;def canopyName:String}
class AuthorFLNameCanopy(val entity:AuthorEntity) extends CanopyAttribute[AuthorEntity] {
  def canopyName:String=(initial(entity.entityRoot.asInstanceOf[AuthorEntity].fullName.firstName)+entity.entityRoot.asInstanceOf[AuthorEntity].fullName.lastName).toLowerCase
  //def canopyName:String=(initial(entity.fullName.firstName)+entity.fullName.lastName).toLowerCase
  def initial(s:String):String = if(s!=null && s.length>0)s.substring(0,1) else ""
}
/**Attributes specific to REXA authors*/
class FullName(val entity:Entity,f:String,m:String,l:String) extends SeqVariable[String](Seq(f,m,l)) with EntityAttr {
  def setFirst(s:String)(implicit d:DiffList) = update(0,s)
  def setMiddle(s:String)(implicit d:DiffList) = update(1,s)
  def setLast(s:String)(implicit d:DiffList) = update(2,s)
  def setFullName(that:FullName)(implicit d:DiffList) = {
    if(firstName!=that.firstName)setFirst(that.firstName)(null)
    if(middleName!=that.middleName)setMiddle(that.middleName)(null)
    if(lastName!=that.lastName)setLast(that.lastName)(null)
  }
  def firstName = value(0)
  def middleName = value(1)
  def lastName = value(2)
  def domain = GenericDomain
  override def toString:String = {
    val result = new StringBuffer
    if(firstName!=null)result.append(firstName+" ")
    if(middleName!=null)result.append(middleName+" ")
    if(lastName!=null)result.append(lastName+" ")
    result.toString
  }
}
class Title(val entity:Entity,title:String) extends StringVariable(title) with EntityAttr
class Year(val entity:Entity,year:Int) extends IntegerVariable(year) with EntityAttr
class VenueName(val entity:Entity,venueName:String) extends StringVariable(venueName) with EntityAttr
class Bags extends HashMap[String,BagOfWords]
class BagOfTopics(val entity:Entity, topicBag:Map[String,Double]=null) extends BagOfWordsVariable(Nil, topicBag) with EntityAttr
class BagOfVenues(val entity:Entity, venues:Map[String,Double]=null) extends BagOfWordsVariable(Nil, venues) with EntityAttr
class BagOfCoAuthors(val entity:Entity,coAuthors:Map[String,Double]=null) extends BagOfWordsVariable(Nil, coAuthors) with EntityAttr
/**Entity variables*/
/**An entity with the necessary variables/coordination to implement hierarchical coreference.*/
class PaperEntity(s:String="DEFAULT",isMention:Boolean=false) extends HierEntity(isMention){
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
class AuthorEntity(f:String="DEFAULT",m:String="DEFAULT",l:String="DEFAULT", isMention:Boolean = false) extends HierEntity(isMention) with HasCanopyAttributes[AuthorEntity]{
  var _id = java.util.UUID.randomUUID.toString+""
  override def id = _id
  var priority:Double=scala.math.exp(random.nextDouble)
  canopyAttributes += new AuthorFLNameCanopy(this)
  attr += new FullName(this,f,m,l)
  attr += new BagOfTopics(this)
  attr += new BagOfVenues(this)
  attr += new BagOfCoAuthors(this)
  def fullName = attr[FullName]
  def bagOfTopics = attr[BagOfTopics]
  def bagOfVenues = attr[BagOfVenues]
  def bagOfCoAuthors = attr[BagOfCoAuthors]
  def string = f+" "+m+" "+l
  var paper:PaperEntity = null
  def defaultCanopy = canopyAttributes.head.canopyName
}
class AuthorEntityCubbie(author:AuthorEntity=null) extends HierEntityCubbie{
  protected var _author:AuthorEntity = author
  val firstName = new StringSlot("firstName")
  val middleName = new StringSlot("middleName")
  val lastName = new StringSlot("lastName")
  val bagOfTopics = new CubbieSlot("bagOfTopics", () => new BagOfWordsCubbie)
  val bagOfVenues = new CubbieSlot("bagOfVenues", () => new BagOfWordsCubbie)
  val bagOfCoAuthors = new CubbieSlot("bagOfCoAuthors", () => new BagOfWordsCubbie)
  val canopies = new StringListSlot("canopies")
  val priority = new DoubleSlot("priority")
  val children = new InverseSlot("children",(a:AuthorEntityCubbie) => a.entityRef)
  if(author!=null)storeEntity(author)
  def fetchAuthorEntity(cr:CubbieRefs):AuthorEntity = fetchEntity(cr).asInstanceOf[AuthorEntity]
  override def newEntity:Entity = new AuthorEntity
  override def newEntityCubbie:EntityCubbie = new AuthorEntityCubbie
  override def finishFetchEntity(e:Entity) : Unit ={
    super.finishFetchEntity(e)
    e.attr[FullName].setFirst(firstName.value)(null)
    e.attr[FullName].setMiddle(middleName.value)(null)
    e.attr[FullName].setLast(lastName.value)(null)
    e.attr[BagOfTopics] ++= bagOfTopics.value.fetch
    e.attr[BagOfVenues] ++= bagOfVenues.value.fetch
    e.attr[BagOfCoAuthors] ++= bagOfCoAuthors.value.fetch
    e.asInstanceOf[AuthorEntity].priority = priority.value
    e.asInstanceOf[AuthorEntity]._id = this.id.toString
    _author=e.asInstanceOf[AuthorEntity]
  }
  override def finishStoreEntity(e:Entity) : Unit = {
    super.finishStoreEntity(e)
    firstName := e.attr[FullName].firstName
    middleName := e.attr[FullName].middleName
    lastName := e.attr[FullName].lastName
    bagOfTopics := new BagOfWordsCubbie().store(e.attr[BagOfTopics].value)
    bagOfVenues := new BagOfWordsCubbie().store(e.attr[BagOfVenues].value)
    bagOfCoAuthors := new BagOfWordsCubbie().store(e.attr[BagOfCoAuthors].value)
    canopies := e.asInstanceOf[AuthorEntity].canopyAttributes.map(_.canopyName).toSeq
    priority := e.asInstanceOf[AuthorEntity].priority
    this.id=e.id
  }
  def getAuthor:AuthorEntity=_author
}

object Coref3 {
  class HierCorefModel extends TemplateModel(
    new TemplateWithStatistics3[EntityRef,FullName,FullName] {
      def unroll1(er:EntityRef) = if(er.dst!=null)Factor(er, er.src.attr[FullName], er.dst.attr[FullName]) else Nil
      def unroll2(childName:FullName) = Factor(childName.entity.parentEntityRef, childName, childName.entity.parentEntity.attr[FullName])
      def unroll3(parentName:FullName) = for (e <- parentName.entity.childEntities) yield Factor(e.parentEntityRef, e.attr[FullName], parentName)
      def score(s:Stat): Double = {
        var result = 0.0
        val childName = s._2
        val parentName = s._3
        val childFirst = childName(0).toLowerCase
        val childMiddle = childName(1).toLowerCase
        val childLast = childName(2).toLowerCase
        val parentFirst = parentName(0).toLowerCase
        val parentMiddle = parentName(1).toLowerCase
        val parentLast = parentName(2).toLowerCase
        if(childLast != parentLast)result -= 8
        if(initialsMisMatch(childFirst,parentFirst))result -=8
        if(initialsMisMatch(childMiddle,parentMiddle))result -= 8
        if(nameMisMatch(childFirst,parentFirst))result -= 8
        if(parentMiddle.length > childMiddle.length)result += 100
        if((childMiddle.length==0 && parentMiddle.length>0) || (parentMiddle.length==0 && childMiddle.length>0))result -= 0.25
        //var result = -cc.factorie.app.strings.editDistance(childString, parentString)
        //println("EntityName:"+result)
 //       println("  FullName: "+result)
        result
      }
      def initialsMisMatch(c:String,p:String):Boolean = (c!=null && p!=null && c.length>0 && p.length>0 && c.charAt(0)!=p.charAt(0))
      def nameMisMatch(c:String,p:String):Boolean = (c!=null && p!=null && c.length>1 && p.length>1 && c != p)
    },
    // Compatibility between parent/child co-author bags. Currently an approximation, but well worth the speed.
    new TemplateWithStatistics3[EntityRef,BagOfCoAuthors,BagOfCoAuthors] {
      val strength = 16.0
      val shift = -0.25
      def unroll1(er:EntityRef) = if(er.dst!=null)Factor(er, er.src.attr[BagOfCoAuthors], er.dst.attr[BagOfCoAuthors]) else Nil
      def unroll2(childBow:BagOfCoAuthors) = Nil//if(childBow.entity.parentEntity!=null)Factor(childBow.entity.parentEntityRef, childBow, childBow.entity.parentEntity.attr[BagOfCoAuthors]) else Nil
      def unroll3(parentBow:BagOfCoAuthors) = Nil // for(e<-parentBow.entity.childEntities) yield Factor(e.parentEntityRef,e.attr[BagOfCoAuthors],parentBow)
      def score(s:Stat): Double = {
        val childBow = s._2
        val parentBow = s._3
        //System.out.flush
        var result = childBow.cosineSimilarity(parentBow,childBow)
//        var result = childBow.cosineSimilarity(parentBow)
//        println("  BowCoAuth: "+result)
//        println("  scaled: "+(result+shift)*strength)
//        println("    childBag :"+childBow)
//        println("    parentBag:"+parentBow)
        (result+shift)*strength
      }
    },

    // Compatibility between parent/child venue bags. Currently an approximation, but well worth the speed.
    new ChildParentTemplateWithStatistics[BagOfVenues] {
      val strength = 8.0
      val shift = -0.25
      //def unroll1(er:EntityRef) = if(er.dst!=null)Factor(er, er.src.attr[BagOfVenues], er.dst.attr[BagOfVenues]) else Nil
      //def unroll2(childBow:BagOfVenues) = Nil//if(childBow.entity.parentEntity!=null)Factor(childBow.entity.parentEntityRef, childBow, childBow.entity.parentEntity.attr[BagOfVenues]) else Nil
      //def unroll3(parentBow:BagOfVenues) = Nil//for(e<-parentBow.entity.childEntities) yield Factor(e.parentEntityRef,e.attr[BagOfVenues],parentBow)
      def score(s:Stat): Double = {
        val childBow = s._2
        val parentBow = s._3
        var result = childBow.cosineSimilarity(parentBow)
        (result+shift)*strength
      }
    },

    //structural priors
/*
    new TemplateWithStatistics1[EntityRef] {
      val entityExistenceCost = 40000.0 //8
      val subEntityExistenceCost = 0.5
      def score(s:Stat): Double = {
        val child:Entity = s._1._1
        val parent:Entity = s._1._2
        var result = 0.0
        if (parent==null && child.isConnected) result -= entityExistenceCost
        if (parent != null && child.isObserved) result -= subEntityExistenceCost
        //println("result: "+result)
        result
      }
    }
    */
    new TemplateWithStatistics3[EntityExists,IsEntity,IsMention] {
      val entityExistenceCost = 2.0 //2 //8
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
//        println("  STRUCTURE PRIOR: "+result)
        result
      }
    }/*,
    //bags of words priors
    new TemplateWithStatistics1[BagOfWordsVariable]{
      var temperature=0.1
      def symmetricDirichletAlpha=temperature+1.0
      def score(s:Stat):Double ={
        val bag = s._1
        var result = -dirichletKernel(bag)
        result
      }
      def dirichletKernel(bag:BagOfWords):Double ={
        var result = 0.0
        var l1Norm = 0.0
        //val iterator = bag.iterator
        for((k,v) <- bag.iterator)l1Norm += v
        for((k,v) <- bag.iterator)result += scala.math.pow(v/l1Norm,symmetricDirichletAlpha-1.0)
        result
      }
    }*/
  )
  /*
class CanopySampler[T<:Entity](model:HierCorefModel){
  var proposers = new HashMap[String,HierarchicalCorefSampler]
  def setEntities(ents:T):Unit ={
    proposers = new HashMap[String,HierarchicalCorefSampler]
        
  }
}
*/
  /*
  class ParallelAuthorSampler(model:TemplateModel){
    val samplers = new ArrayBuffer(String,AuthorSampler)
    protected def createBatches(ents:Seq[AuthorEntity], numBatches:Int):Unit ={
      val canopies = new HashMap[String,AuthorEntity]
      for(e<-ents)canopies.getOrElse(cname,{val a = new ArrayBuffer[AuthorEntity];canopies(cname)=a;a}) += e
      var largestBatch = canopies.foldLeft()
    }
    def process(ents:Seq[AuthorEntity],a:Int,b:Int,c:Int):Unit ={
      
    }
  }
*/

  class AuthorSampler(model:TemplateModel) extends HierCorefSampler[AuthorEntity](model){
    protected var canopies = new HashMap[String,ArrayBuffer[AuthorEntity]]
    var proposalCount = 0
    def newEntity = new AuthorEntity
    override def addEntity(e:AuthorEntity):Unit ={
      super.addEntity(e)
      val cname = e.defaultCanopy
      canopies.getOrElse(cname,{val a = new ArrayBuffer[AuthorEntity];canopies(cname)=a;a}) += e
    }
    override def setEntities(ents:Iterable[AuthorEntity]):Unit ={
      canopies = new HashMap[String,ArrayBuffer[AuthorEntity]]
      super.setEntities(ents)
      println("Number of canopies: "+canopies.size)
      for((k,v) <- canopies)println("  -"+k+":"+v.size)
    }
    def sampleAttributes(author:AuthorEntity)(implicit d:DiffList) = {
      val representative = author.childEntities.sampleUniformly(random)
      author.attr[FullName].set(representative.attr[FullName])
      author.attr[Dirty].reset
      if(author.parentEntity != null)author.parentEntity.attr[Dirty].++()(d)
    }
    override def nextEntity(context:AuthorEntity=null):AuthorEntity=if(context==null)sampleEntity(entities) else sampleEntity(canopies(context.defaultCanopy))
    override def mergeLeft(left:AuthorEntity,right:AuthorEntity)(implicit d:DiffList):Unit ={
      val oldParent = right.parentEntity
      right.setParentEntity(left)(d)
      propagateBagUp(right)(d)
      propagateRemoveBag(right,oldParent)(d)
      structurePreservationForEntityThatLostChild(oldParent)(d)
    }
    /**Jump function that proposes merge: entity1--->NEW-PARENT-ENTITY<---entity2 */
    override def mergeUp(e1:AuthorEntity,e2:AuthorEntity)(implicit d:DiffList):AuthorEntity = {
      val oldParent1 = e1.parentEntity
      val oldParent2 = e2.parentEntity
      val result = newEntity
      e1.setParentEntity(result)(d)
      e2.setParentEntity(result)(d)
      if(e1.attr[FullName].middleName.length>0)
        result.attr[FullName].setFullName(e1.attr[FullName])
      else
        result.attr[FullName].setFullName(e2.attr[FullName])
      result.attr[BagOfCoAuthors].add(e1.attr[BagOfCoAuthors].value)(d)
      result.attr[BagOfCoAuthors].add(e2.attr[BagOfCoAuthors].value)(d)
      result.attr[BagOfVenues].add(e1.attr[BagOfVenues].value)(d)
      result.attr[BagOfVenues].add(e2.attr[BagOfVenues].value)(d)
      propagateRemoveBag(e1,oldParent1)(d)
      propagateRemoveBag(e2,oldParent2)(d)
      structurePreservationForEntityThatLostChild(oldParent1)(d)
      structurePreservationForEntityThatLostChild(oldParent2)(d)
      result
    }
    /**Peels off the entity "right", does not really need both arguments unless we want to error check.*/
    override def splitRight(left:AuthorEntity,right:AuthorEntity)(implicit d:DiffList):Unit ={
      val oldParent = right.parentEntity
      right.setParentEntity(null)(d)
      propagateRemoveBag(right,oldParent)(d)
      structurePreservationForEntityThatLostChild(oldParent)(d)
    }
    def propagateBagUp(entity:Entity)(implicit d:DiffList):Unit ={
      var e = entity.parentEntity
      while(e!=null){
        e.attr[BagOfCoAuthors].add(entity.attr[BagOfCoAuthors].value)(d)
        e.attr[BagOfVenues].add(entity.attr[BagOfVenues].value)(d)
        e = e.parentEntity
      }
    }
    def propagateRemoveBag(parting:Entity,formerParent:Entity)(implicit d:DiffList):Unit ={
      var e = formerParent
      while(e!=null){
        e.attr[BagOfCoAuthors].remove(parting.attr[BagOfCoAuthors].value)
        e.attr[BagOfVenues].remove(parting.attr[BagOfVenues].value)
        e = e.parentEntity
      }
    }
    override def proposalHook(proposal:Proposal) = {
      super.proposalHook(proposal)
      for(diff<-proposal.diff){
        diff.variable match{
          case bag:BagOfWordsVariable => bag.accept
          //case bag:TrueBow => bag.accept
          case _ => {}
        }
      }
      proposalCount += 1
      if(proposalCount % 1000==0)
        print(proposalCount+" ")
      if(proposalCount % (1000*20)==0)
        println
    }
/*
    override def pickProposal(proposals:Seq[Proposal]): Proposal = {
      var result = super.pickProposal(proposals)
      println("JUMPS")
      for(p <- proposals){
        if(p eq result)print("  *") else print("   ")
        println("SCORE: "+p.modelScore)
        p.diff.redo
        p.diff.scoreAndUndo(model)
      }
      result
    }
    */
  }

/*
  class Rexa2Canopy(rexa2:Rexa2, name:String){
    protected val col = rexa2.mongoDB.getCollection(name)
    protected val canopyCollection = new MongoCubbieCollection(coll,() => new CanopyCubbie, (c:CanopyCubbie) => Seq(Seq(c.canopies)))
    var canopyCollections = new ArrayBuffer[MongoCubbieCollection]
    def addCanopyCollection[T<:HasCanopyAttributes](entityWithCanopy:T,name:String):Unit ={
      val coll = rexa2.mongoDB.getCollection(name)
      val mcoll = new MongoCubbieCollection[](coll,)
    }
  }
  */
  class Rexa2(mongoServer:String="localhost",mongoPort:Int=27017,mongoDBName:String="rexa2-cubbie"){
    import MongoCubbieImplicits._
    import MongoCubbieConverter._
    protected var _author2cubbie = new HashMap[Any,AuthorEntityCubbie]
    protected var cache = new HashMap[Any,AuthorEntityCubbie]
    //protected var authorCache = new HashMap[Any,AuthorEntity]
    protected val mongoConn = new Mongo(mongoServer,mongoPort)
    protected val mongoDB = mongoConn.getDB(mongoDBName)
    protected val coll = mongoDB.getCollection("authors")
    protected val authors = new MongoCubbieCollection(coll,() => new AuthorEntityCubbie,(a:AuthorEntityCubbie) => Seq(Seq(a.canopies),Seq(a.priority),Seq(a.entityRef))) with LazyCubbieConverter[AuthorEntityCubbie]
    println("Created a rexa2 database: "+mongoDBName+"@"+mongoServer+":"+mongoPort)
    def drop = coll.drop
    def insertMentionsFromBibDir(bibDir:File):Unit ={
      for(f<-bibDir.listFiles)insertMentionsFromBibFile(f)
    }
    def insertMentionsFromBibFile(bibFile:File,numEntries:Int = Integer.MAX_VALUE):Unit ={
      import MongoCubbieConverter._
      val paperEntities = loadBibTeXFile(bibFile)
      var count = -1
      while({count;count+=1;count} < scala.math.min(numEntries,paperEntities.size)){
       val paper = paperEntities(count)
      //for(paper <- paperEntities){
        for(author <- paper.authors){
          addFeatures(author)
          authors += new AuthorEntityCubbie(author)
        }
      }
    }
    def insertMentionsFromDBLP(location:String):Unit ={
      var time = System.currentTimeMillis
      import MongoCubbieConverter._
      val paperEntities = DBLPLoader.loadDBLPData(location)
      for(paper <- paperEntities){
        for(author <- paper.authors){
          addFeatures(author)
          authors += new AuthorEntityCubbie(author)
        }
      }
      println("Inserting " + authors.size + " author mentions took " + (System.currentTimeMillis -time)/1000L+"s.")
    }
    def wasLoadedFromDB(author:AuthorEntity):Boolean = _author2cubbie.contains(author.id)
    def author2cubbie(author:AuthorEntity):AuthorEntityCubbie = _author2cubbie.getOrElse(author.id,null)
    protected def reset:Unit ={
      var author2cubbie = new HashMap[Any,AuthorEntityCubbie]
      var cache = new CubbieRefs
    }
    def nextBatch(n:Int=10):Seq[AuthorEntity] ={
      reset
      println("  Popping "+n+" off the priority queue.")
      //val canopies = authors.query(_.entityRef(null)).sort(_.priority)
      val canopyHash = new HashSet[String]
      var result = new ArrayBuffer[AuthorEntityCubbie]
      var topPriority = new ArrayBuffer[AuthorEntityCubbie]
      val sorted = authors.query(null,_.canopies.select.priority.select).sort(_.priority(-1))
      for(i <- 0 until n)if(sorted.hasNext)topPriority += sorted.next
      sorted.close
      for(author <- topPriority){
        //println("priority: "+author.priority)
        for(name <- author.canopies.value){
          if(!canopyHash.contains(name)){
            result ++= authors.query(_.canopies(Seq(name)))
            canopyHash += name
            println("    added canopy name: "+author.canopies+ " result.size="+result.size)
          }
        }
      }
      
      val initialAuthors = (for(authorCubbie<-result) yield authorCubbie.fetchAuthorEntity(null)).toSeq
      for(cubbie <- result){
        cache += cubbie.id -> cubbie
        _author2cubbie += cubbie.getAuthor.id -> cubbie
      }
      assembleEntities(result, (id:Any)=>{cache(id).getAuthor}, (e:AuthorEntityCubbie)=>{e.getAuthor})
      //println("DONE ASSEMBLING AUTHORS")
      //printEntities(initialAuthors,false)
      //println("DONE PRINTING ASSEMBELD AUTHORS")
      checkIntegrity(initialAuthors)
      initialAuthors
      /*
      cache = new CubbieRefs
      val inverter = new CachedFunction(new LazyMongoInverter(Map(manifest[AuthorEntityCubbie] -> authors)))
      def loadChildren(author:AuthorEntityCubbie):Unit ={
        for(childCubbie <- author.children.value)
          loadChildren(childCubbie)
      }

      implicit val refs = GraphLoader.load(result,{
        case a:AuthorEntityCubbie => Seq()
        case a:AuthorEntityCubbie => a.children.value(inverter).toSeq//.map(GraphLoader.SlotInCollection(_,authors)).toSeq
        //case a:AuthorEntityCubbie => a.children.value(inverter).map(GraphLoader.SlotInCollection(_.entityRef,authors)).toSeq
        //case a:AuthorEntityCubbie => a.children.value(inverter).map((entityRef:AuthorEntityCubbie#RefSlot)=>GraphLoader.SlotInCollection(entityRef,authors)).toSeq
        //case a:AuthorEntityCubbie => a.children.value(inverter).map((aec:AuthorEntityCubbie)=>MongoCubbieImplicits.toMongoRefSlot(aec)).toSeq
      })
      //todo: de-ref
      //def cubbie2slot(c:AuthorEntityCubbie):RefSlot = {}
      println("REF SIZE: "+refs.size)
      refs.map(_._2).toSeq
*/
    }
    /*
    def nextBatch:Seq[AuthorEntity]={
      cache = new CubbieRefs
      val result = for(authorCubbie<-authors) yield authorCubbie.fetchAuthorEntity(cache)
      result.toSeq
    }*/
    def assembleEntities[C<:EntityCubbie,E<:Entity](toAssemble:Seq[C],id2entity:Any=>E, cubbie2entity:C=>E):Unit ={
      for(c<-toAssemble){
        val child = cubbie2entity(c)
        val parent = if(c.entityRef.isDefined)id2entity(c.entityRef.value) else null.asInstanceOf[E]
        //println("entityRef defined? = "+entityRef.isDefined+" in id list? = "+id2entity)
        child.setParentEntity(parent)(null)
      }
    }
    def store(entitiesToStore:Iterable[AuthorEntity]):Unit ={
      var timer = System.currentTimeMillis
      var numdeleted=0
      var numadded=0
      var nummodified=0
      changePriorities(entitiesToStore)
      val deletedByInference = entitiesToStore.filter(!_.isConnected)
      val updatedOrAddedByInference = entitiesToStore.filter(_.isConnected)
      println("deleted by inference size: "+deletedByInference.size)
      for(deleted <- deletedByInference){
        if(wasLoadedFromDB(deleted)){
          numdeleted+=1
          authors.remove(_.idIs(author2cubbie(deleted).id))
        }
      }
      //todo: modify api to mongo cubbies to delete
      for(updatedOrAdded <- updatedOrAddedByInference){
        val old = cache.getOrElse(updatedOrAdded.id,null).asInstanceOf[AuthorEntityCubbie]
        val newAuthor = new AuthorEntityCubbie(updatedOrAdded)
        if(old==null){
          authors += newAuthor
          numadded += 1
        }
        else{
          authors.updateDelta(old,newAuthor)
          nummodified +=1
        }
        //authors.updateDelta(cache.getOrElse(updatedOrAdded.id,null).asInstanceOf[AuthorEntityCubbie],new AuthorEntityCubbie(updatedOrAdded))
      } //update delta code to handle null
      timer = (System.currentTimeMillis-timer)/1000L
      println("Store summary: adding "+numadded + ", removing "+numdeleted + " and modified "+nummodified+" entities took "+timer+"s.")
    }
    def changePriorities(entities:Iterable[AuthorEntity]):Unit ={
      for(e<-entities)e.priority = scala.math.exp(e.priority - random.nextDouble)
    }
  }

  var skipped=0
  var numParsed=0
  def loadBibTeXFile(file:File):Seq[PaperEntity]={
    val bibParser = new BibtexParser(false)
    val expander = new PersonListExpander(true,true)
    val bibDoc = new BibtexFile
    val result = new ArrayBuffer[PaperEntity]
    try{
      bibParser.parse(bibDoc,new BufferedReader(new InputStreamReader(new FileInputStream(file))))
      try {expander.expand(bibDoc)}
      catch {
        case e : Exception => {
          e.printStackTrace
          println("Adding .bad extension to file: " + file.getName());
          val badDir = new File(file.getParent()+ "/bad");
          badDir.mkdirs();
          //println(badDir.getAbsolutePath() + "/" + file.getName() + ".bad\n");
          file.renameTo(new File(badDir.getAbsolutePath + "/" + file.getName() + ".bad"));
        }
      }
      val entries = bibDoc.getEntries
      for(i<-0 until entries.size){
        entries.get(i) match{
          case x:BibtexEntry => result += bib2mention(x)
          case x:BibtexToplevelComment => {}
          case _ => {}
        }
      }
      numParsed += 1
    }catch{
      case e:Exception =>{
        skipped += 1
        println("\n=================================")
        e.printStackTrace
        println("=================================")
        println("=================================")
        println("ill-formated bib entry in file: " + file.getName)
        println("  total skipped: " + skipped)
      }
    }
    //processEdits(result)
    //filter(result)
    result
  }
  def splitFirst(firstName:String) : (String,String) ={
    var first:String = ""
    var middle : String = ""
    if(firstName==null)return ("","")
    val split=firstName.replaceAll("[\\.]",". ").replaceAll("[ ]+"," ").trim.split(" ",2)
    first = split(0)
    if(split.length==2)middle = split(1)
    (first,middle)
  }
  def filterFieldNameForMongo(s:String) = s.replaceAll("[$\\.]","")
  def addFeatures(author:AuthorEntity):Unit ={
    val paper = author.paper
    if(paper!=null){
      for(coAuthor <- paper.authors){
        if(coAuthor.ne(author)){
          var word:String = coAuthor.fullName.firstName
          if(word!=null && word.length>0)word=word.charAt(0)+"" else word = ""
          word += coAuthor.fullName.lastName
          author.bagOfCoAuthors += word.toLowerCase
        }
        if(paper.venueName!=null && paper.venueName.value.length>0)
          for(tok<-FeatureUtils.venueBag(paper.venueName.value))
          author.bagOfVenues.add(tok)(null)
      }
    }else println("Warning: paper is null for author with id "+author.id+" cannot compute features.")
  }
  def bib2mention(entry:BibtexEntry):PaperEntity ={
    val entryType = entry.getEntryType.toLowerCase
    val key = entry.getEntryKey
    val paperEntity = new PaperEntity("DEFAULT",true)
    //paperMention.citationString=entry.toString
    val iter = entry.getFields.keySet.iterator
    while(iter.hasNext){
      val name = filterFieldNameForMongo(iter.next.asInstanceOf[String].toLowerCase)
      val value = entry.getFieldValue(name)
      value match{
        //case x:BibtexConcatenatedValue => {x.toString}
        //case x:BibtexMacroReference => {x.toString}
        //case x:BibtexMultipleValues => {x.toString}
        //case x:BibtexPersonList => {x.toString}
        //case x:BibtexString => {x.toString}
        case x:BibtexPersonList =>{
          val authors = new ArrayBuffer[AuthorEntity]
          for(j<-0 until x.getList.size){
            //val authorMention = new AuthorMention
            //authorMention.paperMention = paperMention
            //authorMention.authorIndex = j
            val person = x.getList.get(j).asInstanceOf[BibtexPerson]
            val (first,middle) = splitFirst(person.getFirst)
            var last = person.getLast;if(last==null)last=""
            val authorEntity = new AuthorEntity(first,middle,last,true)
            authorEntity.paper = paperEntity
            //paperEntity.authorEntities += authorEntity
            //authorMention.authorIndex = j
            val labelTest = last.split("\\[")
            if(labelTest.length==2){
              if(labelTest(1).matches("[0-9]+\\]")){
                //authorentity.setClusterID(labelTest(1).substring(0,labelTest(1).length-1).toInt)
                last = labelTest(0)
              }
            }
            authorEntity.fullName.setLast(last)(null)
            authors += authorEntity//new AuthorMention(first,middle,last)
            //if(person.getLineage!=null)
            //  personObj.put(PersonRecord.LINEAGE,person.getLineage)
          }
          if(name == "editor"){}
          else if(name == "author")
            paperEntity.authors ++= authors
        }
        case x:BibtexAbstractValue =>{
          var xv=value.toString.replaceAll("[\r\n\t ]+"," ").replaceAll("[^A-Za-z0-9\\-\\.,\\(\\) ]","")
          if(name != "authors" && name != "editors" && name != "mentions" && name!="entity_ref"){
            if(xv.startsWith("{"))xv=xv.substring(1,xv.length-1)
            if(name == "title")paperEntity.title.set(xv)(null)
            if(name == "year"){
              xv = xv.replaceAll("[^0-9]","")
              if(xv.length==0)xv="-1"
              paperEntity.attr[Year] := xv.toInt
            }
          }

          //println("ENTRY TYPE: "+entryType)
          if (name == "journal"
            || (name == "booktitle" && entryType.indexOf("book") == -1)
            || (name == "institution" && (entryType=="techreport" || entryType.indexOf("thesis") != -1))){
            if(xv==null || xv.replaceAll(FeatureUtils.tokenFilterString,"").length>0){
              paperEntity.attr[VenueName]:=xv
              //val venueMention = new VenueMention
              //venueMention.name = xv
              //paperMention.venueMention = venueMention
            }
          }
        }
      }
    }
    if(paperEntity.title==null)paperEntity.title.set("")(null)
    //paperMention.createAllSingletonEntities
    paperEntity
  }

  val tokenizer = "[^A-Za-z0-9]"

  def main(args:Array[String]): Unit = {
    val opts = new HashMap[String,String]
    args.foreach((s:String) => {val sp=s.split("=");opts += sp(0) -> sp(1)})
    opts.foreach(p => println(p._1 + ": " + p._2))
    val dblpFile = opts.getOrElse("dblpFile", "/Users/mwick/data/dblp/small.xml")
    val numSteps = opts.getOrElse("numSteps","10000").toInt
    val dbName = opts.getOrElse("mongoDBName","rexa2-cubbie")
    val dbServer = opts.getOrElse("mongoServer","localhost")
    val dbPort = opts.getOrElse("mongoPort","27017").toInt
    val populateDB = opts.getOrElse("populateDB","true").toBoolean
    val dropDB = opts.getOrElse("dropDB","false").toBoolean
    val numToPop = opts.getOrElse("numToPop","10").toInt
    val numIterations = opts.getOrElse("numIterations","100").toInt
    println("Rexa2 database: "+dbName+"@"+dbServer+":"+dbPort)
    val rexa2 = new Rexa2(dbServer,dbPort,dbName)
//    rexa2.insertMentionsFromBibDir(new File("/Users/mwick/data/thesis/all3/"))
//    rexa2.insertMentionsFromBibDir(new File("/Users/mwick/data/thesis/rexa2/bibs/"))
//    rexa2.insertMentionsFromBibFile(new File("/Users/mwick/data/thesis/rexa2/test.bib"))
/*
    if(1+1==2){
      rexa2.drop
      rexa2.insertMentionsFromBibFile(new File("/Users/mwick/data/thesis/rexa2/labeled/fpereira.bib"))
    }else{
      if(dropDB)rexa2.drop
      if(populateDB)rexa2.insertMentionsFromDBLP(dblpFile)
    }
*/
    val model = new HierCorefModel
    val predictor = new AuthorSampler(model)
    for(i<-0 until numIterations){
      println("\n=============")
      println("BATCH NO. "+i)
      println("==============")
      var time = System.currentTimeMillis
      val mentions =rexa2.nextBatch(100)
      println("Loading " + mentions.size + " took " + (System.currentTimeMillis -time)/1000L+"s.")
      //for(m <- mentions)if(!m.isObserved) throw new Exception("DB is singletons, should be entirely mentions.")
      /*
    for(m <- mentions){
      println(this.entityString(m))
      println("   *properties:  (exists?="+m.isConnected+" mention?="+m.isObserved+" #children:"+m.subEntitiesSize+")")
    }*/
      println("Number of mentions: "+mentions.size)
      predictor.setEntities(mentions)
      time = System.currentTimeMillis
      predictor.process(numSteps)
      System.out.println(numSteps+" of inference took "+(System.currentTimeMillis-time)/1000L + "s.")
      //println("Entities:\n"+predictor.getEntities)
      println("\nPRINTING ENTITIES")
      printEntities(predictor.getEntities)
      checkIntegrity(predictor.getEntities)
      println("Inferred entities: "+predictor.getEntities.size)
      //predictor.performMaintenance
      rexa2.store((predictor.getEntities ++ predictor.getDeletedEntities).map(_.asInstanceOf[AuthorEntity]))
    }
  }

  def load20News(dir:java.io.File):ArrayBuffer[(String,ArrayBuffer[String])] ={
    var result = new ArrayBuffer[(String,ArrayBuffer[String])]
    for(subdir <- dir.listFiles){
      for(file <- subdir.listFiles){
        val tokens = new ArrayBuffer[String]
        for(line <- io.Source.fromFile(file).getLines){
          tokens ++= line.split("[^A-Za-z]+")
        }
        result += subdir.getName -> tokens
      }
    }
    result
  }

  def checkIntegrity(entities:Seq[Entity]):Unit ={
    var numErrors = 0
    for(e<-entities){
      if(e.isRoot && checkIntegrity(e))
        numErrors += 1
    }
    println("Number of errors: "+numErrors)
  }
  def checkIntegrity(e:Entity):Boolean ={
    var result = false
    //println("CHECKING INTEGRITY OF: "+entityString(e))
    if(e.isRoot && e.isConnected){
      val bag = new ComposableBagOfWords
      //println("  NUM DESC: "+e.descendantsOfClass[Entity].size)
      for(m <- e.descendantsOfClass[Entity]){
        if(m.isObserved || m.childEntitiesSize==0)
          bag.addBag(m.attr[BagOfCoAuthors].value)
      }
      if(e.isObserved)bag.addBag(e.attr[BagOfCoAuthors].value)
      bag.incorporateBags
      //println("  bag: "+bag)
      val actualBag = e.attr[BagOfCoAuthors].value.asHashMap
      //println("  act: "+actualBag)
      var count = 0.0
      for((k,v) <- actualBag){
        count += scala.math.abs(bag(k)-v)
      }
      if(count!=0.0){
        println("ERROR COUNT: "+count)
        println("computed exhaustively: "+bag)
        println("taken from entity    : "+actualBag)
        println("ENTITY\n")
        println(entityString(e))
        result=true
      }
    }
    //for(c <- e.childEntitiesIterator)if(checkIntegrity(c))result=true
    result
  }
  def printEntities(entities:Seq[Entity],includeSingletons:Boolean=true):Unit = {
    var count = 0
    for(e <- entities.filter((e:Entity) => {e.isRoot && e.isConnected})){
      if(!e.isObserved || includeSingletons)
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
  protected def entityString(e:Entity, result:StringBuffer, depth:Int=0):Unit = {
    for(i<-0 until depth)result.append("   ")
    //result.append(e.id+"-")
    if(e.isRoot){
      result.append("EntityRoot["+e.attr[FullName]+"]")
      //result.append("(exists?="+e.isConnected+" mention?="+e.isObserved+" #children:"+e.subEntitiesSize+")")
    }else if(e.isObserved){
      result.append("Mention["+e.attr[FullName]+"]")
      //result.append(" Title="+e.asInstanceOf[AuthorEntity].paper.title)
    }else{
      result.append("SubEntity["+e.attr[FullName]+"]")
      if(e.childEntitiesSize==0)result.append("-SUBENTITY ERROR")//throw new Exception("ERROR SUB ENTITY IS EMPTY")
      //if(e.subEntitySize==1)throw new Exception("ERROR SUB ENTITY HAS ONE CHILD)
    }
    result.append("{"+bagToString(e.attr[BagOfCoAuthors].value)+"}")
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

object FeatureUtils{
  //venue projections
  val venueP0 = "\\(.+\\)";
  val venueP1 = "(in )?[Pp]roceedings( of)?( the)?";
  val venueP2 = "[0-9\\-]+(th|st|rd)?"
  val venueP3 = "([Ee]leventh|[Tt]welfth|[Tt]hirteenth|[Ff]ourteenth|[Ff]ifteenth|[Ss]ixteenth|[Ss]eventeenth|[Ee]ighteenth|[Nn]ineteenth|[Tt]wentieth|[Tt]hirtieth|[Ff]ourtieth)?([Tt]wenty|[Tt]hirty|[Ff]orty)?[- ]?([Ff]irst|[Ss]econd|[Tt]hird|[Ff]ourth|[Ff]ifth|[Ss]ixth|[Ss]eventh|[Ee]ighth|[Nn]ineth)?"
  val venueP4 = "(in the )?[Pp]roceedings of (the )?[a-z0-9]+ "
  val venueP5 = "(([Aa]dvances( ?in ?)?|[Pp]roceedings|[Pp]roc\\.? )) ?"
  val venuePost = " ?([Ee]ndowment|[Ee]ndow|Proceedings|Meeting)\\.?"
  val venForAuthStops = "(proceedings|proc|endowment|endow|conference|in|the|of|[a-z]+eenth|[a-z]+tieth|first|second|third|fourth|fifth|sixth|seventh|eighth|nineth|tenth|eleventh|twelfth)"
  val tokenFilterString = "[^A-Za-z0-9]"
  def venueBag(s:String):Seq[String] = {val toks = new ArrayBuffer[String];toks++=tokenizeVenuesForAuthors(s);toks ++= getVenueAcronyms(s).map(_._1);toks.map(_.toLowerCase).toSeq}
  def tokenizeVenuesForAuthors(s:String):Seq[String] ={
    var filtered = s.toLowerCase.replaceAll("[^a-z ]","")
    filtered = filtered.replaceAll(venForAuthStops,"")
    filtered.split(" ")
  }
  def venueAKA(s:String) : String ={
    if(s == null) return null
    val beginIndex = s.indexOf("(")
    val endIndex = s.indexOf(")",beginIndex)
    if(endIndex>beginIndex)
      s.substring(beginIndex+1,endIndex).replaceAll(venueP2,"").toLowerCase
    else null
  }
  def filterVenue(s:String) : String = {
    if(s == null) return null
    else return s.replaceAll(venueP0,"").replaceAll(venueP4,"").replaceAll(venueP2,"").replaceAll(venueP5,"").replaceAll(tokenFilterString," ").replaceAll(" ","").toLowerCase
  }
  def getVenueAcronyms(s:String) : HashMap[String,Double] = {
    val result = new HashMap[String,Double]
    var initialsFromCaps = ""
    var split = s.split(" (of the|in) ")
    if(split.length>1)
      initialsFromCaps=split(split.length-1).replaceAll(venueP3,"").replaceAll(venueP5,"").replaceAll(venuePost,"").replaceAll("[^A-Z]","")
    else
      initialsFromCaps=s.replaceAll(venueP3,"").replaceAll(venueP5,"").replaceAll(venuePost,"").replaceAll("[^A-Z]","")
    val filtered = s.replaceAll("[^A-Za-z\\(\\) ]+","").replaceAll(venueP3,"").replaceAll(venueP5,"").replaceAll(venuePost,"").replaceAll("  +","")
    val aka = venueAKA(filtered)
    split = filtered.split(" ")
    if(split.length>1 && split(0).matches("[A-Z]+ "))
        result += split(0) -> 1
    if(aka!=null)
      result += aka.toUpperCase -> 1
    if(initialsFromCaps.length>1 && initialsFromCaps.length<8)
      result += initialsFromCaps.toUpperCase -> 1
    if(filtered.length<8 && filtered.matches("[A-Z]+"))
      result += filtered.toUpperCase -> 1
    if(filtered.length>=2 && filtered.length<=4 && filtered.matches("[A-Za-z]+"))
      result += filtered.toUpperCase -> 1
    val lcaseAcr = acroForLower(s)
    if(lcaseAcr!="")
      result += lcaseAcr -> 1
    for((k,v) <- result)
      if(k.replaceAll("[^A-Z]","").length==0)
        result.remove(k)
    split = s.toLowerCase.split("[^A-Za-z]")
    //for(sp<-tokenizeVenuesForAuthors(s))result += sp -> 1
    result
  }
  def acroForLower(s:String) : String ={
    var str = s.toLowerCase.replaceAll("[a-z0-9 ]","")
    str = str.replaceAll("(a|the|of) ","")
    str = str.replaceAll("  +","")
    var split = str.split("(proceedings|journal|proc) ")
    var finalString:String = if(split.length==1) split(0) else split(split.length-1)
    split = finalString.split(" ")
    var result = ""
    if(split.length>=3 && split.length <=5){
    for(init<-split)
      result += init.charAt(0)
    }
    result.toUpperCase
  }
}

/*** Consider a set-up where cosine distance is computed between arrays of bags of words, and each bag is a variable that can go on the diff list.*/
/**Basic trait for doing operations with bags of words*/
trait BagOfWords{ // extends scala.collection.Map[String,Double]{
  //def empty: This
  def size:Int
  def asHashMap:HashMap[String,Double]
  def apply(word:String):Double
  def iterator:Iterator[(String,Double)]
  def l2Norm:Double
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
  protected var _bag = new HashMap[String,Double]
  if(initialWords!=null)for(w<-initialWords)this += (w,1.0)
  if(initialBag!=null)for((k,v)<-initialBag)this += (k,v)
  def l2Norm = _l2Norm
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
    _l2Norm += w*w + 2*this(s)*w
    _bag(s) = _bag.getOrElse(s,0.0) + w
  }
  def -= (s:String, w:Double=1.0):Unit ={
    _l2Norm += w*w - 2.0*this(s)*w
    if(w == _bag(s))_bag.remove(s)
    else _bag(s) = _bag.getOrElse(s,0.0) - w
  }
  def addBag(that:BagOfWords) = for((k,v) <- that.iterator) this += (k,v)
  def removeBag(that:BagOfWords) = for((k,v) <- that.iterator)this -= (k,v)
}

trait ProposeAndCombineBags extends BagOfWords{
  def incorporateBags:Unit
  def asHashMap:HashMap[String,Double]
}
/**Efficient implementation for bags of words where proposals temporarily hypothesize new bags, and these proposals are efficiently undone/redone.*/
class ComposableBagOfWords(initialWords:Iterable[String]=null,initialBag:Map[String,Double]=null) extends ProposeAndCombineBags{
  val _bag = new HashMap[String,Double]
  var addBag:Option[BagOfWords] = None
  var removeBag:Option[BagOfWords] = None
  protected var _l2Norm2 = 0.0

  if(initialWords!=null)for(w<-initialWords)this += (w,1.0)
  if(initialBag!=null)for((k,v)<-initialBag)this += (k,v)
  override def toString = {
    getCombinedBag.toString
    //"l2 norm: "+l2Norm + " l2 nmBF: "+this.l2NormBruteForce+":"+getCombinedBag.toString
  } //_bag.toString
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
    //println("removeBag")
    //println("  that: "+that)
    //println("  addBag: "+addBag)
    //println("  remBag: "+removeBag)
    if(addBag!=None && addBag.get.eq(that))addBag=None
    else if(removeBag == None)removeBag = Some(that)
    else this --= that
  }
  def asHashMap = getCombinedBag
  def getCombinedBag:HashMap[String,Double] ={
    val combined = new HashMap[String,Double]
    for((k,v)<-_bag.iterator)combined(k) = combined.getOrElse(k,0.0)+v
    for(bag<-addBag)for((k,v)<-bag.iterator)combined(k) = combined.getOrElse(k,0.0)+v
    for(bag<-removeBag)for((k,v)<-bag.iterator)combined(k) = combined.getOrElse(k,0.0)-v
    val result = new HashMap[String,Double]
    result ++= combined.filter(_._2 != 0.0)
    //result ++= combined.filter((k,v):(String,Double) => {v != 0.0})
  }
  def size = {var r =_bag.size;for(b<-addBag)r+=b.size;for(b<-removeBag)r+=b.size;r}
  def iterator = getCombinedBag.iterator //this will be slow
  def apply(s:String):Double = {
    var result = _bag.getOrElse(s,0.0)
    for(bag<-addBag)result += bag(s)
    for(bag<-removeBag)result -= bag(s)
    result
  }
  def contains(s:String):Boolean = _bag.contains(s)
  def *(that:BagOfWords):Double = {
    //if(this.size > that.size)return that * this
    var result = 0.0
    for((word,weight) <- _bag.iterator)
      result += weight * that(word)
    for(bag<-addBag)result += bag  * this
    for(bag<-removeBag)result -= bag * this
    result
  }
  protected def timesCore(that:BagOfWords):Double = {var result = 0.0;for((word,weight) <- _bag.iterator)result += weight * that(word);result}
  protected def timesCore(that:Option[BagOfWords]):Double = if(that==None)0.0 else this.timesCore(that.get)

  def +=(s:String,w:Double=1.0):Unit ={
//    println(" (before) "+_l2Norm2)
//    println(" (before) "+ l2Norm*l2Norm)
//    println(" (before) "+_bag)
//    println(" (before) "+ this.getCombinedBag)
//    val addL2Norm2 = getOptionBagL2Norm2(addBag)
//    val removeL2Norm2 = getOptionBagL2Norm2(removeBag)
//    println("    _addBag: " + addBag)
//    println("    _removeBag: "+ removeBag)
//    println("      _l2Norm2   : " + _l2Norm2)
//    println("      _addL2Norm2: " + addL2Norm2)
//    println("      _remL2Norm2: " + removeL2Norm2)
//    val r = _l2Norm2 + addL2Norm2 + removeL2Norm2 + 2 * (this.timesCore(addBag) - this.timesCore(removeBag)  - mult (addBag,removeBag))
//    println("      _result2: " + r)

//    println("  w: "+w)
//    println("  this(s): "+ this(s))
    _l2Norm2 += w*w + 2*_bag.getOrElse(s,0.0)*w
    if(_bag.getOrElse(s,0.0) == -w)_bag.remove(s)
    else _bag(s) = _bag.getOrElse(s,0.0) + w

//    _bag(s) = _bag.getOrElse(s,0.0)+w
//    println(" (after) "+_l2Norm2)
//    println(" (after) "+l2Norm*l2Norm)
//    println(" (after) "+_bag)
//    println(" (after) "+ this.getCombinedBag)

  }
  def -=(s:String,w:Double=1.0):Unit ={
    _l2Norm2 +=  w*w - 2*_bag.getOrElse(s,0.0)*w
    if(_bag.getOrElse(s,0.0)==w)_bag.remove(s)
    else _bag(s) = _bag.getOrElse(s,0.0) - w
  }
  def l2Norm:Double ={
    var result = _l2Norm2
    if(addBag!=None && this.eq(addBag.get))throw new Exception("Cannot add myself.")
    val addL2Norm2 = getOptionBagL2Norm2(addBag)
    val removeL2Norm2 = getOptionBagL2Norm2(removeBag)
//    println("_bag: " + _bag)
//    println("_addBag: " + addBag)
//    println("_removeBag: "+ removeBag)
//    println("  _l2Norm2   : " + _l2Norm2)
//    println("  _addL2Norm2: " + addL2Norm2)
//    println("  _remL2Norm2: " + removeL2Norm2)
    //result += addL2Norm2 + removeL2Norm2 + 2 * (this * addBag - this * removeBag  - mult (addBag,removeBag))
    result += addL2Norm2 + removeL2Norm2 + 2 * (this.timesCore(addBag) - this.timesCore(removeBag)  - mult (addBag,removeBag))

//    println("  _result2: " + result)
//    println("  _result : " + scala.math.sqrt(result))
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

class BagOfWordsCubbie extends Cubbie{
  //_map = new HashMap[String,Any]
  val words = StringListSlot("words")
  val weights = DoubleListSlot("weights")
  def store(bag:BagOfWords):BagOfWordsCubbie ={
    words := bag.iterator.map(_._1).toSeq
    weights := bag.iterator.map(_._2).toSeq
    this
  }
  def fetch:HashMap[String,Double] ={
    val result = new HashMap[String,Double]
    val wordSeq = words.value
    val weightSeq = weights.value
    for(i<-0 until wordSeq.size)result += wordSeq(i) -> weightSeq(i)
    result
  }
}
//trait BagOfWordsVar extends Variable with VarAndValueGenericDomain[BagOfWordsVar,ProposeAndCombineBags] with Iterable[(String,Double)]
//class BagOfWordsVariable(initialWords:Iterable[String]=Nil,initialMap:Map[String,Double]=null) extends BagOfWordsVar with VarAndValueGenericDomain[BagOfWordsVariable,ProposeAndCombineBags] {
trait BagOfWordsVar extends Variable with VarAndValueGenericDomain[BagOfWordsVar,SparseBagOfWords] with Iterable[(String,Double)]
class BagOfWordsVariable(initialWords:Iterable[String]=Nil,initialMap:Map[String,Double]=null) extends BagOfWordsVar with VarAndValueGenericDomain[BagOfWordsVariable,SparseBagOfWords] {
  // Note that the returned value is not immutable.
  def value = _members
  private val _members:SparseBagOfWords = {
    val result = new SparseBagOfWords(initialWords)
    if(initialMap!=null)for((k,v) <- initialMap)result += (k,v)
    result
  }
  /*
  def reset()(implicit d:DiffList):Unit = {
    _members.incorporateBags
    val copy = _members.asHashMap
    for((k,v) <- copy)add(k,v)(d)
    if(_members.l2Norm!=0.0)throw new Exception("ERROR L2 NORM IS "+l2Norm)
  }*/
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

object DBLPLoader{
  def loadDBLPData(location:String) : Seq[PaperEntity] ={
    val REFERENCE_ELEMENT = "article"
    val docFactory:DocumentBuilderFactory = DocumentBuilderFactory.newInstance
    val docBuilder:DocumentBuilder = docFactory.newDocumentBuilder
    val file:File = new File(location)
    if(!file.exists()) error("Couldn't find file " + location + ", aborting load.")
    val doc:Document = docBuilder.parse(file)
    val nodes:NodeList = doc.getElementsByTagName("dblp").item(0).getChildNodes//doc.getElementsByTagName(REFERENCE_ELEMENT);
    println("Num nodes: "+nodes.getLength)
    var numArticles=0
    var numConfs=0
    val result = new ArrayBuffer[PaperEntity]
    for(i <- 0 until nodes.getLength){
      val node:Node = nodes.item(i)
      val bibtexReferenceType = node.getNodeName //\in {article,inproceedings,proceedings}
      //println(" -bibReferenceType: "+bibtexReferenceType)
      if(filterBib(bibtexReferenceType)){
        val paperMention = new PaperEntity
        paperMention.flagAsMention
        result += paperMention
        bibtexReferenceType match{
          case "article" => {addFieldsForDBLP(node,paperMention);numArticles+=1}
          case "inproceedings" => {addFieldsForDBLP(node,paperMention);numConfs+=1}
          case _ => {}
        }
        if((numArticles+numConfs) % 10000==0){
          val total = numArticles+numConfs
          println("Loaded: "+total)
          //println("Loaded: "+numArticles+" journal articles and "+numConfs+" conference papers. Total: "(numArticles+numConfs))
        }
      }
    }
    result
  }
  protected def filterBib(s:String):Boolean = (s=="article" || s=="inproceedings")
  protected def addFieldsForDBLP(node:Node,paperMention:PaperEntity) : Unit ={
    val nodes = node.getChildNodes
    for(i<-0 until nodes.getLength){
      val node = nodes.item(i)
      val text = node.getTextContent
      //println("    Node name: "+node.getNodeName)
      //println("    Node text: "+text)
      node.getNodeName match{
        //all fields
        case "author" => {
          for(j<-0 until node.getChildNodes.getLength){
            val authorMention = new AuthorEntity
            authorMention.flagAsMention
            authorMention.fullName.setFirst("")(null);authorMention.fullName.setMiddle("")(null);authorMention.fullName.setLast("")(null)
            paperMention.authors += authorMention
            authorMention.paper = paperMention
            val authorNode = node.getChildNodes.item(i)
            val split = text.replaceAll("(Jr\\.|Sr\\.)","").split(" ")
            if(split.length>1){
              authorMention.fullName.setFirst(split(0))(null)
              authorMention.fullName.setLast(split(split.length-1))(null)
              if(split.length>2){
                var middle = ""
                for(k<-1 until split.length-1)middle += split(k)
                authorMention.fullName.setMiddle(middle)(null)
              }
            }else {
              authorMention.fullName.setLast(split(0))(null)
            }
          }
        }
        case "title" => paperMention.title.set(text)(null)
        case "year" => paperMention.year.set(text.toInt)(null)
        case "pages" => {}
        //@inproceedings fields
        case "booktitle" => {paperMention.venueName.set(text)(null)}
        //@article fields
        case "journal" => {paperMention.venueName.set(text)(null)}
        case "month" => {}
        case "volume" => {}
        case _ => {}
      }
    }
  }
}
class DebugDiffList extends DiffList{
  override def scoreAndUndo(model:Model): Double = {
    println("--v---------------------------------------v--")
    println("DEBUGDIFFLIST: "+this.length)
    if (this.length == 0) return 0.0  // short-cut the simple case
    println("SCORING Y\'")
    var s = model.score(this)
    println("    **Y\' score: "+s)
    //log(Log.DEBUG)("DiffList scoreAndUndo  pre-undo score=" + s)
    this.undo
    println("\nSCORING Y")
    // We need to re-calculate the Factors list because the structure may have changed
    val s2=model.score(this)
    println("   **Y  score:"+s2)
    s -= s2
    println(" ***diff score: "+s+"***")
    println("--^---------------------------------------^--")
    //log(Log.DEBUG)("DiffList scoreAndUndo post-undo score=" + s)
    s
  }
}
