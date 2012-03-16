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
import example.Coref3.Year
import scala.util.parsing.combinator.Parsers
import java.io.{InputStreamReader, FileInputStream, BufferedReader, File}
/**
 * Consider a set-up where cosine distance is computed between arrays of bags of words, and each bag is a variable that can go on the diff list.
 */

class FullName(val entity:Entity,f:String,m:String,l:String) extends SeqVariable[String](Seq(f,m,l)){
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
object Coref3 {
  trait HasCanopyAttributes[T<:Entity]{
    val canopyAttributes = new ArrayBuffer[CanopyAttribute[T]]
  }
  trait CanopyAttribute[T<:Entity]{def entity:T;def canopyName:String}
  class AuthorFLNameCanopy(val entity:AuthorEntity) extends CanopyAttribute[AuthorEntity]{
    def canopyName:String=(initial(entity.fullName.firstName)+entity.fullName.lastName).toLowerCase
    def initial(s:String):String = if(s!=null && s.length>0)s.substring(0,1) else ""
  }
  /**Attributes/Features of entities*/
  class Bow(val entity:Entity,ss:Iterable[String]=Nil) extends BagOfWordsVariable(ss)
  /**Attributes specific to REXA authors*/
  class Title(val entity:Entity,title:String) extends StringVariable(title)
  class Year(val entity:Entity,year:Int) extends IntegerVariable(year)
  class VenueName(val entity:Entity,venueName:String) extends StringVariable(venueName)
  class Bags extends HashMap[String,BagOfWords]
  class BagOfTopics(val entity:Entity, topicBag:Map[String,Double]=null) extends BagOfWordsVariable(Nil, topicBag)
  class BagOfVenues(val entity:Entity, venues:Map[String,Double]=null) extends BagOfWordsVariable(Nil, venues)
  class BagOfCoAuthors(val entity:Entity,coAuthors:Map[String,Double]=null) extends BagOfWordsVariable(Nil, coAuthors)
  /**Entity variables*/
  /**An entity with the necessary variables/coordination to implement hierarchical coreference.*/
/*
  object HierarchicalEntity{
    def structurePreservationForEntityThatLostChild(e:Entity)(implicit d:DiffList):Unit ={
      if(e!=null && e.subEntitiesSize<=1){
        for(subEntity <- e.subEntities)
          subEntity.setSuperEntity(e.superEntity)
        e.setSuperEntity(null)(d)
      }
    }
    def propagateBagUp[C<:BagOfWordsVariable](added:Entity,entity:Entity,d:DiffList)(implicit m:Manifest[C]):Unit ={
      var e = entity
      if(e!=null){
        e.attr[C].add(added.attr[C].value)(d)
        e = e.superEntity
      }
    }
    def propagateBagUp[C<:BagOfWordsVariable](entity:Entity,d:DiffList)(implicit m:Manifest[C]):Unit ={
      var e = entity.superEntity
      while(e!=null){
        e.attr[C].add(entity.attr[C].value)(d)
        e = e.superEntity
      }
    }
    def propagateRemoveBag[C<:BagOfWordsVariable](partingChild:Entity,d:DiffList)(implicit m:Manifest[C]):Unit ={
      var e = partingChild.superEntity //formerqq qParent
      while(e!=null){
        e.attr[C].remove(partingChild.attr[C].value)(d)
        e = e.superEntity
      }
    }
    def propagateRemoveBag[C<:BagOfWordsVariable](removed:Entity,entity:Entity,d:DiffList)(implicit m:Manifest[C]):Unit ={
      var e = entity //formerqq qParent
      if(e!=null){
        e.attr[C].remove(removed.attr[C].value)(d)
        e = e.superEntity
      }
    }
  }
  */
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
  var nextId = -1
  var entityCount = 0
  class AuthorEntity(f:String="DEFAULT",m:String="DEFAULT",l:String="DEFAULT", isMention:Boolean = false) extends HierEntity(isMention) with HasCanopyAttributes[AuthorEntity]{
    //override def id = {entityCount+=1;entityCount-1}
    //println("id: "+id)
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
    //_map = new HashMap[String,Any]
    //val name = new StringListSlot("name")
    val firstName = new StringSlot("firstName")
    val middleName = new StringSlot("middleName")
    val lastName = new StringSlot("lastName")
    val bagOfTopics = new CubbieSlot("bagOfTopics", () => new BagOfWordsCubbie)
    val bagOfVenues = new CubbieSlot("bagOfVenues", () => new BagOfWordsCubbie)
    val bagOfCoAuthors = new CubbieSlot("bagOfCoAuthors", () => new BagOfWordsCubbie)
    val canopies = new StringListSlot("canopies")
    val priority = new DoubleSlot("priority")
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
      //val result = new BagOfWordsVariable
      val result = new HashMap[String,Double]
      val wordSeq = words.value
      val weightSeq = weights.value
      for(i<-0 until wordSeq.size)result += wordSeq(i) -> weightSeq(i)
      result
    }
  }

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
        if(initialsMisMatch(childFirst,parentFirst))result -=4
        if(initialsMisMatch(childMiddle,parentMiddle))result -= 4
        if(nameMisMatch(childFirst,parentFirst))result -= 4
        //var result = -cc.factorie.app.strings.editDistance(childString, parentString)
        //println("EntityName:"+result)
        result
      }
      def initialsMisMatch(c:String,p:String):Boolean = (c!=null && p!=null && c.length>0 && p.length>0 && c.charAt(0)!=p.charAt(0))
      def nameMisMatch(c:String,p:String):Boolean = (c!=null && p!=null && c.length>1 && p.length>1 && c != p)
    },
    // compatibility between parent/child bows
    new TemplateWithStatistics3[EntityRef,BagOfCoAuthors,BagOfCoAuthors] {
      def unroll1(er:EntityRef) = if(er.dst!=null)Factor(er, er.src.attr[BagOfCoAuthors], er.dst.attr[BagOfCoAuthors]) else Nil
      def unroll2(childBow:BagOfCoAuthors) = if(childBow.entity.parentEntity!=null)Factor(childBow.entity.parentEntityRef, childBow, childBow.entity.parentEntity.attr[BagOfCoAuthors]) else Nil
      def unroll3(parentBow:BagOfCoAuthors) = for(e<-parentBow.entity.childEntities) yield Factor(e.parentEntityRef,e.attr[BagOfCoAuthors],parentBow)
      def score(s:Stat): Double = {
        val childBow = s._2
        val parentBow = s._3
        //childBow.intersect(parentBow).size
        var result = childBow.cosineSimilarity(parentBow)
        //println("Bow: "+result)
        //println("result: "+result)
        //println("  bag1:"+childBow)
        //println("  bag2:"+parentBow)
        result
      }
    },
    new TemplateWithStatistics3[EntityRef,BagOfVenues,BagOfVenues] {
      def unroll1(er:EntityRef) = if(er.dst!=null)Factor(er, er.src.attr[BagOfVenues], er.dst.attr[BagOfVenues]) else Nil
      def unroll2(childBow:BagOfVenues) = if(childBow.entity.parentEntity!=null)Factor(childBow.entity.parentEntityRef, childBow, childBow.entity.parentEntity.attr[BagOfVenues]) else Nil
      def unroll3(parentBow:BagOfVenues) = for(e<-parentBow.entity.childEntities) yield Factor(e.parentEntityRef,e.attr[BagOfVenues],parentBow)
      def score(s:Stat): Double = {
        val childBow = s._2
        val parentBow = s._3
        //childBow.intersect(parentBow).size
        var result = childBow.cosineSimilarity(parentBow)
        //println("Bow: "+result)
        //println("result: "+result)
        //println("  bag1:"+childBow)
        //println("  bag2:"+parentBow)
        result
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
    new TemplateWithStatistics3[EntityExists,IsEntity,IsMention]{
      val entityExistenceCost = 2.0 //8
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
        //println("STRUCTURE PRIOR: "+result)
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
      /*for(e<-ents){
        val cname = e.defaultCanopy
        canopies.getOrElse(cname,{val a = new ArrayBuffer[AuthorEntity];canopies(cname)=a;a}) += e
      }*/
      println("Number of canopies: "+canopies.size)
      for((k,v) <- canopies)println("  -"+k+":"+v.size)
    }
    /*
    def testIntegrity:Unit ={
      for(e<-entities){
        if(!e.isRoot && !e.isObserved && e.subEntitiesSize==0)throw new Exception("ERROR")
        if(e.isRoot)testIntegrity(e)
      }
    }
    def testIntegrity(e:Entity){
      if(!e.isRoot && !e.isObserved && e.subEntitiesSize==0)throw new Exception("ERROR SUB")
      for(c<-e.subEntitiesIterator)testIntegrity(c)
    }
    */
    def sampleAttributes(author:AuthorEntity)(implicit d:DiffList) = {
      val representative = author.childEntities.sampleUniformly(random)
      author.attr[Dirty].reset
      if(author.parentEntity != null)author.parentEntity.attr[Dirty].++()(d)
      //println("RESAMPLING ATTRIBUTE")
      /*
      author.fullName.set(representative.fullName)
      author.bagOfCoAuthors.reset
      author.bagOfVenues.reset
      for(c<-author.subEntities){
        author.bagOfCoAuthors ++= c.attr[BagOfCoAuthors]
        author.bagOfVenues ++= c.attr[BagOfVenues]
      }
      */
    }
    override def nextEntity(context:AuthorEntity=null):AuthorEntity={
      var result = if(context==null)sampleEntity(entities) else sampleEntity(canopies(context.defaultCanopy))
      /*
      if(context!=null){
        println("canopy size: "+canopies(context.defaultCanopy).size)
        println("canopy name: "+context.defaultCanopy)
        println("e1: "+context.attr[FullName].lastName)
        println("e2: "+result.attr[FullName].lastName)
      }
      */
      result
    }
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
      result.attr[FullName].setFullName(e1.attr[FullName])
      //result.attr[BagOfTopics].add(e1.attr[BagOfTopics].value)(d)
      //result.attr[BagOfTopics].add(e2.attr[BagOfTopics].value)(d)
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
        e = e.parentEntity
      }
    }
    def propagateRemoveBag(parting:Entity,formerParent:Entity)(implicit d:DiffList):Unit ={
      var e = formerParent
      while(e!=null){
        e.attr[BagOfCoAuthors].remove(parting.attr[BagOfCoAuthors].value)
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
      //println("JUMPS")
      //for(p <- proposals){
        //if(p eq result)print("  *") else print("   ")
        //println("SCORE: "+p.modelScore)
      //}
      testIntegrity
      result
    }*/
  }

  class Rexa2(mongoServer:String="localhost",mongoPort:Int=27017,mongoDBName:String="rexa2-cubbie"){
    protected var cache = new CubbieRefs
    protected var authorCache = new HashMap[Any,AuthorEntity]
    protected val mongoConn = new Mongo(mongoServer,mongoPort)
    protected val mongoDB = mongoConn.getDB(mongoDBName)
    protected val authors = {
      val coll = mongoDB.getCollection("authors")
      coll.drop()
      //new MongoCubbieCollection(coll,() => new AuthorEntityCubbie)
      new MongoCubbieCollection(coll,() => new AuthorEntityCubbie,(a:AuthorEntityCubbie) => Seq(Seq(a.canopies),Seq(a.priority),Seq(a.entityRef)))
    }
    def populateREXAFromDir(bibDir:File):Unit ={
      for(f<-bibDir.listFiles)populateREXA(f)
    }
    def populateREXA(bibFile:File):Unit ={
      import MongoCubbieConverter._
      val paperEntities = loadBibTeXFile(bibFile)
      for(paper <- paperEntities){
        for(author <- paper.authors){
          addFeatures(author)
          authors += new AuthorEntityCubbie(author)
        }
      }
    }
    /*
    def nextBatch:Seq[AuthorEntity]={
      cache = new CubbieRefs
      authorCache = new HashMap[Any,AuthorEntity]
      var topk = topK(100)
      var result = new ArrayBuffer[]
      for(author <- topk)
    }
    protected def topK(k:Int):Seq[AuthorEntity]={
      var count = 0
      var result = new ArrayBuffer[AuthorEntity]
      val iter = authors.coll.find().sort(Map("priority" -> -1))
      while(count<k && iter.hasNext){
        result += iter.next
        count += 1
      }
    }
  */
    def nextBatch:Seq[AuthorEntity]={
      cache = new CubbieRefs
      val result = for(authorCubbie<-authors) yield authorCubbie.fetchAuthorEntity(cache)
      result.toSeq
    }
    def store(entitiesToStore:Iterable[AuthorEntity]):Unit ={
      changePriorities(entitiesToStore)
      val deletedByInference = entitiesToStore.filter(!_.isConnected)
      val updatedOrAddedByInference = entitiesToStore.filter(_.isConnected)
      for(deleted <- deletedByInference)authors.updateDelta(cache.getOrElse(deleted.id,null).asInstanceOf[AuthorEntityCubbie],null)//todo: modify api to mongo cubbies to delete
      for(updatedOrAdded <- updatedOrAddedByInference)authors.updateDelta(cache.getOrElse(updatedOrAdded.id,null).asInstanceOf[AuthorEntityCubbie],new AuthorEntityCubbie(updatedOrAdded)) //update delta code to handle null
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


  def main(args:Array[String]): Unit = {
    val numSteps=500000
    val rexa2 = new Rexa2
    //rexa2.populateREXAFromDir(new File("/Users/mwick/data/thesis/all3/"))
    //rexa2.populateREXAFromDir(new File("/Users/mwick/data/thesis/rexa2/bibs/"))
    rexa2.populateREXA(new File("/Users/mwick/data/thesis/rexa2/labeled/fpereira.bib"))
    var time = System.currentTimeMillis
    val mentions =rexa2.nextBatch
    println("Loading " + mentions.size + " took " + (System.currentTimeMillis -time)/1000L+"s.")
    //for(m <- mentions)if(!m.isObserved) throw new Exception("DB is singletons, should be entirely mentions.")
    /*
    for(m <- mentions){
      println(this.entityString(m))
      println("   *properties:  (exists?="+m.isConnected+" mention?="+m.isObserved+" #children:"+m.subEntitiesSize+")")
    }*/
    println("Coref mentions: "+data)
    println("Number of mentions: "+mentions.size)
    val model = new HierCorefModel
    val predictor = new AuthorSampler(model)
    predictor.setEntities(mentions)
    time = System.currentTimeMillis
    predictor.process(numSteps)
    System.out.println(numSteps+" of inference took "+(System.currentTimeMillis-time)/1000L + "s.")
    //println("Entities:\n"+predictor.getEntities)
    println("\nPRINTING ENTITIES")
    printEntities(predictor.getEntities)
    println("Inferred entities: "+predictor.getEntities.size)
    //predictor.performMaintenance
    rexa2.store((predictor.getEntities ++ predictor.getDeletedEntities).map(_.asInstanceOf[AuthorEntity]))
//    Entities.store((predictor.getEntities ++ predictor.getDeletedEntities).map(_.asInstanceOf[MyEntity]))
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
  protected def entityString(e:Entity, result:StringBuffer, depth:Int=0):Unit = {
    for(i<-0 until depth)result.append("   ")
    //result.append(e.id+"-")
    if(e.isRoot){
      result.append("EntityRoot["+e.attr[FullName]+"]")
      //result.append("(exists?="+e.isConnected+" mention?="+e.isObserved+" #children:"+e.subEntitiesSize+")")
    }else if(e.isObserved){
      result.append("Mention["+e.attr[FullName]+"]")
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
    if(denominator==0.0) 0.0 else numerator/denominator
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
class BagOfWordsVariable(initialWords:Iterable[String]=Nil,initialMap:Map[String,Double]=null) extends BagOfWordsVar with VarAndValueGenericDomain[BagOfWordsVariable,ProposeAndCombineBags] {
  // Note that the returned value is not immutable.
  def value = _members
  private val _members:ProposeAndCombineBags = {
    val result = new ComposableBagOfWords(initialWords)
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
