package cc.factorie.app.bib
import cc.factorie._
import app.nlp.coref._
import db.mongo._
import com.mongodb.{DB, Mongo}
import collection.mutable.{HashSet, HashMap, ArrayBuffer}
import bibtex.parser.BibtexParser
import bibtex.expansions.PersonListExpander
import java.io.{FileInputStream, InputStreamReader, BufferedReader, File}
import bibtex.dom._
import javax.xml.parsers.{DocumentBuilder, DocumentBuilderFactory}
import org.w3c.dom.{Node, NodeList, Document}

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
/*
class NameBag(val entity:Entity,authorBag:Map[String,Double]=null) extends BagOfWordsVariable(Nil,authorBag) with EntityAttr{
  def canonicalName:String=""
}
class FullNameBag(val entity:Entity) extends Variable with EntityAttr{
  def domain = GenericDomain
  val firstName = new NameBag(entity)
  val middleName = new NameBag(entity)
  val lastName = new NameBag(entity)
  val suffix = new NameBag(entity)
}
*/
//paper attributes
class Title(val entity:Entity,title:String) extends StringVariable(title) with EntityAttr
class Year(val entity:Entity,year:Int) extends IntegerVariable(year) with EntityAttr
class VenueName(val entity:Entity,venueName:String) extends StringVariable(venueName) with EntityAttr
class BagOfAuthors(val entity:Entity,authorBag:Map[String,Double]=null) extends BagOfWordsVariable(Nil,authorBag) with EntityAttr
class Kind(val entity:Entity, kind:String) extends StringVariable(kind) with EntityAttr
class PromotedMention(val entity:Entity,id:String) extends StringVariable(id) with EntityAttr
//author attributes
class BagOfTopics(val entity:Entity, topicBag:Map[String,Double]=null) extends BagOfWordsVariable(Nil, topicBag) with EntityAttr
class BagOfVenues(val entity:Entity, venues:Map[String,Double]=null) extends BagOfWordsVariable(Nil, venues) with EntityAttr
class BagOfCoAuthors(val entity:Entity,coAuthors:Map[String,Double]=null) extends BagOfWordsVariable(Nil, coAuthors) with EntityAttr
class BagOfEmails(val entity:Entity,keywords:Map[String,Double]=null) extends BagOfWordsVariable(Nil,keywords) with EntityAttr
//misc attributes
class BagOfKeywords(val entity:Entity,keywords:Map[String,Double]=null) extends BagOfWordsVariable(Nil,keywords) with EntityAttr
/**Entity variables*/
/**An entity with the necessary variables/coordination to implement hierarchical coreference.*/

class PaperEntity(s:String="DEFAULT",isMention:Boolean=false) extends HierEntity(isMention) with HasCanopyAttributes[PaperEntity] with Prioritizable{
  var _id = java.util.UUID.randomUUID.toString+""
  override def id = _id
  canopyAttributes += new PaperTitleCanopy(this)
  protected def outer:PaperEntity = this
  attr += new Title(this,s)
  attr += new Year(this,-1)
  attr += new VenueName(this,"")
  attr += new BagOfAuthors(this)
  attr += new BagOfKeywords(this)
  attr += new PromotedMention(this, "NOT-SET")
  def title = attr[Title]
  def year = attr[Year]
  def venueName = attr[VenueName]
  def bagOfAuthors = attr[BagOfAuthors]
  def bagOfKeywords = attr[BagOfKeywords]
  def promotedMention = attr[PromotedMention]
  def string = title.toString
  val authors = new ArrayBuffer[AuthorEntity]{
    override def += (a:AuthorEntity) = {
      a.paper=outer
      a.paperMentionId=outer.id
      println("paperMention: "+outer.id)
      super.+=(a)
    }
  }
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
  attr += new BagOfEmails(this)
  def fullName = attr[FullName]
  def bagOfTopics = attr[BagOfTopics]
  def bagOfVenues = attr[BagOfVenues]
  def bagOfCoAuthors = attr[BagOfCoAuthors]
  def bagOfKeywords = attr[BagOfKeywords]
  def bagOfEmails = attr[BagOfEmails]
  def string = f+" "+m+" "+l
  var paper:PaperEntity = null
  var paperMentionId:String=null
}

object Coref{
  def main(args:Array[String]) ={
    val numEpochs=1
    val batchSize=1000
    val stepMultiplierA = 0.0
    val stepMultiplierB = 0.0//200.0
    val stepMultiplierC = 100.0 //100.0

    val epiDB = new EpistemologicalDB
    epiDB.drop
    //epiDB.insertMentionsFromBibFile(new File("/Users/mwick/data/thesis/rexa2/labeled/fpereira.bib"))
    epiDB.insertMentionsFromBibFile(new File("/Users/mwick/data/thesis/rexa2/labeled/single-promotion-test.bib"))
    epiDB.inferenceSweep(numEpochs,batchSize,stepMultiplierA,stepMultiplierB,stepMultiplierC)
  }
}

class EpistemologicalDB(mongoServer:String="localhost",mongoPort:Int=27017,mongoDBName:String="rexa2-cubbie") extends MongoBibDatabase(mongoServer,mongoPort,mongoDBName){
  val authorPredictor = new AuthorSampler(new AuthorCorefModel)
  val paperPredictor = new PaperSampler(new PaperCorefModel)
  def drop:Unit = {
    authorColl.drop
    paperColl.drop
  }
  protected def infer[E<:HierEntity with HasCanopyAttributes[E] with Prioritizable,C<:EntityCubbie[E]](entityCollection:EntityCollection[E,C],predictor:HierCorefSampler[E],k:Int,inferenceSteps:Int=>Int) ={
    val entities = entityCollection.nextBatch(k)
    predictor.setEntities(entities)
    predictor.process(inferenceSteps(entities.size))
    entityCollection.store(predictor.getEntities ++ predictor.getDeletedEntities.map(_.asInstanceOf[E]))

  }
  def inferenceSweep(numEpochs:Int,k:Int,a:Double,b:Double,c:Double) ={
    def calculateSteps(numEntities:Int) = (numEntities.toDouble*numEntities.toDouble*a + numEntities.toDouble*b + c).toInt
    for(i<-0 until numEpochs){
      println("INFERENCE ROUND "+i)
      println("Paper coreference")
      //infer[PaperEntity,PaperCubbie](paperColl,paperPredictor,k,calculateSteps(_))
      val papers = paperColl.nextBatch(k)
      paperPredictor.setEntities(papers)
      paperPredictor.process(calculateSteps(papers.size))
      val processedPapers = paperPredictor.getEntities ++ paperPredictor.getDeletedEntities.map(_.asInstanceOf[PaperEntity])
      for(paper <- processedPapers)paperPredictor.chooseCanonicalMention(paper)(null)
      paperColl.store(processedPapers)
      EntityUtils.printPapers(paperPredictor.getEntities)
      println("Author coreference")
      infer[AuthorEntity,AuthorCubbie](authorColl,authorPredictor,k,calculateSteps(_))
      EntityUtils.printAuthors(authorPredictor.getEntities)
    }
  }
}

/**Models*/
class PaperCorefModel extends TemplateModel{
  var bagOfAuthorsShift = -1.0
  var bagOfAuthorsWeight= 2.0
  this += new ChildParentTemplateWithStatistics[Title]{
    def score(s:Stat):Double ={
      val childTitle = s._2
      val parentTitle = s._3
      if(childTitle != parentTitle) -16.0 else 0.0
    }
  }
  /*
  this += new ChildParentTemplateWithStatistics[BagOfAuthors] {
    override def unroll2(childBow:BagOfAuthors) = Nil
    override def unroll3(childBow:BagOfAuthors) = Nil
    def score(s:Stat): Double = {
      val childBow = s._2
      val parentBow = s._3
      var result = childBow.cosineSimilarity(parentBow,childBow)
      (result+bagOfAuthorsShift)*bagOfAuthorsWeight
    }
  }
  */
  this += new StructuralPriorsTemplate(8.0,-0.25)
}
class AuthorCorefModel extends TemplateModel{
  var bagOfCoAuthorsShift:Double = -0.25
  var bagOfCoAuthorsWeight:Double= 16.0
  var bagOfVenuesShift:Double = -0.25
  var bagOfVenuesWeight:Double= 8.0
  var bagOfKeywordsShift:Double = -0.25
  var bagOfKeyWordsWeight:Double = 8.0
  this += new ChildParentTemplateWithStatistics[FullName] {
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
      if(parentMiddle.length > childMiddle.length)result += 1
      if((childMiddle.length==0 && parentMiddle.length>0) || (parentMiddle.length==0 && childMiddle.length>0))result -= 0.25
      result
    }
    def initialsMisMatch(c:String,p:String):Boolean = (c!=null && p!=null && c.length>0 && p.length>0 && c.charAt(0)!=p.charAt(0))
    def nameMisMatch(c:String,p:String):Boolean = (c!=null && p!=null && c.length>1 && p.length>1 && c != p)
  }
  this += new ChildParentTemplateWithStatistics[BagOfCoAuthors] {
    override def unroll2(childBow:BagOfCoAuthors) = Nil
    override def unroll3(childBow:BagOfCoAuthors) = Nil
    def score(s:Stat): Double = {
      var result = 0.0
      val childBow = s._2
      val parentBow = s._3
      val dot = childBow.deductedDot(parentBow,childBow)
      if(dot == 0.0) {
        result -= childBow.l2Norm*parentBow.l2Norm - 1.0
      }
      //var result = childBow.cosineSimilarity(parentBow,childBow)
      //(result+bagOfCoAuthorsShift)*bagOfCoAuthorsWeight
      result
    }
  }
  new ChildParentTemplateWithStatistics[BagOfVenues] {
    val strength = 16.0
    val shift = -0.25
    override def unroll2(childBow:BagOfVenues) = Nil
    override def unroll3(childBow:BagOfVenues) = Nil
    def score(s:Stat): Double = {
      val childBow = s._2
      val parentBow = s._3
      var result = childBow.cosineSimilarity(parentBow,childBow)
      (result+shift)*strength
    }
  }
  this += new ChildParentTemplateWithStatistics[BagOfKeywords] {
    val strength = 16.0
    val shift = -0.25
    override def unroll2(childBow:BagOfKeywords) = Nil
    override def unroll3(childBow:BagOfKeywords) = Nil
    def score(s:Stat): Double = {
      val childBow = s._2
      val parentBow = s._3
      var result = childBow.cosineSimilarity(parentBow,childBow)
      (result+shift)*strength
    }
  }
  this += new StructuralPriorsTemplate(8.0,0.25)
}

/**Samplers*/
class AuthorSampler(model:TemplateModel) extends BibSampler[AuthorEntity](model){
  def newEntity = new AuthorEntity
  def sampleAttributes(author:AuthorEntity)(implicit d:DiffList) = {
    val representative = author.childEntities.sampleUniformly(random)
    author.attr[FullName].setFullName(representative.attr[FullName])
    //author.attr[Dirty].reset
    if(author.attr[Dirty].value>0)author.attr[Dirty].--()(d)
    if(author.parentEntity != null)author.parentEntity.attr[Dirty].++()(d)
  }
  protected def propagateBagUp(entity:Entity)(implicit d:DiffList):Unit ={
    var e = entity.parentEntity
    while(e!=null){
      e.attr[BagOfCoAuthors].add(entity.attr[BagOfCoAuthors].value)(d)
      e.attr[BagOfVenues].add(entity.attr[BagOfVenues].value)(d)
      e = e.parentEntity
    }
  }
  protected def propagateRemoveBag(parting:Entity,formerParent:Entity)(implicit d:DiffList):Unit ={
    var e = formerParent
    while(e!=null){
      e.attr[BagOfCoAuthors].remove(parting.attr[BagOfCoAuthors].value)
      e.attr[BagOfVenues].remove(parting.attr[BagOfVenues].value)
      e = e.parentEntity
    }
  }
  protected def createAttributesForMergeUp(e1:AuthorEntity,e2:AuthorEntity,parent:AuthorEntity)(implicit d:DiffList):Unit ={
    if(e1.attr[FullName].middleName.length>0)
      parent.attr[FullName].setFullName(e1.attr[FullName])
    else
      parent.attr[FullName].setFullName(e2.attr[FullName])
    parent.attr[BagOfCoAuthors].add(e1.attr[BagOfCoAuthors].value)(d)
    parent.attr[BagOfCoAuthors].add(e2.attr[BagOfCoAuthors].value)(d)
    parent.attr[BagOfVenues].add(e1.attr[BagOfVenues].value)(d)
    parent.attr[BagOfVenues].add(e2.attr[BagOfVenues].value)(d)
  }
  override def mergeLeft(left:AuthorEntity,right:AuthorEntity)(implicit d:DiffList):Unit ={
    val oldParent = right.parentEntity
    right.setParentEntity(left)(d)
    if(right.attr[FullName].middleName.size > left.attr[FullName].middleName.size)
      left.attr[FullName].setMiddle(left.attr[FullName].middleName)
    propagateBagUp(right)(d)
    propagateRemoveBag(right,oldParent)(d)
    structurePreservationForEntityThatLostChild(oldParent)(d)
  }
}
class PaperSampler(model:TemplateModel) extends BibSampler[PaperEntity](model){
  def newEntity = new PaperEntity
  def chooseCanonicalMention(paper:PaperEntity)(implicit d:DiffList):Unit ={
    var canonical:PaperEntity = null
    for(c<- paper.descendantsOfClass[PaperEntity]){
      if(c.isObserved && (canonical==null || c.id.hashCode < canonical.id.hashCode))
        canonical = c
    }
    if(canonical==null)canonical=paper
    if(paper.isEntity.booleanValue)paper.promotedMention.set(canonical.id.toString) else paper.promotedMention.set(null.asInstanceOf[String])
  }
  def sampleAttributes(author:PaperEntity)(implicit d:DiffList) = {
    val representative = author.childEntities.sampleUniformly(random)
    author.attr[Title].set(representative.attr[Title].value)
    author.attr[Dirty].reset
    if(author.parentEntity != null)author.parentEntity.attr[Dirty].++()(d)
  }
  def propagateBagUp(entity:Entity)(implicit d:DiffList):Unit ={
    var e = entity.parentEntity
    while(e!=null){
      //e.attr[BagOfAuthors].add(entity.attr[BagOfAuthors].value)(d)
      e = e.parentEntity
    }
  }
  def propagateRemoveBag(parting:Entity,formerParent:Entity)(implicit d:DiffList):Unit ={
    var e = formerParent
    while(e!=null){
      //e.attr[BagOfAuthors].remove(parting.attr[BagOfAuthors].value)
      e = e.parentEntity
    }
  }
  protected def createAttributesForMergeUp(e1:PaperEntity,e2:PaperEntity,parent:PaperEntity)(implicit d:DiffList):Unit ={
    parent.attr[Title].set(e1.title.value)
    //parent.attr[BagOfAuthors].add(e1.attr[BagOfAuthors].value)(d)
    //parent.attr[BagOfAuthors].add(e2.attr[BagOfAuthors].value)(d)
  }
}

abstract class BibSampler[E<:HierEntity with HasCanopyAttributes[E] with Prioritizable](model:TemplateModel) extends HierCorefSampler[E](model){
  protected var canopies = new HashMap[String,ArrayBuffer[E]]
  protected var _amountOfDirt = 0.0
  protected var _numSampleAttempts = 0.0 //TODO, could this overflow?
  var proposalCount = 0
  //def newEntity = new AuthorEntity
  override def addEntity(e:E):Unit ={
    super.addEntity(e)
    for(cname <- e.canopyAttributes.map(_.canopyName)){
      canopies.getOrElse(cname,{val a = new ArrayBuffer[E];canopies(cname)=a;a}) += e
    }
  }
  override def setEntities(ents:Iterable[E]):Unit ={
    _amountOfDirt = 0.0
    _numSampleAttempts = 0.0
    canopies = new HashMap[String,ArrayBuffer[E]]
    super.setEntities(ents)
    println("Number of canopies: "+canopies.size)
    for((k,v) <- canopies)println("  -"+k+":"+v.size)
  }

  override def nextEntity(context:E=null.asInstanceOf[E]):E = {
    if(context==null)sampleDirtyEntity(entities)
    else sampleDirtyEntity(canopies(context.canopyAttributes.sampleUniformly(random).canopyName))
  }
  protected def sampleDirtyEntity(samplePool:ArrayBuffer[E]) ={
    var numTries = 10
    var result = sampleEntity(samplePool)
    _amountOfDirt += result.dirty.value.toDouble
    _numSampleAttempts += 1.0
    while(numTries>0 && result.dirty.value.toDouble<=(_amountOfDirt/_numSampleAttempts)){
      result = sampleEntity(samplePool)
      _amountOfDirt += result.dirty.value.toDouble
      _numSampleAttempts += 1.0
      numTries -= 1
    }
    //println("result dirty: "+result.dirty.value+ " average dirty: "+(_amountOfDirt/_numSampleAttempts))
    result
  }

  override def mergeLeft(left:E,right:E)(implicit d:DiffList):Unit ={
    val oldParent = right.parentEntity
    right.setParentEntity(left)(d)
    propagateBagUp(right)(d)
    propagateRemoveBag(right,oldParent)(d)
    structurePreservationForEntityThatLostChild(oldParent)(d)
  }
  /**Jump function that proposes merge: entity1--->NEW-PARENT-ENTITY<---entity2 */
  override def mergeUp(e1:E,e2:E)(implicit d:DiffList):E = {
    val oldParent1 = e1.parentEntity
    val oldParent2 = e2.parentEntity
    val result = newEntity
    e1.setParentEntity(result)(d)
    e2.setParentEntity(result)(d)
    createAttributesForMergeUp(e1,e2,result)
    propagateRemoveBag(e1,oldParent1)(d)
    propagateRemoveBag(e2,oldParent2)(d)
    structurePreservationForEntityThatLostChild(oldParent1)(d)
    structurePreservationForEntityThatLostChild(oldParent2)(d)
    result
  }
  /**Peels off the entity "right", does not really need both arguments unless we want to error check.*/
  override def splitRight(left:E,right:E)(implicit d:DiffList):Unit ={
    val oldParent = right.parentEntity
    right.setParentEntity(null)(d)
    propagateRemoveBag(right,oldParent)(d)
    structurePreservationForEntityThatLostChild(oldParent)(d)
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
  protected def propagateBagUp(entity:Entity)(implicit d:DiffList):Unit
  protected def propagateRemoveBag(parting:Entity,formerParent:Entity)(implicit d:DiffList):Unit
  protected def createAttributesForMergeUp(e1:E,e2:E,parent:E)(implicit d:DiffList):Unit
}

/**Back-end implementations for prioritized and canopy based DB access and inference*/
trait BibDatabase{
  def authorColl:EntityCollection[AuthorEntity,AuthorCubbie]
  def paperColl:EntityCollection[PaperEntity,PaperCubbie]
}
abstract class MongoBibDatabase(mongoServer:String="localhost",mongoPort:Int=27017,mongoDBName:String="rexa2-cubbie") extends BibDatabase{
  import MongoCubbieConverter._
  import MongoCubbieImplicits._
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
    protected def changePromotedMention(e:PaperEntity):Unit ={

    }
    protected def promotionChanges:Seq[(String,String)] ={
      val result = new ArrayBuffer[(String,String)]
      for((id, cubbie) <- _id2cubbie){
        val oldPromoted = cubbie.pid.value.toString
        val newPromoted = _id2entity(cubbie.id).entityRoot.asInstanceOf[PaperEntity].promotedMention.value
        if(oldPromoted != newPromoted)result += oldPromoted -> newPromoted
      }
      result
    }
    protected def changePromoted(oldPromotedId:String, newPromotedId:String):Unit ={
      println("CHANGE PROMOTED: "+oldPromotedId+" --> "+newPromotedId)
      entityCubbieColl.update(_.pid.set(oldPromotedId),_.pid.set(newPromotedId)) //TODO: optimize this later, it is not necessary when entire paper entity is in memory,
      authorColl.entityCubbieColl.remove(_.pid.set(oldPromotedId).isMention.set(true).parentRef.exists(false)) //remove all singletons
      authorColl.entityCubbieColl.update(_.pid.set(oldPromotedId),_.pid.set(newPromotedId).inferencePriority.set(0.0)) //TODO: we can optimize this also if all authors are in memory
    }
    override def store(entitiesToStore:Iterable[PaperEntity]):Unit ={
      super.store(entitiesToStore)
      for((oldPromotedId,newPromotedId) <- promotionChanges)changePromoted(oldPromotedId,newPromotedId)
    }
  }
  def add(papers:Iterable[PaperEntity]):Unit ={
    for(p<-papers){
      p.promotedMention.set(p.id)(null)
      addFeatures(p)
      for(a<-p.authors)addFeatures(a)
    }
    paperColl.insert(papers)
    authorColl.insert(papers.flatMap(_.authors))
  }
  def insertMentionsFromBibFile(bibFile:File,numEntries:Int = Integer.MAX_VALUE):Unit ={
    val paperEntities = loadBibTeXFile(bibFile)
    add(paperEntities)
  }
  def insertMentionsFromDBLP(location:String):Unit ={
    val paperEntities = DBLPLoader.loadDBLPData(location)
    add(paperEntities)
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
          author.bagOfCoAuthors += filterFieldNameForMongo(FeatureUtils.firstInitialLastName(coAuthor))
        }
        if(paper.venueName!=null && paper.venueName.value.length>0)
          for(tok<-FeatureUtils.venueBag(paper.venueName.value))
            author.bagOfVenues.add(filterFieldNameForMongo(tok))(null)
      }
    }else println("Warning: paper is null for author with id "+author.id+" cannot compute features.")
  }
  def addFeatures(paper:PaperEntity):Unit ={
    for(author <- paper.authors)
      paper.bagOfAuthors += filterFieldNameForMongo(FeatureUtils.firstInitialLastName(author))
    if(paper.venueName!=null && paper.venueName.value.length>0)
      for(tok<-FeatureUtils.venueBag(paper.venueName.value))
        paper.bagOfKeywords.add(filterFieldNameForMongo(tok))(null)
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
            for(a<-authors)paperEntity.authors += a
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
}

/**Handles loading/storing issues generic to entity collections with canopies, ids, priorities, and hierarchical structure. Agnostic to DB technology (sql, mongo, etc)*/
trait EntityCollection[E<:HierEntity with HasCanopyAttributes[E] with Prioritizable, C<:EntityCubbie[E]]{
  protected var _id2cubbie:HashMap[Any,C] = null
  protected var _id2entity:HashMap[Any,E] = null
  protected def newEntityCubbie:C
  protected def newEntity:E
  protected def fetchFromCubbie(c:C,e:E):Unit
  protected def register(entityCubbie:C) = _id2cubbie += entityCubbie.id->entityCubbie
  protected def wasLoadedFromDB(entity:E) = _id2cubbie.contains(entity.id)
  protected def entity2cubbie(e:E):C =  _id2cubbie(e.id)
  protected def putEntityInCubbie(e:E) = {val ec=newEntityCubbie;ec.store(e);ec}
  protected def entityCubbieColl:MutableCubbieCollection[C]
  protected def changePriority(e:E):Unit =e.priority = scala.math.exp(e.priority - random.nextDouble)
  def insert (c:C) = entityCubbieColl += c
  def insert (e:E) = entityCubbieColl += putEntityInCubbie(e)
  //def insert(cs:Iterable[C]) = entityCubbieColl ++= cs
  def insert(es:Iterable[E]) = entityCubbieColl ++= es.map(putEntityInCubbie(_))
  def drop:Unit
  def finishLoadAndReturnEntities(entityCubbies:Iterable[C]) = {
    val entities = new ArrayBuffer[E]
    for(cubbie <- entityCubbies){
      val entity:E = newEntity
      fetchFromCubbie(cubbie,entity)
      register(cubbie)
      entities += entity
    }
    entities
  }
  def reset:Unit ={
    _id2cubbie = new HashMap[Any,C]
    _id2entity = new HashMap[Any,E]
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
    for(e <- created)entityCubbieColl += putEntityInCubbie(e)
    deletedHook(deleted)
    updatedHook(updated)
    createdHook(created)
    println("deleted: "+deleted.size)
    println("updated: "+updated.size)
    println("created: "+created.size)
  }
  protected def deletedHook(deleted:Seq[E]):Unit ={}
  protected def updatedHook(updated:Seq[E]):Unit ={}
  protected def createdHook(created:Seq[E]):Unit ={}
  /**This is database specific for example because mongo has a specialized _id field*/
  protected def removeEntities(entitiesToRemove:Seq[E]):Unit
  def assembleEntities(toAssemble:Seq[C],id2entity:Any=>E):Unit ={
    for(c<-toAssemble){
      val child = id2entity(c.id)
      val parent = if(c.parentRef.isDefined)id2entity(c.parentRef.value) else null.asInstanceOf[E]
      child.setParentEntity(parent)(null)
    }
  }
  def nextBatch(n:Int=10):Seq[E]
}
abstract class MongoEntityCollection[E<:HierEntity with HasCanopyAttributes[E] with Prioritizable,C<:EntityCubbie[E]](val name:String, mongoDB:DB) extends EntityCollection[E,C]{
  import MongoCubbieImplicits._
  import MongoCubbieConverter._
  protected val entityColl = mongoDB.getCollection(name)
  val entityCubbieColl = new MongoCubbieCollection(entityColl,() => newEntityCubbie,(a:C) => Seq(Seq(a.canopies),Seq(a.inferencePriority),Seq(a.parentRef))) with LazyCubbieConverter[C]
  protected def removeEntities(deleted:Seq[E]):Unit = for(e <- deleted)entityCubbieColl.remove(_.idIs(entity2cubbie(e).id))
  //TODO: generalize MongoSlot so that we can move this into the EnttiyCollection class
  def drop:Unit = entityColl.drop
  def nextBatch(n:Int=10):Seq[E] ={
    reset
    val canopyHash = new HashSet[String]
    var result = new ArrayBuffer[C]
    var topPriority = new ArrayBuffer[C]
    val sorted = entityCubbieColl.query(null,_.canopies.select.inferencePriority.select).sort(_.inferencePriority(-1))
    for(i <- 0 until n)if(sorted.hasNext)
      topPriority += sorted.next
    sorted.close
    for(entity <- topPriority){
      for(name <- entity.canopies.value){
        if(!canopyHash.contains(name)){
          result ++= entityCubbieColl.query(_.canopies(Seq(name)))
          canopyHash += name
        }
      }
    }
    val initialEntities = (for(entityCubbie<-result) yield {val e = newEntity;entityCubbie.fetch(e);e}).toSeq
    for(entityCubbie <- result)_id2cubbie += entityCubbie.id -> entityCubbie
    for(entity <- initialEntities)_id2entity += entity.id -> entity
    println("initial entities: "+initialEntities.size)
    println("initial cubbies : "+result.size)
    println("_id2cubbie: "+ _id2cubbie.size)
    println("_id2entity: "+ _id2entity.size)
    assembleEntities(result, (id:Any)=>{_id2entity(id)})
    initialEntities
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
  def deductedDot(that:BagOfWords, deduct:BagOfWords):Double
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
  def deductedDot(that:BagOfWords,deduct:BagOfWords) : Double = {
    var result = 0.0
    for((k,v) <- iterator)result += v*(that(k) - deduct(k))
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
  def firstInitialLastName(author:AuthorEntity):String ={
    var word:String = author.fullName.firstName
    if(word!=null && word.length>0)word=word.charAt(0)+"" else word = ""
    word += author.fullName.lastName
    word.toLowerCase
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
object EntityUtils{
  def printAuthors(entities:Seq[AuthorEntity],includeSingletons:Boolean=true):Unit ={
    printEntities(entities,includeSingletons,(e:Entity)=>e.attr[FullName].toString,(e:Entity)=>{"{"+bagToString(e.attr[BagOfCoAuthors].value)+"}"})
  }
  def printPapers(entities:Seq[PaperEntity],includeSingletons:Boolean=true):Unit ={
    printEntities(entities,includeSingletons,(e:Entity)=>e.attr[Title].toString)
  }
   def printEntities(entities:Seq[Entity],includeSingletons:Boolean=true,represent:Entity=>String=(e:Entity)=>"",context:Entity=>String=(e:Entity)=>""):Unit = {
    var count = 0
    for(e <- entities.filter((e:Entity) => {e.isRoot && e.isConnected})){
      if(!e.isObserved || includeSingletons)
        println(entityString(e,context))
      count += 1
    }
    println("Printed " + count + " entities.")
  }
  def entityString(e:Entity,context:Entity=>String):String = {
    if(e==null)return "null"
    val result = new StringBuffer
    entityString(e,result,0,context)
    result.toString
  }
  protected def entityString(e:Entity, result:StringBuffer, depth:Int=0,represent:Entity=>String=(e:Entity)=>"", context:Entity=>String=(e:Entity)=>""):Unit = {
    for(i<-0 until depth)result.append("   ")
    //result.append(e.id+"-")
    if(e.isRoot){
      result.append("EntityRoot["+represent(e)+"]")
      //result.append("(exists?="+e.isConnected+" mention?="+e.isObserved+" #children:"+e.subEntitiesSize+")")
    }else if(e.isObserved){
      result.append("Mention["+represent(e)+"]")
      //result.append(" Title="+e.asInstanceOf[AuthorEntity].paper.title)
    }else{
      result.append("SubEntity["+represent(e)+"]")
      if(e.childEntitiesSize==0)result.append("-SUBENTITY ERROR")//throw new Exception("ERROR SUB ENTITY IS EMPTY")
      //if(e.subEntitySize==1)throw new Exception("ERROR SUB ENTITY HAS ONE CHILD)
    }
    result.append(context(e))
    //result.append("{"+bagToString(e.attr[BagOfCoAuthors].value)+"}")
    result.append("\n")
    for(childEntity <- e.childEntitiesIterator)entityString(childEntity,result,depth+1,context)
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