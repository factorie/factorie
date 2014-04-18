package cc.factorie.app.bib

import cc.factorie.app.nlp.hcoref._
import collection.mutable.{Queue,LinkedList,HashSet, HashMap, LinkedHashMap, ArrayBuffer}
import cc.factorie.db.mongo.{LazyCubbieConverter, MutableCubbieCollection, AbstractCubbieCollection, MongoCubbieCollection}
import cc.factorie.util.Cubbie
import experiments.{PaperModelOptions, AuthorModelOptions, ExperimentOptions}
import com.mongodb.{MongoClient, Mongo}
import java.io.File
import scala.Some
import collection.mutable
import Utils.random
/*
class MongoAuthorCollection(mongoDB:com.mongodb.DB) extends MongoDBBibEntityCollection[AuthorEntity,AuthorCubbie]("authors",mongoDB) with BibCollectionAccessPatterns[AuthorCubbie]{
    def newEntityCubbie:AuthorCubbie = new AuthorCubbie
    def newEntity:AuthorEntity = new AuthorEntity
    def fetchFromCubbie(authorCubbie:AuthorCubbie,author:AuthorEntity):Unit = authorCubbie.fetch(author)
}
class MongoPaperCollection(mongoDB:com.mongodb.DB) extends MongoDBBibEntityCollection[PaperEntity,PaperCubbie]("papers",mongoDB) with BibCollectionAccessPatterns[PaperCubbie]{
    def newEntityCubbie:PaperCubbie = new PaperCubbie
    def newEntity:PaperEntity = new PaperEntity
    def fetchFromCubbie(paperCubbie:PaperCubbie,paper:PaperEntity):Unit = paperCubbie.fetch(paper)
}
*/
class Change[C<:Cubbie](val previousVersion:C, val currentVersion:C)
trait BibCollectionAccessPatterns[C<:Cubbie]{
  def cubbieCollection:MutableCubbieCollection[C]
  def findByIds(ids:Seq[String]):Iterator[C] = cubbieCollection.findByIds(ids)
  def findById(id:String):Iterator[C] = cubbieCollection.findById(id)
  def findByCanopy(canopy:String):Iterator[C] = findByCanopies(Seq(canopy))
  def findByCanopies(canopies:Seq[String]):Iterator[C] = cubbieCollection.findByAttribute("canopies",canopies)
  def saveChanges(changes:Seq[Change[C]]) = {var i = 0;while(i<changes.size){val change=changes(i);cubbieCollection.updateDelta(change.previousVersion,change.currentVersion);i+=1}}
}
class BibKBAuthorCollection(val cubbieCollection:MutableCubbieCollection[AuthorCubbie]) extends BibCollectionAccessPatterns[AuthorCubbie]{
  def entity2cubbie(e:AuthorEntity):AuthorCubbie = {val c = new AuthorCubbie;c.store(e);c}
  def cubbie2entity(c:AuthorCubbie):AuthorEntity = {val e = new AuthorEntity;c.fetch(e);e}
  def insert(e:AuthorEntity) = cubbieCollection += entity2cubbie(e)
  def insert(e:Seq[AuthorEntity]) = cubbieCollection ++= e.map(entity2cubbie(_))
}
class BibKBPaperCollection(val cubbieCollection:MutableCubbieCollection[PaperCubbie]) extends BibCollectionAccessPatterns[PaperCubbie]{
  def entity2cubbie(e:PaperEntity):PaperCubbie = {val c = new PaperCubbie;c.store(e);c}
  def cubbie2entity(c:PaperCubbie):PaperEntity = {val e = new PaperEntity;c.fetch(e);e}
  def insert(e:PaperEntity) = cubbieCollection += entity2cubbie(e)
  def insert(e:Seq[PaperEntity]) = cubbieCollection ++= e.map(entity2cubbie(_))
  def createRootAndLinkMentions(mentions:Seq[PaperEntity]):PaperEntity = {
    val root = new PaperEntity
    val canonical = mentions.head
    root.setId(FeatureUtils.paperHash(canonical))
    root.title.set(canonical.title.value)(null)
    root.year.set(canonical.year.value)(null)
    for(m <- mentions)EntityUtils.linkChildToParent(m,root)(null)
    root
  }
}
trait BibKBAccessPatterns{
  def addMentionsNoCoref(paper:Seq[PaperEntity]):Unit
  def addMentionsColdStartCoref(paper:Seq[PaperEntity]):Unit
  def addMentionWarmStartCoref(paper:PaperEntity):Unit
  def addMentionsWarmStartCoref(papers:Seq[PaperEntity]):Unit
}
/*
abstract class MongoBibDatabase(mongoServer:String="localhost",mongoPort:Int=27017,mongoDBName:String="rexa2-cubbie") extends BibDatabase{
  import MongoCubbieConverter._
  import MongoCubbieImplicits._
  println("MongoBibDatabase: "+mongoServer+":"+mongoPort+"/"+mongoDBName)
  protected val mongoConn = new Mongo(mongoServer,mongoPort)
  protected val mongoDB = mongoConn.getDB(mongoDBName)

 */
object BibKB{
  lazy val ldaOpt = if(ldaFileOpt==None)None else Some(LDAUtils.loadLDAModelFromAlphaAndPhi(new File(ldaFileOpt.get)))
  var ldaFileOpt:Option[String] = None
  object opts extends ExperimentOptions with AuthorModelOptions with PaperModelOptions{
    val advanceSeed = new CmdOption("advance-seed",0,"INT","Number of times to call random.nextInt to advance the seed.")
    //db options
    val server = new CmdOption("server","localhost","STRING","Location of Mongo server.")
    val port = new CmdOption("port",27017,"INT","Port of Mongo server.")
    val database = new CmdOption("database","rexa2","FILE","Name of mongo database.")
    val ldaModel = new CmdOption("ldaModel","lda-model.txt","FILE","Location of lda model")
    val rexaFileList = new CmdOption("rexaFileList","none","FILE","Location of the file listing the locations of REXA \".crf.xml\" files to load.")
  }
  def main(argsIn:Array[String]):Unit ={
    var args:Array[String]=new Array[String](0)
    if(argsIn.length>0 && argsIn.head.startsWith("--config")){
      val contents = scala.io.Source.fromFile(new File(argsIn.head.split("=")(1))).mkString
      args = contents.split("\\s+") ++ argsIn
    } else args=argsIn
    opts.parse(args)
    if(opts.ldaModel.wasInvoked)ldaFileOpt = Some(opts.ldaModel.value)
    val mongo = new MongoClient(opts.server.value,opts.port.value)
    val knowledgebase = new MongoBibKB(mongo,opts.database.value)

    //if(1+1==2){
    //  BibKBUtils.createAuthorCitationGraph(knowledgebase)
    //  System.exit(0)
    //}


    if(opts.rexaFileList.wasInvoked){
      println("Loading file names")
      val fileNames = RexaCitationLoader.getFileNames(new File(opts.rexaFileList.value))
      println("Found "+fileNames.size)
      var count=0
      for(fileName <- fileNames){
        val papers = RexaCitationLoader.loadFile(new File(fileName))
        BibFeatures.decorate(papers,FeatureUtils.venueBag(_),ldaOpt)
        if(knowledgebase.paperColl.cubbieCollection.size==0)knowledgebase.addMentionsColdStartCoref(papers)
        else knowledgebase.addMentionsWarmStartCoref(papers)
        count+=1;if(count % 1000==0)print(".")
        if(count % 25000==0){
          println(count)
          println("  -number corefed to a mention: "+knowledgebase.numCoref2Mention)
          println("  -number corefed to an entity: "+knowledgebase.numCoref2Entity)
          println("  -number not corefed: "+knowledgebase.numNotCoref)
        }
      }
      knowledgebase.mostCited()
      //val papers = RexaCitationLoader.loadFromList(new File(opts.rexaFileList.value))
      //BibFeatures.decorate(papers,ldaOpt)
      //if(knowledgebase.paperColl.cubbieCollection.size==0)knowledgebase.addMentionsColdStartCoref(papers)
      //else knowledgebase.addMentionsWarmStartCoref(papers)
    }
  }
}

/*
class EntityAssemblyUnit{
  val id2cubbie = new HashMap[String,AuthorCubbie]
}
*/
class MongoBibKB(mongo:Mongo,dbName:String) extends BibKB{
  val db = mongo.getDB(dbName)
  val authorColl = new BibKBAuthorCollection(new MongoCubbieCollection(db.getCollection("authors"),() => new AuthorCubbie,(a:AuthorCubbie) => Seq(Seq(a.canopies),Seq(a.inferencePriority),Seq(a.parentRef),Seq(a.bagOfTruths),Seq(a.pid))) with LazyCubbieConverter[AuthorCubbie])
  val paperColl = new BibKBPaperCollection(new MongoCubbieCollection(db.getCollection("papers"),() => new PaperCubbie,(a:PaperCubbie) => Seq(Seq(a.canopies),Seq(a.inferencePriority),Seq(a.parentRef),Seq(a.bagOfTruths),Seq(a.pid))) with LazyCubbieConverter[PaperCubbie])
}
abstract class BibKB extends BibKBAccessPatterns{
  protected var numCoref2Mention=0
  protected var numCoref2Entity=0
  protected var numNotCoref=0
  def authorColl:BibKBAuthorCollection
  def paperColl:BibKBPaperCollection
  def isPaperSufficientForInsert(paper:PaperEntity):Boolean= paper.title.value != null && paper.title.value.length > 0 && FeatureUtils.paperHash(paper).length >= 1
  def selectCanonical(papers:Seq[PaperEntity]):PaperEntity = papers.head
  //def addFeatures(paper:PaperEntity):Unit
  //def addFeatures(author:AuthorEntity):Unit
  def mostCited(): Unit = {
    val id2cubbie = new HashMap[String,PaperCubbie]
    val aid2cubbie = new HashMap[String,AuthorCubbie]
    val citedMentions = new ArrayBuffer[PaperCubbie]
    val authorMentions = new ArrayBuffer[AuthorCubbie]
    val id2citations = new HashMap[String,ArrayBuffer[String]]
    val pid2authors = new HashMap[String,ArrayBuffer[AuthorCubbie]]
    val aid2papers = new HashMap[String,ArrayBuffer[PaperCubbie]]
    //val aid2cubbie = new HashMap[String,AuthorCubbie]
    def getRoot[C<:DBEntityCubbie[_]](c:C,id2c:HashMap[String,C]):C ={
      var result = c
      while(result.parentRef.isDefined && id2c.contains(result.parentRef.value.toString))result = id2c(c.parentRef.value.toString)
      result
    }
    def getAuthorRoots(c:PaperCubbie):Seq[AuthorCubbie] ={
      val result = new ArrayBuffer[AuthorCubbie]
      val authors = pid2authors.getOrElse(c.pid.value.toString,new ArrayBuffer[AuthorCubbie])
      authors.map(getRoot[AuthorCubbie](_, aid2cubbie))
    }
    def citationCount(papers:Seq[PaperCubbie]):Int ={
      var result = 0
      for(paper <- papers)result += id2citations(paper.id.toString).size
      result
    }
    var count=0
    var allAuthors = new Queue[AuthorCubbie]
    for(author <- authorColl.cubbieCollection){
      author.coauthors := new BagOfWordsCubbie //to save space
      author.firstNameBag := new BagOfWordsCubbie
      author.middleNameBag := new BagOfWordsCubbie
      allAuthors += author
      count += 1
      if(count % 1000==0)print(".");if(count % 50000==0)println(count)
    }
    while(allAuthors.size>0){
      val author = allAuthors.dequeue()
      if(author.isMention.value)pid2authors.getOrElseUpdate(author.pid.value.toString,new ArrayBuffer[AuthorCubbie]) += author
      aid2cubbie += author.id.toString -> author
      if(author.isMention.value)authorMentions += author
    }
    count=0
    for(paper <- paperColl.cubbieCollection){
      id2cubbie += paper.id.toString -> paper
      if(paper.isMention.value && paper.citedBy.isDefined)citedMentions += paper
      count += 1
      if(count % 1000==0)print(".");if(count % 50000==0)println(count)
    }
    count=0
    for(paper <- citedMentions){
      val root = getRoot[PaperCubbie](paper,id2cubbie)
      if(root.id.toString != paper.id.toString){
        id2citations.getOrElseUpdate(root.id.toString,new ArrayBuffer[String]) += paper.citedBy.value.toString
      }
      count += 1
      if(count % 1000==0)print(".");if(count % 50000==0)println(count)
    };println(" "+count)

    /*
    for(author <- authorMentions){
      val paper = id2cubbie(author.pid.value.toString)
      val paperRoot = getRoot[PaperCubbie](paper,id2cubbie)

    }
    */
    for(author <- authorMentions){
      val root = getRoot[AuthorCubbie](author,aid2cubbie)
      val paper = id2cubbie(author.pid.value.toString)
      val paperRoot = getRoot[PaperCubbie](paper,id2cubbie)
      aid2papers.getOrElseUpdate(root.id.toString,new ArrayBuffer[PaperCubbie]) += id2cubbie(paperRoot.pid.value.toString)
    }
    val sortedAuthors = aid2papers.toSeq.sortBy((idpapers: Pair[String, ArrayBuffer[PaperCubbie]]) => {citationCount(idpapers._2)})
    for((aid,papers)<-sortedAuthors){
      val author = aid2cubbie(aid)
      println("Name: "+author.firstName.value+" "+author.middleName.value+" "+author.lastName.value)
      println("NO. Papers: "+papers.size)
      val sortedPapers = papers.sortBy((paper:PaperCubbie)=>id2citations(paper.id.toString).size).reverse
      var count = 0
      var totalCites=0
      for(paper <- sortedPapers){
        count += 1
        val cites = id2citations(paper.id.toString).size
        println("  "+count+". (#cites: "+cites+") "+paper.title.value)
        totalCites += cites
      }
      println("  (#total cites: "+totalCites+")")
    }
    /*
    val sorted = id2citations.toSeq.sortBy(_._2.size).reverse.take(100)
    var i =0
    for((id,citedBy) <- sorted){
      val p = id2cubbie(id)
      println("1. (#cites "+citedBy.size+") "+p.title.value)
      i += 1
    }
    */
  }
  def addMentionsNoCoref(papers:Seq[PaperEntity]):Unit ={
    paperColl.insert(papers)
    for(paper <- papers.filter(isPaperSufficientForInsert(_)))authorColl.insert(paper.authors)
  }
  //    if(paper.isEntity.booleanValue)paper.promotedMention.set(canonical.id.toString) else paper.promotedMention.set(null.asInstanceOf[String])

  def addMentionsColdStartCoref(papers:Seq[PaperEntity]):Unit ={
    val key2papers = new HashMap[String,ArrayBuffer[PaperEntity]]
    for (paper <- papers.filter(isPaperSufficientForInsert(_))){
      val key = FeatureUtils.paperHash(paper)
      println("id: "+paper.id+" key: "+key)
      paper.promotedMention.set(null.asInstanceOf[String])(null)
      key2papers.getOrElseUpdate(key,new ArrayBuffer[PaperEntity]) += paper
    }
    println("About to insert: "+key2papers.size+" paper entities from "+papers.size+" paper mentions.")
    var numInserted=0
    for ((key,mentions) <- key2papers){
      val canonical = selectCanonical(mentions)
      if(mentions.size>=2){
        val root = paperColl.createRootAndLinkMentions(mentions)
        root.promotedMention.set(canonical.id)(null)
        paperColl.insert(root)
      }
      authorColl.insert(canonical.authors)
      paperColl.insert(mentions)
      numInserted += mentions.size
    }
    println("Inserted "+numInserted +". Should be same as: "+papers.size)
  }
  def addMentionsWarmStartCoref(papers:Seq[PaperEntity]):Unit ={
    //numCoref2Mention=0
    //numCoref2Entity=0
    //numNotCoref=0
    for(paper <- papers)addMentionWarmStartCoref(paper)
    //println("Finished inserting "+papers.size)
    //println("  number corefed to a mention: "+numCoref2Mention)
    //println("  number corefed to an entity: "+numCoref2Entity)
    //println("  number not corefed: "+numNotCoref)
  }
  def addMentionWarmStartCoref(paper:PaperEntity):Unit ={
    //addFeatures(paper)
    if(isPaperSufficientForInsert(paper)){
      val key = FeatureUtils.paperHash(paper)
      val rootOption = paperColl.findById(key).toIterable.headOption
      //println("key: "+key+" id: "+paper.id)
      if(rootOption == None){
        val otherMentionOption = paperColl.findByCanopy(key).toIterable.headOption
        if(otherMentionOption == None){
          //paper.authors.foreach(addFeatures(_))
          //paper.promotedMention.set(paper.id.toString)(null)
          paperColl.insert(paper)
          authorColl.insert(paper.authors)
          numNotCoref += 1
          //println("Paper: "+EntityUtils.entityStringPretty(paper))
          //println("Paper: "+paper.title.value+" hash "+key+" id: "+paper.id)
          //println("  not corefed")
        } else{
          val otherPaper = paperColl.cubbie2entity(otherMentionOption.get)
          val newRoot = paperColl.entity2cubbie(paperColl.createRootAndLinkMentions(Seq(paper,otherPaper)))
          newRoot.pid := otherPaper.id
          //otherPaper.paperMentionId := null
          paperColl.cubbieCollection += newRoot
          paperColl.insert(paper)
          numCoref2Mention += 1
          //println("  corefed to mention")
        }
      }else{ //found an existing root entity
      val entityRoot = {val e=new PaperEntity;rootOption.get.fetch(e);e}
        EntityUtils.linkChildToParent(paper,entityRoot)(null)
        paperColl.insert(paper)
        paperColl.cubbieCollection.updateDelta(rootOption.get,paperColl.entity2cubbie(entityRoot))
        numCoref2Entity += 1
        //println("  corefed to entity")
      }
    }
  }
}

object BibKBUtils{
  def getPromoted(paper:PaperCubbie)(implicit bibkb:BibKB):PaperCubbie ={
    if(paper.parentRef.isDefined)throw new Exception("Error, only root level entities have promoted mentions.")
    if(paper.isMention.value)paper
    else bibkb.paperColl.cubbieCollection.findById(paper.pid.value).next()
  }
  def root(paper:PaperCubbie)(implicit bibkb:BibKB):PaperCubbie ={
    var result = paper
    while(result.parentRef.isDefined){
      val parent = bibkb.paperColl.cubbieCollection.findById(result.parentRef.value).next()
      result = parent
    }
    result
  }
  def root(author:AuthorCubbie)(implicit bibkb:BibKB):AuthorCubbie ={
    var result = author
    while(result.parentRef.isDefined){
      val parent = bibkb.authorColl.cubbieCollection.findById(result.parentRef.value).next()
      result = parent
    }
    result
  }
  def citedBy(authorMention:AuthorCubbie)(implicit bibkb:BibKB):Option[PaperCubbie] ={
    assert(authorMention.isMention.value)
    val paper = bibkb.paperColl.cubbieCollection.findById(authorMention.pid.value).next()
    citedIn(paper)
  }
  def citedIn(paperMention:PaperCubbie)(implicit bibkb:BibKB):Option[PaperCubbie] ={
    assert(paperMention.isMention.value)
    if(paperMention.citedBy.isDefined)Some(bibkb.paperColl.cubbieCollection.findById(paperMention.citedBy.value).next()) else None
  }
  def getPaperMention(authorMention:AuthorCubbie)(implicit bibkb:BibKB):Option[PaperCubbie] ={
    assert(authorMention.isMention.value)
    bibkb.paperColl.cubbieCollection.findById(authorMention.pid.value).toIterable.headOption
  }
  def getAuthorMentions(paperMention:PaperCubbie)(implicit bibkb:BibKB):Seq[AuthorCubbie] ={
    val result = new ArrayBuffer[AuthorCubbie]
    for(a <- bibkb.authorColl.cubbieCollection.findBySlot(_.pid,Seq(paperMention.id.toString)))result += a
    result
  }
  def citedByAuthorEntities(authorMention:AuthorCubbie)(implicit bibkb:BibKB):Seq[AuthorCubbie] ={
    val result = new ArrayBuffer[AuthorCubbie]
    val paperMention = getPaperMention(authorMention).get
    val citedInMentionOpt = citedIn(paperMention)
    for(citedInMention <- citedInMentionOpt){
      val citedInPaper = root(citedInMention)
      val promoted = getPromoted(citedInPaper)
      val authorMentions = getAuthorMentions(promoted)
      result ++= authorMentions.map((a:AuthorCubbie)=>root(a))
    }
    result
  }
  val ids2compressed = new HashMap[String,Int]
  var idCount=0

  val sizeThreshold=10
  def createAuthorCitationGraph(bibkb:BibKB):Unit ={
    val pwNodes = new java.io.PrintWriter(new File("author.nodes"))
    val pwEdges = new java.io.PrintWriter(new File("author.edges"))
    val pwCompress = new java.io.PrintWriter(new File("author.compress"))
    var count=0
    if(sizeThreshold>=0){
      pwEdges.println("0 filtered")
    }

    for(author <- bibkb.authorColl.cubbieCollection){
      if(author.isMention.value){
        val authorEntity = root(author)(bibkb)
        println("count: "+authorEntity.mentionCount.value)
        if(authorEntity.mentionCount.value>=sizeThreshold){
          val authorsThatCitedMe = citedByAuthorEntities(author)(bibkb)
          for(authorThatCitedMe <- authorsThatCitedMe){
            incEdgeCount(authorThatCitedMe,authorEntity,pwEdges)
          }
        }
      }
      if(!author.parentRef.isDefined && author.mentionCount.value>=sizeThreshold)addNode(author,pwNodes)
      count +=1;if(count % 1000==0)print(".");if(count % 50000==0)println(count)
    }
    for((id,compId) <- ids2compressed)pwCompress.println(id+" "+compId)
    pwCompress.flush()
    pwNodes.close()
    pwEdges.close()
    pwCompress.close()
    ids2compressed.clear()
    idCount=0
  }

  def addNode(rootAuthorCubbie:AuthorCubbie,pw:java.io.PrintWriter):Unit ={
    //val id = ids2compressed.getOrElseUpdate(rootAuthorCubbie.id.toString,{idCount+=1;idCount})
    val id=rootAuthorCubbie.id.toString
    val sb = new StringBuffer
    sb.append(rootAuthorCubbie.id.toString+" ")
    sb.append(rootAuthorCubbie.mentionCount.value+" ")
    if(rootAuthorCubbie.firstName.isDefined)sb.append(rootAuthorCubbie.firstName.value)
    if(rootAuthorCubbie.middleName.isDefined)sb.append(rootAuthorCubbie.middleName.value)
    if(rootAuthorCubbie.lastName.isDefined)sb.append(" "+rootAuthorCubbie.lastName.value)
    val line = sb.toString.replaceAll(" +"," ").trim
    pw.println(line)
    pw.flush()
  }
  def incEdgeCount(citer:AuthorCubbie,cited:AuthorCubbie,pw:java.io.PrintWriter):Unit ={
    var idCiter=citer.id.toString
    val idCited=cited.id.toString

    if(citer.mentionCount.value<=sizeThreshold){
      idCiter="0"
    }
    //val idCiter = ids2compressed.getOrElseUpdate(citer.id.toString,{idCount+=1;idCount})
    //val idCited = ids2compressed.getOrElseUpdate(cited.id.toString,{idCount+=1;idCount})
    pw.println(idCiter+" "+idCited)
    pw.flush()
  }
}
