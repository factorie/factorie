package cc.factorie.app.bib
import cc.factorie.app.bib.parser._
import cc.factorie._
import cc.factorie.util.DefaultCmdOptions
import app.nlp.coref._
import db.mongo._
import com.mongodb.{DB, Mongo}
import collection.mutable.{HashSet, HashMap, LinkedHashMap, ArrayBuffer}
import bibtex.parser.BibtexParser
import bibtex.expansions.PersonListExpander
import java.io.{FileInputStream, InputStreamReader, BufferedReader, File}
import bibtex.dom._
import javax.xml.parsers.{DocumentBuilder, DocumentBuilderFactory}
import org.w3c.dom.{Node, NodeList, Document}
import parser.Dom.Entry

//import org.apache.commons.codec._
//import org.apache.commons.codec.language._


class AuthorFLNameCanopy(val entity:AuthorEntity) extends CanopyAttribute[AuthorEntity] {
  def canopyName:String=FeatureUtils.normalizeName((initial(entity.entityRoot.asInstanceOf[AuthorEntity].fullName.firstName)+entity.entityRoot.asInstanceOf[AuthorEntity].fullName.lastName).toLowerCase)
  //def canopyName:String=(initial(entity.fullName.firstName)+entity.fullName.lastName).toLowerCase
  def initial(s:String):String = if(s!=null && s.length>0)s.substring(0,1) else ""

/*
  def convertCanopyForAuthors:Unit ={
    println("Converting canopies...")
    var counter = 0
    val proj = new BasicDBObject
    proj.put("first",true)
    proj.put("last",true)
    proj.put("_id",true)
    val canopizer = new AuthorFLastNameCanopy
    val size = authorEntityColl.size
    val iterator = authorEntityColl.find(null,proj)
    println("SIZE: "+size)
    for(data <- iterator){
      counter += 1
      if(counter % 10000 == 0) print(counter)+" "
      if(counter % 200000 == 0)println(" out of: "+size)
      val oid = data.get("_id").asInstanceOf[ObjectId]
      val first = data.getOrElse("first",null.asInstanceOf[String]).toString
      val last = data.getOrElse("last",null.asInstanceOf[String]).toString
      val author = new AuthorMention(first,null,last)
      val canopy = canopizer.value(author)
      if(canopy!=null && canopy.length>0){
        authorEntityColl.update(Map("_id"->oid), com.mongodb.casbah.query.Implicits.$set("canopy_last"->canopy))
      }
    }
  }
  */

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
class BagOfAuthors(val entity:Entity,authorBag:Map[String,Double]=null) extends BagOfWordsVariable(Nil,authorBag) with EntityAttr{
  def addAuthor(author:AuthorEntity,order:Int)(implicit d:DiffList) = this.add(order+"-"+FeatureUtils.filterFieldNameForMongo(FeatureUtils.firstInitialLastName(author)),1.0)
}
class Kind(val entity:Entity, kind:String) extends StringVariable(kind) with EntityAttr
class PromotedMention(val entity:Entity,id:String) extends StringVariable(id) with EntityAttr
//class AuthorList(val entity:Entity) extends SeqVariable[String] with EntityAttr
//author attributes
class BagOfTopics(val entity:Entity, topicBag:Map[String,Double]=null) extends BagOfWordsVariable(Nil, topicBag) with EntityAttr
class BagOfVenues(val entity:Entity, venues:Map[String,Double]=null) extends BagOfWordsVariable(Nil, venues) with EntityAttr
class BagOfCoAuthors(val entity:Entity,coAuthors:Map[String,Double]=null) extends BagOfWordsVariable(Nil, coAuthors) with EntityAttr
class BagOfEmails(val entity:Entity,keywords:Map[String,Double]=null) extends BagOfWordsVariable(Nil,keywords) with EntityAttr
class BagOfFirstNames(val entity:Entity,names:Map[String,Double]=null) extends BagOfWordsVariable(Nil,names) with EntityAttr{
  override def add(s:String,w:Double=1.0)(implicit d:DiffList) = super.add(FeatureUtils.normalizeName(s).toLowerCase,w)(d)
}
class BagOfMiddleNames(val entity:Entity,names:Map[String,Double]=null) extends BagOfWordsVariable(Nil,names) with EntityAttr{
  override def add(s:String,w:Double=1.0)(implicit d:DiffList) = super.add(FeatureUtils.normalizeName(s).toLowerCase,w)(d)
}
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
  attr += new BagOfVenues(this)
  attr += new PromotedMention(this, "NOT-SET")
  def title = attr[Title]
  def year = attr[Year]
  def venueName = attr[VenueName]
  def bagOfAuthors = attr[BagOfAuthors]
  def bagOfKeywords = attr[BagOfKeywords]
  def bagOfVenues = attr[BagOfVenues]
  def promotedMention = attr[PromotedMention]
  def string = title.toString
  val authors = new ArrayBuffer[AuthorEntity]{
    override def += (a:AuthorEntity) = {
      a.paper=outer
      a.paperMentionId=outer.id
      bagOfAuthors.addAuthor(a,size)(null)
      super.+=(a)
    }
  }
  def propagateAddBagsUp()(implicit d:DiffList):Unit = {throw new Exception("not implemented")}
  def propagateRemoveBagsUp()(implicit d:DiffList):Unit = {throw new Exception("not implemented")}
}
class AuthorEntity(f:String="DEFAULT",m:String="DEFAULT",l:String="DEFAULT", isMention:Boolean = false) extends HierEntity(isMention) with HasCanopyAttributes[AuthorEntity] with Prioritizable{
  var _id = java.util.UUID.randomUUID.toString+""
  override def id = _id
  priority = random.nextDouble
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
  //var groundTruth:Option[String] = None

  val bagOfFirstNames = new BagOfFirstNames(this)
  val bagOfMiddleNames = new BagOfMiddleNames(this)
//  val bagOfTruths = new BagOfTruths(this)
  attr += bagOfFirstNames
  attr += bagOfMiddleNames
  //attr += bagOfTruths
}

object Coref{
  def main(args:Array[String]) ={
    println("Args: "+args.length)
    for(arg <- args)
      println("  "+arg)
    object opts extends DefaultCmdOptions{
      //db options
      val server = new CmdOption("server","localhost","FILE","Location of Mongo server.")
      val port = new CmdOption("port","27017","FILE","Port of Mongo server.")
      val database = new CmdOption("database","rexa2-cubbies","FILE","Name of mongo database.")
      //db creation options
      val bibDirectory = new CmdOption("bibDir","/Users/mwick/data/thesis/all3/","FILE","Pointer to a directory containing .bib files.")
      val rexaData = new CmdOption("rexaData","/Users/mwick/data/rexa/rexaAll/","FILE","Location of the labeled rexa2 directory.")
      val createDB = new CmdOption("create","true","FILE","Creates a new DB by destroying the old one.")
      //inference options
      val numEpochs = new CmdOption("epochs","1","FILE","Number of inference round-trips to DB.")
      val batchSize = new CmdOption("batchSize","1","FILE","Number of entities used to retrieve canopies from.")
      val stepMultiplierA = new CmdOption("a","0.0","FILE","Runs for n^2 steps (n=number of mentions to do inference on.)")
      val stepMultiplierB = new CmdOption("b","0.0","FILE","Runs for n steps (n=number of mentions to do inference on.)")
      val stepMultiplierC = new CmdOption("c","0.0","FILE","Runs for c steps (c=constant)")
      val evaluateOnly = new CmdOption("evaluate","false","FILE","Loads labeled data, evaluates the accuracy of coreference, and exits.")
    }
    opts.parse(args)
    if(opts.evaluateOnly.value.toBoolean){
      val tmpDB = new EpistemologicalDB(opts.server.value,opts.port.value.toInt,opts.database.value)
      tmpDB.loadAndEvaluateAuthors
      System.exit(0)
    }
    //Watch order here: first drop the collection, then create the epidb, then insert mentions. Required to make sure indexes remain intact.
    if(opts.createDB.value.toBoolean){
      println("Dropping database.")
      val mongoConn = new Mongo(opts.server.value,opts.port.value.toInt)
      val mongoDB = mongoConn.getDB(opts.database.value)
      mongoDB.getCollection("authors").drop
      mongoDB.getCollection("papers").drop
    }
    val epiDB = new EpistemologicalDB(opts.server.value,opts.port.value.toInt,opts.database.value)
    if(opts.createDB.value.toBoolean){
      println("About to add data.")
      if(opts.bibDirectory.value.toLowerCase != "none")
        epiDB.insertMentionsFromBibDir(new File(opts.bibDirectory.value))
      if(opts.rexaData.value.toLowerCase != "none"){
        println("Loading labeled data from: " + opts.rexaData.value)
        epiDB.insertLabeledRexaMentions(new File(opts.rexaData.value))
      }
    }

    /*
    //epiDB.drop
    epiDB.insertLabeledRexaMentions(new File("/Users/mwick/data/rexa/rexaTrain/"))
    epiDB.insertLabeledRexaMentions(new File("/Users/mwick/data/rexa/rexaTest/"))
    //epiDB.insertLabeledRexaMentions(new File("/Users/mwick/data/rexa/rexaAll/"))
    epiDB.insertMentionsFromBibFile(new File("/Users/mwick/data/thesis/rexa2/labeled/fpereira.bib"))
    epiDB.insertMentionsFromBibDir(new File("/Users/mwick/data/thesis/all3/"))
    //epiDB.insertMentionsFromBibFile(new File("/Users/mwick/data/thesis/rexa2/labeled/single-promotion-test.bib"))
    */
    epiDB.inferenceSweep(
      opts.numEpochs.value.toInt,
      opts.batchSize.value.toInt,
      opts.stepMultiplierA.value.toDouble,opts.stepMultiplierB.value.toDouble,opts.stepMultiplierC.value.toDouble
    )
  }
}

class EpistemologicalDB(mongoServer:String="localhost",mongoPort:Int=27017,mongoDBName:String="rexa2-cubbie") extends MongoBibDatabase(mongoServer,mongoPort,mongoDBName){
  //val authorModel = new ParameterizedAuthorCorefModel
  //val authorTrainer = new AuthorTrainer(authorModel,new TrainingModel){temperature = 0.001}//;override val amIMetropolis=true}
  //val authorPredictor = new AuthorSampler(new TrainingModel){temperature = 0.001}
  //val authorPredictor = new AuthorSampler(authorModel){temperature = 0.001}
  val authorPredictor = new AuthorSampler(new AuthorCorefModel){temperature = 0.001}
  val paperPredictor = new PaperSampler(new PaperCorefModel)
  def drop:Unit = {
    //authorColl.remove(_)
    //paperColl.remove(_)
    authorColl.drop
    paperColl.drop
  }
  protected def infer[E<:HierEntity with HasCanopyAttributes[E] with Prioritizable,C<:EntityCubbie[E]](entityCollection:EntityCollection[E,C],predictor:HierCorefSampler[E],k:Int,inferenceSteps:Int=>Int) ={
    var timer = System.currentTimeMillis
    val entities = entityCollection.nextBatch(k)
    val numSteps = inferenceSteps(entities.size)
    predictor.setEntities(entities)
    predictor.timeAndProcess(numSteps)
    println(numSteps + " of inference took " + ((System.currentTimeMillis-timer)/1000L) + " seconds.")
    entityCollection.store(predictor.getEntities ++ predictor.getDeletedEntities.map(_.asInstanceOf[E]))
  }

  def loadAndEvaluateAuthors:Unit ={
    val labeledAuthors = authorColl.loadLabeled
    Evaluator.eval(labeledAuthors)
  }
  /*
  //assumes everything is singletons
  protected def trainTestAuthors(pctTrain:Double, trainingEpochs:Int,trainingSteps:Int=>Int, testingSteps:Int=>Int):Unit ={
    val trainingSet = new ArrayBuffer[AuthorEntity]
    val testingSet = new ArrayBuffer[AuthorEntity]
    var entities = authorColl.nextBatch(10000)
    for(e<-entities.filter(_.groundTruth!=None)){
      if(e.groundTruth.get.startsWith("rexaTrain"))trainingSet += e
      else if(e.groundTruth.get.startsWith("rexaTest"))testingSet += e
    }
    println("Num mentions: "+entities.size)
    println("Training set size: "+trainingSet.size)
    println("Testing set.size: "+testingSet.size)
    EntityUtils.printAuthors(EntityUtils.makeTruth(trainingSet))
    println("\nTRAINING...")
    for(i <- 0 until trainingEpochs){
      println("EPOCH "+i)
      //val ts = trainingSet
      //val ts = if((i+1) % 3 != 0) EntityUtils.makeSingletons(trainingSet) else EntityUtils.makeTruth(trainingSet)
      val ts = EntityUtils.makeTruth(trainingSet)
      //val ts = EntityUtils.makeSingletons(trainingSet)
      Evaluator.eval(ts)
      authorTrainer.setEntities(ts)
      authorTrainer.process(10000)
      //authorTrainer.process(trainingSteps(ts.size)/trainingEpochs)
      Evaluator.eval(authorTrainer.getEntities)
    }
    

    println("\nTESTING...")
    authorPredictor.setEntities(testingSet)
    authorPredictor.process(testingSteps(testingSet.size))
    EntityUtils.printAuthors(authorPredictor.getEntities)
    Evaluator.eval(authorPredictor.getEntities)
  }*/
  def inferenceSweep(numEpochs:Int,k:Int,a:Double,b:Double,c:Double) ={
    def calculateSteps(numEntities:Int) = (numEntities.toDouble*numEntities.toDouble*a + numEntities.toDouble*b + c).toInt
    for(i<-0 until numEpochs){
      println("INFERENCE ROUND "+i)
      /*
      println("Paper coreference")
      //infer[PaperEntity,PaperCubbie](paperColl,paperPredictor,k,calculateSteps(_))
      val papers = paperColl.nextBatch(k)
      paperPredictor.setEntities(papers)
      paperPredictor.process(calculateSteps(papers.size))
      val processedPapers = paperPredictor.getEntities ++ paperPredictor.getDeletedEntities.map(_.asInstanceOf[PaperEntity])
      for(paper <- processedPapers)paperPredictor.chooseCanonicalMention(paper)(null)
      paperColl.store(processedPapers)
      EntityUtils.printPapers(paperPredictor.getEntities)
      */
      println("Author coreference")
      //trainTestAuthors(0.6,10,calculateSteps(_),calculateSteps(_))
      infer[AuthorEntity,AuthorCubbie](authorColl,authorPredictor,k,calculateSteps(_))
      //EntityUtils.printAuthors(authorPredictor.getEntities)
      Evaluator.eval(authorPredictor.getEntities)
    }
  }
}

/**Models*/
class TrainingModel extends TemplateModel{
  /*
  this += new ChildParentTemplateWithStatistics[BagOfTruths]{
    override def unroll2(childBow:BagOfTruths) = Nil
    override def unroll3(childBow:BagOfTruths) = Nil
    def score(s:Stat):Double ={
      val childBow  =s._2
      val parentBow =s._3
      val result = childBow.cosineSimilarity(parentBow,childBow)
      result
    }
  }
  */
  this += new TemplateWithStatistics2[BagOfTruths,IsEntity]{
    val precisionDominated=0.95
    def unroll1(bot:BagOfTruths) =if(bot.entity.isRoot)Factor(bot,bot.entity.attr[IsEntity]) else Nil
    def unroll2(isEntity:IsEntity) = if(isEntity.entity.isRoot)Factor(isEntity.entity.attr[BagOfTruths],isEntity) else Nil
    override def score(s:Stat):Double ={
      var result = 0.0
      val bag = s._1
      val bagSeq = bag.iterator.toSeq
      var i=0;var j=0;
      var tp = 0.0
      var fp = 0.0
      while(i<bagSeq.size){
        val e:AuthorEntity=null
        val (labeli,weighti) = bagSeq(i)
        j = i
        while(j<bagSeq.size){
          val (labelj,weightj) = bagSeq(j)
          if(labeli==labelj)
            tp += (weighti*(weighti-1))/2.0
          else
            fp += weighti*weightj
          j += 1
        }
        i += 1
      }
      val normalizer = tp+fp
      result = tp - fp
      //println("  bag: "+bag)
      //println("    tp: "+tp)
      //println("    fp: "+fp)
      //println("    result: "+result)
      // result = tp*(1.0 - precisionDominated) - fp*(precisionDominated)
      //if(fp==0) result = tp else result = -fp

      //println("  normalized: "+result/normalizer)
      result
    }
  }
}
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
  //this += new StructuralPriorsTemplate(8.0,0.25)
}
class AuthorCorefModel extends TemplateModel{
  var bagOfCoAuthorsShift:Double = 0.0
  var bagOfCoAuthorsWeight:Double= 4.0
  var bagOfVenuesShift:Double = -0.25
  var bagOfVenuesWeight:Double= 8.0
  var bagOfKeywordsShift:Double = -0.25
  var bagOfKeyWordsWeight:Double = 8.0
  this += new ChildParentTemplateWithStatistics[FullName] {
    def score(s:Stat): Double = {
      var result = 0.0
      val childName = s._2
      val parentName = s._3
      val childFirst = FeatureUtils.normalizeName(childName(0)).toLowerCase
      val childMiddle = FeatureUtils.normalizeName(childName(1)).toLowerCase
      val childLast = FeatureUtils.normalizeName(childName(2)).toLowerCase
      val parentFirst = FeatureUtils.normalizeName(parentName(0)).toLowerCase
      val parentMiddle = FeatureUtils.normalizeName(parentName(1)).toLowerCase
      val parentLast = FeatureUtils.normalizeName(parentName(2)).toLowerCase
      if(childLast != parentLast)result -= 8
      if(initialsMisMatch(childFirst,parentFirst))result -= 8
      if(initialsMisMatch(childMiddle,parentMiddle))result -= 8
      if(nameMisMatch(childFirst,parentFirst))result -= 8
      if(nameMisMatch(childMiddle,parentMiddle))result -= 2
      if(parentMiddle.length > childMiddle.length)result += 0.5
      if(parentFirst.length > childFirst.length)result += 0.5
      if((childMiddle.length==0 && parentMiddle.length>0) || (parentMiddle.length==0 && childMiddle.length>0))result -= 0.25
      result
    }
    def initialsMisMatch(c:String,p:String):Boolean = (c!=null && p!=null && c.length>0 && p.length>0 && c.charAt(0)!=p.charAt(0))
    def nameMisMatch(c:String,p:String):Boolean = (c!=null && p!=null && !FeatureUtils.isInitial(c) && !FeatureUtils.isInitial(p) && c != p)
  }

  this += new ChildParentTemplateWithStatistics[BagOfCoAuthors] {
    override def unroll2(childBow:BagOfCoAuthors) = Nil
    override def unroll3(childBow:BagOfCoAuthors) = Nil
    def score(s:Stat): Double = {
      var result = 0.0
      val childBow = s._2
      val parentBow = s._3
      val dot = childBow.deductedDot(parentBow,childBow)
      var cossim:Double=0.0
      if(dot == 0.0) {
        result -= childBow.l2Norm*parentBow.l2Norm*2 - 0.5
      }
      else cossim=childBow.cosineSimilarity(parentBow,childBow)
      result += (cossim+bagOfCoAuthorsShift)*bagOfCoAuthorsWeight
      //println("child: "+childBow+" norm: "+childBow.l2Norm+" bforce: "+childBow.l2NormBruteForce)
      //println("parnt: "+parentBow+" norm: "+parentBow.l2Norm+" bforce: "+parentBow.l2NormBruteForce)
      //println("  dot: "+dot)
      //println("  cos: "+cossim)
      //println("  result: "+result)
      result
    }
  }
  this += new ChildParentTemplateWithStatistics[BagOfVenues] {
    val strength = 4.0 //16.0
    val shift = 0.0 //-0.25
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
    val strength = 4.0
    val shift = 0.0
    override def unroll2(childBow:BagOfKeywords) = Nil
    override def unroll3(childBow:BagOfKeywords) = Nil
    def score(s:Stat): Double = {
      val childBow = s._2
      val parentBow = s._3
      var result = childBow.cosineSimilarity(parentBow,childBow)
      (result+shift)*strength
    }
  }


  this += new TemplateWithStatistics1[BagOfFirstNames]{
    def score(s:Stat):Double ={
      var result = 0.0
      val bag = s._1
      //val bagSeq = bag.iterator.toSeq
      val bagSeq = bag.iterator.filter(_._1.length>0).toSeq
      var firstLetterMismatches = 0
      var nameMismatches = 0
      var i=0;var j=0;
      while(i<bagSeq.size){
        val (wordi,weighti) = bagSeq(i)
        j=i+1
        while(j<bagSeq.size){
          val (wordj,weightj) = bagSeq(j)
          if(wordi.charAt(0) != wordj.charAt(0))firstLetterMismatches += 1 //weighti*weightj
          else if(FeatureUtils.isInitial(wordi) && FeatureUtils.isInitial(wordj) && wordi != wordj)firstLetterMismatches += 1
          if(wordi.length>1 && wordj.length>1 && !FeatureUtils.isInitial(wordi) && !FeatureUtils.isInitial(wordj))nameMismatches += wordi.editDistance(wordj)
          j += 1
        }
        i += 1
      }
      result -= firstLetterMismatches*firstLetterMismatches*4
      result -= nameMismatches*nameMismatches*4
      /*
      val bag = s._1
      var fullNameCount = 0.0
      var initialCount = 0.0
      var result = 0.0
      for((k,v) <- bag.iterator)if(k.length>1)fullNameCount+=1 else if(k.length==1)initialCount+=1
      result -= (initialCount*initialCount -1)*4
      result -= (fullNameCount*fullNameCount -1)*4
      */
      result
    }
  }
  this += new TemplateWithStatistics1[BagOfMiddleNames]{
    def score(s:Stat):Double ={
      var result = 0.0
      val bag = s._1
      val bagSeq = bag.iterator.filter(_._1.length>0).toSeq
      //val bagSeq = bag.iterator.toSeq
      var firstLetterMismatches = 0
      var nameMismatches = 0
      var i=0;var j=0;
      while(i<bagSeq.size){
        val (wordi,weighti) = bagSeq(i)
        j=i+1
        while(j<bagSeq.size){
          val (wordj,weightj) = bagSeq(j)
          if(wordi.charAt(0) != wordj.charAt(0))firstLetterMismatches += 1 //weighti*weightj
          else if(FeatureUtils.isInitial(wordi) && FeatureUtils.isInitial(wordj) && wordi != wordj)firstLetterMismatches += 1
          if(wordi.length>1 && wordj.length>1 && !FeatureUtils.isInitial(wordi) && !FeatureUtils.isInitial(wordj))nameMismatches += wordi.editDistance(wordj)
          j += 1
        }
        i += 1
      }
      result -= firstLetterMismatches*firstLetterMismatches*4
      result -= nameMismatches*nameMismatches*4
      /*
      println("bag: "+bag)
      println("  mismatches: "+firstLetterMismatches)
      println("  namemm    : "+nameMismatches)
      println("  result: "+result)
      */
      /*
      var initialNameMisMatches=0
      var fullNameCount = 0
      var initialCount = 0
      var result = 0.0
      val initials = new Array[String](bag.size)
      val fullName = new Array[String](bag.size)
      for((k,v) <- bag.iterator){
        if(isInitial(k)){
          initials(initialCount)=k
          initialCount+=1
        } else if(k.length>1){
          fullName(fullNameCount)=k
          fullNameCount+=1
        }
      } //if(k.length>1)fullNameCount+=1 else initialCount+=1
      result -= (initialCount*initialCount -1)*2
      result -= (fullNameCount*fullNameCount -1)*4

      var i=0;var j=0
      while(i<initialCount){
        while(j<-fullNameCount){
          if(initials(i)._1.charAt(0) != fullName(j)._1.charAt(j))initialNameMisMatches += 1
          j+=1
        }
        i+=1
      }
      result -= initialNameMisMatches*initialNameMisMatches*4

      println("Bag: "+bag)
      println("  initialCount: "+initialCount)
      println("  fullnmeCount: "+fullNameCount)
      println("  initial-name: "+initialNameMisMatches)
      println("  result: "+result)
      */
      result
    }

  }
  
  //this += new StructuralPriorsTemplate(0.0,-0.5)
  this += new StructuralPriorsTemplate(4.0,0.25)
  //this += new StructuralPriorsTemplate(16.0,0.25)
  //TODO: the structural prior should look at a canopy, look at the number of non-singleton entities, and make the clustering strength stronger proportional to this
  //the idea is that ambiguous canopies will be less aggressively clustered than unambiguous ones, and this will depend on the data
  //TODO: have an ambiguity score for each entity?
}

/**Samplers*/
/*
trait Jump[E<:HierEntity]{
  def execute(d:DiffList):Unit
  def createdEntities:Seq[E] = Seq.empty[E]
}
trait Merge(val left:E,val right:E) extends Jump[E]
class MergeLeft[E<:HierEntity](left:E,right:E) extends Merge[E](left,right){
  def execute(d:DiffList)Unit ={
    
  }
}

class MultiProposal(model:TemplateModel) extends AuthorSampler(model){
  val proposalQueue = new PriorityQueue[Jump]
}*/
/*
class PrioritizedAuthorSampler(model:TemplateModel) extends AuthorSampler(model){
  //def sampleFirst:AuthorEntity = null
  //def sampleSecond:AuthorEntity = null
  override def mergeLeft(left:AuthorEntity,right:AuthorEntity)(implicit d:DiffList) ={
    super.mergeLeft(left,right)(d)
  }
}
*/

/*
class AuthorTrainer(model:TemplateModel, trainingSignal:TemplateModel) extends AuthorSampler(model) with SampleRank with AROWUpdates{
  override def objective = trainingSignal
}
*/

class AuthorSampler(model:TemplateModel) extends BibSampler[AuthorEntity](model){
  def newEntity = new AuthorEntity
  def sampleAttributes(author:AuthorEntity)(implicit d:DiffList) = {
    if(author.childEntities.size==0){
      EntityUtils.printAuthors(Seq(author))
      println("observeD? "+author.isObserved)
      println("entity? "+author.isRoot)
      println("exists? "+author.isConnected)
      throw new Exception("ERROR AUTHOR HAS NO CHILDREN")
    }
    val representative = author.childEntities.value.sampleUniformly(random)
    author.attr[FullName].setFullName(representative.attr[FullName])
    //author.attr[Dirty].reset
    if(author.attr[Dirty].value>0)author.attr[Dirty].--()(d)
    if(author.parentEntity != null)author.parentEntity.attr[Dirty].++()(d)
  }
  //val recurseBags=false
  protected def propagateBagUp(entity:Entity)(implicit d:DiffList):Unit ={
    var e = entity.parentEntity
    while(e!=null){
      //if(recurseBags || (e eq entity)){
        e.attr[BagOfCoAuthors].add(entity.attr[BagOfCoAuthors].value)(d)
        e.attr[BagOfVenues].add(entity.attr[BagOfVenues].value)(d)
        e.attr[BagOfKeywords].add(entity.attr[BagOfKeywords].value)(d)
      //}
      e.attr[BagOfFirstNames].add(entity.attr[BagOfFirstNames].value)(d)
      e.attr[BagOfMiddleNames].add(entity.attr[BagOfMiddleNames].value)(d)
      e.attr[BagOfTruths].add(entity.attr[BagOfTruths].value)(d)
      e = e.parentEntity
    }
  }
  protected def propagateRemoveBag(parting:Entity,formerParent:Entity)(implicit d:DiffList):Unit ={
    var e = formerParent
    while(e!=null){
      //if(recurseBags || (e eq formerParent)){
        e.attr[BagOfCoAuthors].remove(parting.attr[BagOfCoAuthors].value)
        e.attr[BagOfVenues].remove(parting.attr[BagOfVenues].value)
        e.attr[BagOfKeywords].remove(parting.attr[BagOfKeywords].value)(d)
      //}
      e.attr[BagOfFirstNames].remove(parting.attr[BagOfFirstNames].value)(d)
      e.attr[BagOfMiddleNames].remove(parting.attr[BagOfMiddleNames].value)(d)
      e.attr[BagOfTruths].remove(parting.attr[BagOfTruths].value)(d)
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
    parent.attr[BagOfKeywords].add(e1.attr[BagOfKeywords].value)(d)
    parent.attr[BagOfKeywords].add(e2.attr[BagOfKeywords].value)(d)
    parent.attr[BagOfFirstNames].add(e1.attr[BagOfFirstNames].value)(d)
    parent.attr[BagOfFirstNames].add(e2.attr[BagOfFirstNames].value)(d)
    parent.attr[BagOfMiddleNames].add(e1.attr[BagOfMiddleNames].value)(d)
    parent.attr[BagOfMiddleNames].add(e2.attr[BagOfMiddleNames].value)(d)
    parent.attr[BagOfTruths].add(e1.attr[BagOfTruths].value)(d)
    parent.attr[BagOfTruths].add(e2.attr[BagOfTruths].value)(d)
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
//  override def collapse(entity:AuthorEntity)(implicit d:DiffList):Unit={}

  def proposeMergeIfValid(entity1:AuthorEntity,entity2:AuthorEntity,changes:ArrayBuffer[(DiffList)=>Unit]):Unit ={
    if (entity1.entityRoot.id != entity2.entityRoot.id)  //sampled nodes refer to different entities
      if(!isMention(entity1))
        changes += {(d:DiffList) => mergeLeft(entity1,entity2)(d)} //what if entity2 is a mention?
  }

  override def settings(c:Null) : SettingIterator = new SettingIterator {
    val changes = new scala.collection.mutable.ArrayBuffer[(DiffList)=>Unit]
    val (entity1,entity2) = nextEntityPair
    if (entity1.entityRoot.id != entity2.entityRoot.id) { //sampled nodes refer to different entities
      /*
      if(!isMention(entity1)){
        changes += {(d:DiffList) => mergeLeft(entity1,entity2)(d)} //what if entity2 is a mention?
        if(entity1.id != entity1.entityRoot.id) //avoid adding the same jump to the list twice
          changes += {(d:DiffList) => mergeLeft(entity1.entityRoot.asInstanceOf[T],entity2)(d)} //unfortunately casting is necessary unless we want to type entityRef/parentEntity/childEntities
      }
      */
      var e1 = entity1
      var e2 = entity2.getAncestor(random.nextInt(entity2.depth+1)).asInstanceOf[AuthorEntity]
      while(e1 != null){
        proposeMergeIfValid(e1,e2,changes)
        e1 = e1.parentEntity.asInstanceOf[AuthorEntity]
      }
      if(entity1.parentEntity==null && entity2.parentEntity==null)
        changes += {(d:DiffList) => mergeUp(entity1,entity2)(d)}
    } else { //sampled nodes refer to same entity
      changes += {(d:DiffList) => splitRight(entity1,entity2)(d)}
      changes += {(d:DiffList) => splitRight(entity2,entity1)(d)}
      if(entity1.parentEntity != null && !entity1.isObserved)
        changes += {(d:DiffList) => {collapse(entity1)(d)}}
    }
    if(entity1.dirty.value>0)changes += {(d:DiffList) => sampleAttributes(entity1)(d)}
    if(entity1.entityRoot.id != entity1.id && entity1.entityRoot.attr[Dirty].value>0)changes += {(d:DiffList) => sampleAttributes(entity1.entityRoot.asInstanceOf[AuthorEntity])(d)}

    changes += {(d:DiffList) => {}} //give the sampler an option to reject all other proposals
    var i = 0
    def hasNext = i < changes.length
    def next(d:DiffList) = {val d = newDiffList; changes(i).apply(d); i += 1; d }
    def reset = i = 0
  }
  override def proposalHook(proposal:Proposal) = {
    super.proposalHook(proposal)
    if(proposalCount % (10000*20)==0){
      Evaluator.eval(getEntities)
    }
//    if(proposalCount % 10000==0)
//      printWeightInfo
    /*
    if(proposal.modelScore!=0.0){
      println("\n=============")
      println("PICKED: "+proposal)
      println("  model-score: "+proposal.modelScore)
      val debugdiff = new cc.factorie.example.DebugDiffList
      debugdiff ++= proposal.diff
      println("----DEBUG----")
      val score = debugdiff.scoreAndUndo(model)
      println("-------------")
      println("=============")
          }
          */

/*
    if(proposalCount % 10000==0){
      print("Making singletons... ")
      EntityUtils.makeSingletons(entities)
      this.performMaintenance(entities)
      println("done.")
    }
*/
  }
  /*
  def printWeightInfo:Unit ={
    var l2Norm = 0.0
    for(template <- model.familiesOfClass(classOf[DotFamily])){
      for(i<-template.weights.activeDomain){
        l2Norm += template.weights(i)*template.weights(i)
      }
    }
    l2Norm = scala.math.sqrt(l2Norm)
    CorefDomain.printWeights(model)
    Evaluator.eval(getEntities)
    println("l2Norm: "+ l2Norm)
    println("samps: "+proposalCount)
  }
  */
/*
  override def proposalHook(proposal:Proposal) = {
    super.proposalHook(proposal)
    if(proposalCount == 100000){
      print("Making singletons... ")
      EntityUtils.makeSingletons(entities)
      println("done.")
      this.performMaintenance(entities)
    }
  }
  */
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
    val representative = author.childEntities.value.sampleUniformly(random)
    author.attr[Title].set(representative.attr[Title].value)
    author.attr[Dirty].reset
    if(author.parentEntity != null)author.parentEntity.attr[Dirty].++()(d)
  }
  def propagateBagUp(entity:Entity)(implicit d:DiffList):Unit ={
    var e = entity.parentEntity
    while(e!=null){
      e.attr[BagOfAuthors].add(entity.attr[BagOfAuthors].value)(d)
      e.attr[BagOfVenues].add(entity.attr[BagOfVenues].value)(d)
      e.attr[BagOfKeywords].add(entity.attr[BagOfKeywords].value)(d)
      e = e.parentEntity
    }
  }
  def propagateRemoveBag(parting:Entity,formerParent:Entity)(implicit d:DiffList):Unit ={
    var e = formerParent
    while(e!=null){
      e.attr[BagOfAuthors].remove(parting.attr[BagOfAuthors].value)
      e.attr[BagOfVenues].remove(parting.attr[BagOfVenues].value)(d)
      e.attr[BagOfKeywords].remove(parting.attr[BagOfKeywords].value)(d)
      e = e.parentEntity
    }
  }
  protected def createAttributesForMergeUp(e1:PaperEntity,e2:PaperEntity,parent:PaperEntity)(implicit d:DiffList):Unit ={
    parent.attr[Title].set(e1.title.value)
    parent.attr[BagOfAuthors].add(e1.attr[BagOfAuthors].value)(d)
    parent.attr[BagOfAuthors].add(e2.attr[BagOfAuthors].value)(d)
    parent.attr[BagOfVenues].add(e1.attr[BagOfVenues].value)(d)
    parent.attr[BagOfVenues].add(e2.attr[BagOfVenues].value)(d)
    parent.attr[BagOfKeywords].add(e1.attr[BagOfKeywords].value)(d)
    parent.attr[BagOfKeywords].add(e2.attr[BagOfKeywords].value)(d)
  }
}

abstract class BibSampler[E<:HierEntity with HasCanopyAttributes[E] with Prioritizable](model:TemplateModel) extends HierCorefSampler[E](model){
  var numAccepted = 0
  protected var canopies = new HashMap[String,ArrayBuffer[E]]
  protected var _amountOfDirt = 0.0
  protected var _numSampleAttempts = 0.0 //TODO, could this overflow?
  var proposalCount = 0
  protected var totalTime:Long=0L
  protected var intervalTime:Long=0L
  override def timeAndProcess(n:Int):Unit = {
    println("About to take "+ n + " samples.")
    totalTime=System.currentTimeMillis
    intervalTime=totalTime
    numAccepted=0
    proposalCount=0
    super.process(n)
  }
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
    //println("Number of canopies: "+canopies.size)
    //for((k,v) <- canopies)println("  -"+k+":"+v.size)
  }
  override def nextEntity(context:E=null.asInstanceOf[E]):E = {
    var result:E=null.asInstanceOf[E]
    if(context==null)result=sampleEntity(entities)
    else {
      val cname = context.canopyAttributes.sampleUniformly(random).canopyName
      val canopy = canopies.getOrElse(cname,{val c = new ArrayBuffer[E];c+=context;c})
      result= if(canopy.size>0)sampleEntity(canopy) else sampleEntity(entities)//{val c = new ArrayBuffer[E];c+=context;c})
    }
    if(result==null)result = context
    result
  }
/*
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
  }*/

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
    if(proposal.diff.size>0)numAccepted += 1
    for(diff<-proposal.diff){
      diff.variable match{
        case bag:BagOfWordsVariable => bag.accept
        //case bag:TrueBow => bag.accept
        case _ => {}
      }
    }
    proposalCount += 1
    if(proposalCount % 10000==0)
      print(".")
    if(proposalCount % (10000*20)==0){
      var pctAccepted = numAccepted.toDouble/proposalCount.toDouble*100
      println(" No. samples: "+proposalCount+", %accepted: "+pctAccepted+", time: "+(System.currentTimeMillis-intervalTime)/1000L+"sec., total: "+(System.currentTimeMillis-totalTime)/1000L+" sec.  Samples per sec: "+ (proposalCount.toInt/(((System.currentTimeMillis-totalTime+1L)/1000L).toInt))+" Accepted per sec: "+(numAccepted/(((System.currentTimeMillis-totalTime+1L)/1000L).toInt)))
      intervalTime = System.currentTimeMillis
    }
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
    protected def promotionChanges:Seq[(String,String)] ={
      val result = new ArrayBuffer[(String,String)]
      for((id, cubbie) <- _id2cubbie){
        if(cubbie.pid.isDefined){
          val oldPromoted = cubbie.pid.value.toString
          val newPromoted = _id2entity(cubbie.pid.value).entityRoot.asInstanceOf[PaperEntity].promotedMention.value
          //val newPromoted = _id2entity(cubbie.id).entityRoot.asInstanceOf[PaperEntity].promotedMention.value
          if(oldPromoted != newPromoted)result += oldPromoted -> newPromoted
        }
      }
      result
    }
    protected def promotionCreations(promotionChanges:Seq[(String,String)],entities:Seq[PaperEntity]) ={
      val result = new ArrayBuffer[PaperEntity]
      val promoted = new HashSet[String]
      for((oldPromoted,newPromoted) <- promotionChanges){
        promoted += oldPromoted
        promoted += newPromoted
      }
      for(e <- entities)
        if(e.promotedMention.value!=null)
          result += e
      result
    }
    protected def createPromotedMentions(promoted:PaperEntity):Unit ={
    }
    protected def changePromoted(oldPromotedId:String, newPromotedId:String):Unit ={
      println("CHANGE PROMOTED: "+oldPromotedId+" --> "+newPromotedId)
      entityCubbieColl.update(_.pid.set(oldPromotedId),_.pid.update(newPromotedId),false,true) //TODO: optimize this later, it is not necessary when entire paper entity is in memory,
      authorColl.entityCubbieColl.remove(_.pid.set(oldPromotedId).isMention.set(true).parentRef.exists(false)) //remove all singletons
      authorColl.entityCubbieColl.update(_.pid.set(oldPromotedId),_.pid.update(newPromotedId).inferencePriority.update(0.0),false,true) //TODO: we can optimize this also if all authors are in memory
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
  def createAuthorsFromPaper(p:PaperEntity):Unit ={
    
  }
  def insertLabeledRexaMentions(rexaFile:File):Unit ={
    val paperEntities = RexaLabeledLoader.load(rexaFile)
    add(paperEntities)
  }
  def insertMentionsFromBibDir(bibDir:File):Unit ={
    if(!bibDir.isDirectory)throw new Exception("Error "+bibDir + " is not a directory.")
    println("Inserting mentions from bib directory: "+bibDir)
    var size = bibDir.listFiles.size
    var count = 0
    for(file <- bibDir.listFiles.filter(_.isFile)){
      insertMentionsFromBibFile(file)
      count += 1
      if(count % 10 == 0)print(".")
      if(count % 200 == 0 || count==size)println(" Processed "+ count + " documents, but skipped "+skipped + ".")
    }
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
  def loadBibTeXFile(file:File):Seq[PaperEntity] ={
    val result = new ArrayBuffer[PaperEntity]
    val fileText = scala.io.Source.fromFile(file).toArray.mkString
    loadBibTeXFile(fileText,file,result)
  }
  def loadBibTeXFile(fileText:String,file:File,result:ArrayBuffer[PaperEntity]):Seq[PaperEntity]={
    var docOption:Option[cc.factorie.app.bib.parser.Dom.Document] = None
    try{
      val docOrError = Dom.stringToDom(fileText, false)
      docOrError match{
        case Right(doc:cc.factorie.app.bib.parser.Dom.Document) => docOption = Some(doc)
        case Left(error:String) => {
          //(error+" doc: "+file.getName)
          val split = fileText.split("@")
          if(split.length>=3){
            for(s <- split)loadBibTeXFile("@"+s,file,result)
            skipped += 1
          }
        }
      }
    }
    catch{
      case e:Exception => {
          skipped += 1
        //println(e.getMessage+" doc: "+file.getName())
        //skipped += 1
      }
    }
    if(docOption!=None){
      val doc = docOption.get
      for((provenance, entry) <- doc.entries){
        result += loadBibTexEntry(entry,provenance)
      }//end for((provenance, entry) <- doc.entries)
      numParsed += 1
    }//end .bib parsed correctly
    def loadBibTexEntry(entry:Entry,provenance:String):PaperEntity ={
      val paperEntity = new PaperEntity("DEFAULT",true)
      //result += paperEntity
      val entryType = entry.ty
      //val authors = new ArrayBuffer[AuthorEntity]
      for(author <- entry.authors.getOrElse(Nil)){
        val first = author.first
        val middle = author.von
        var last = author.last
        val authorEntity = new AuthorEntity(first,middle,last,true)
        val labelTest = last.split("\\[")
        if(labelTest.length==2){
          if(labelTest(1).matches("[0-9]+\\]")){
            authorEntity.groundTruth=Some(labelTest(1).substring(0,labelTest(1).length-1))
            //authorentity.setClusterID(labelTest(1).substring(0,labelTest(1).length-1).toInt)
            last = labelTest(0)
            authorEntity.fullName.setLast(last)(null)
          }
        }
        paperEntity.authors += authorEntity
      }
      for(editor <- entry.editors.getOrElse(Nil)){
        //TODO
      }
      for((name,value) <- entry.otherFields){
        var xv=value.toString.replaceAll("[\r\n\t ]+"," ").replaceAll("[^A-Za-z0-9\\-\\.,\\(\\) ]","")
        if(name != "authors" && name != "editors" && name != "mentions" && name!="entity_ref"){
          if(xv.startsWith("{"))xv=xv.substring(1,xv.length-1)
          if(name == "title")paperEntity.title.set(xv)(null)
          if(name == "year"){
            xv = xv.replaceAll("[^0-9]","")
            if(xv.length==0 || xv.length>4)xv="-1"
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
      }//end for((name,value) <- entry.otherFields)
      paperEntity
    } //end loadBibTexEntry
    result
  }

  def loadBibTeXFileOLD(file:File):Seq[PaperEntity]={
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
        //println("\n=================================")
        //e.printStackTrace
        //println("=================================")
        //println("=================================")
        //println("ill-formated bib entry in file: " + file.getName)
        //println("  total skipped: " + skipped)
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
  def filterFieldNameForMongo(s:String) = FeatureUtils.filterFieldNameForMongo(s)//s.replaceAll("[$\\.]","")
  def addFeatures(author:AuthorEntity):Unit ={
    if(author.fullName.firstName.trim.length>0)author.bagOfFirstNames += author.fullName.firstName.trim
    if(author.fullName.middleName.trim.length>0)author.bagOfMiddleNames += author.fullName.middleName.trim
    if(author.groundTruth!=None)author.bagOfTruths += author.groundTruth.get
    val paper = author.paper
    if(paper!=null){
      for(coAuthor <- paper.authors){
        if(coAuthor.ne(author)){
          val coauthorString = FeatureUtils.firstInitialLastName(coAuthor)
          if(coauthorString.length>0)author.bagOfCoAuthors += coauthorString
        }
      }
      if(paper.venueName!=null && paper.venueName.value.length>0)
        for(tok<-FeatureUtils.venueBag(paper.venueName.value))
          author.bagOfVenues.add(filterFieldNameForMongo(tok))(null)
      author.bagOfKeywords.add(paper.bagOfKeywords.value)(null)
      if(author.bagOfKeywords.value.size>0)println("BagOfKeywords: "+author.bagOfKeywords.value)
    }else println("Warning: paper is null for author with id "+author.id+" cannot compute features.")
  }
  def addFeatures(paper:PaperEntity):Unit ={
    //for(author <- paper.authors)
    //  paper.bagOfAuthors += filterFieldNameForMongo(FeatureUtils.firstInitialLastName(author))
    if(paper.venueName!=null && paper.venueName.value.length>0){
      for(tok<-FeatureUtils.venueBag(paper.venueName.value)){
        //paper.bagOfKeywords.add(filterFieldNameForMongo(tok))(null)
        paper.bagOfVenues.add(filterFieldNameForMongo(tok))(null)
      }
    }
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
                authorEntity.groundTruth=Some(labelTest(1).substring(0,labelTest(1).length-1))
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
  protected def changePriority(e:E):Unit ={
    //e.priority = scala.math.exp(e.priority - random.nextDouble)
    if(random.nextDouble>0.25)e.priority = scala.math.min(e.priority,random.nextDouble)
    else e.priority = random.nextDouble
  }
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
    for(e <- entitiesToStore)changePriority(e)
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
  val entityCubbieColl = new MongoCubbieCollection(entityColl,() => newEntityCubbie,(a:C) => Seq(Seq(a.canopies),Seq(a.inferencePriority),Seq(a.parentRef),Seq(a.bagOfTruths))) with LazyCubbieConverter[C]
  protected def removeEntities(deleted:Seq[E]):Unit = for(e <- deleted)entityCubbieColl.remove(_.idIs(entity2cubbie(e).id))
  //TODO: generalize MongoSlot so that we can move this into the EnttiyCollection class
  def drop:Unit = entityColl.drop
  def loadLabeled:Seq[E] ={
    reset
    val result = new ArrayBuffer[C]
    result ++= entityCubbieColl.query(_.bagOfTruths.exists(true))
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
  def nextBatch(n:Int=10):Seq[E] ={
    reset
    val canopyHash = new HashSet[String]
    var result = new ArrayBuffer[C]
    var topPriority = new ArrayBuffer[C]
    val sorted = entityCubbieColl.query(null,_.canopies.select.inferencePriority.select).sort(_.inferencePriority(-1))
    println("Printing top canopies")
    for(i <- 0 until n)if(sorted.hasNext){
      topPriority += sorted.next
      if(i<10)println("  "+topPriority(i).inferencePriority.value+": "+topPriority(i).canopies.value.toSeq)
    }
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
    println("Loaded "+n+" entities to determine canopies. Found "+result.size+" canopies.")
    println("  initial entities: "+initialEntities.size)
    println("  initial cubbies : "+result.size)
    println("  _id2cubbie: "+ _id2cubbie.size)
    println("  _id2entity: "+ _id2entity.size)
    assembleEntities(result, (id:Any)=>{_id2entity(id)})
    initialEntities
  }
}

/*
class TemporaryMultiBagOfWords(wrappedBags:Iterable[BagOfWords]) extends BagOfWords{
  val bags = new ArrayBuffer[BagOfWords]
  for(bag <-wrappedBags)bags += bag
  lazy val size:Int = throw new Exception("Not yet implemented")

  def *(that:BagOfWords):Double ={
    
  }
  def asSparseBagOfWords:SparseBagOfWords = {null}
}
*/


object RexaLabeledLoader{
  var noAuthorInFocusCount = 0
  def load(rexaDir:File):Seq[PaperEntity]={
    var result = new ArrayBuffer[PaperEntity]
    for(ambiguousNameDir <- rexaDir.listFiles){
      if(ambiguousNameDir.listFiles!=null){
        for(entityDir <- ambiguousNameDir.listFiles){
          System.out.flush
          val label = rexaDir.getName+"/"+ambiguousNameDir.getName+"-"+entityDir.getName
          if(entityDir.listFiles != null)
            for(mentionFile <- entityDir.listFiles)
              result += rexaMention2Paper(mentionFile,label)
          else println("Surprising error, listFiles returning null for directory: "+entityDir)
        }
      }
      else println("Surprising error, listFiles returning null for directory: "+ambiguousNameDir)
    }
    println("Could not find the author in focus "+noAuthorInFocusCount + " times.")
    result
  }
  def rexaMention2Paper(file:File,labelString:String):PaperEntity ={
    val paper = new PaperEntity("DEFAULT",true)
    var authorInFocus = new Array[String](3)
    for(line <- scala.io.Source.fromFile(file).getLines.toSeq.reverse){ //for some reason scala loads files in reverse order...
      val split = line.split(":",2)
      if(split.length==2){
        val value = split(1).trim
        split(0) match {
          case "author-in-focus" => {
            authorInFocus(0) = extractFlatSGML(value,"f")
            authorInFocus(1) = extractFlatSGML(value,"m")
            authorInFocus(2) = extractFlatSGML(value,"l")
          }
          case "authorlist" => {
            for(authorString <- value.split("%%")){
              val author = new AuthorEntity("f","m","l",true)
              author.fullName.setFirst(extractFlatSGML(authorString,"f"))(null)
              author.fullName.setMiddle(extractFlatSGML(authorString,"m"))(null)
              author.fullName.setLast(extractFlatSGML(authorString,"l"))(null)
              paper.authors += author
            }
          }
          case "alt-authorlist" => {}
          case "author-in-focus-score" => {}
          case "title" => {
            paper.title.set(value)(null)
            for(keyword <- value.toLowerCase.split(" +"))paper.bagOfKeywords += FeatureUtils.filterFieldNameForMongo(keyword)
          }
          case "altTitle" => {}
          case "editor" => {}
          case "body" => {}
          case "abstract" => {}
          case "keywords" => for(keyword <- value.split(", "))paper.bagOfKeywords += FeatureUtils.filterFieldNameForMongo(keyword.toLowerCase)
          case "keyword" => for(keyword <- value.split(", "))paper.bagOfKeywords += FeatureUtils.filterFieldNameForMongo(keyword.toLowerCase)
          case "journal" => paper.venueName.set(value)(null)
          case "institution" => for(keyword <- value.toLowerCase.split(", "))paper.bagOfKeywords += FeatureUtils.filterFieldNameForMongo(keyword)
          case "email" => for(email <- value.split(", "))paper.bagOfKeywords += email.toLowerCase.replaceAll("\\.","DOT")
          case "year" => {}//paper.year.set(value)(null)
          case _ => println("(RexaLabeledLoader) Field: "+split(0)+" not used.")
        }
      }
    }
    if(authorInFocus(0)!="" || authorInFocus(1)!="" || authorInFocus(2)!=""){
      var foundAuthorInFocus=false
      for(author <- paper.authors){
        //println("authorM: "+author.fullName.firstName+"|"+author.fullName.middleName+"|"+author.fullName.lastName)
        //println("authorI: "+authorInFocus(0)+"|"+authorInFocus(1)+"|"+authorInFocus(2))
        if(author.fullName.firstName==authorInFocus(0) && author.fullName.middleName==authorInFocus(1) && author.fullName.lastName==authorInFocus(2)){
          foundAuthorInFocus=true
          author.groundTruth=Some(labelString)
          //println("  found ground truth: "+labelString)
        }
      }
      if(!foundAuthorInFocus)noAuthorInFocusCount += 1
    }
    paper
  }
  def extractFlatSGML(string:String,tag:String):String ={
    var start = string.indexOf("<"+tag+">")
    if(start == -1)return ""
    start += ("<"+tag+">").length
    val end = string.indexOf("</"+tag+">")
    string.substring(start,end)
  }
  /*
    author-in-focus:  <n><f>Adam</f><l>Blum</l></n>
authorlist:  <n><f>Adam</f><l>Blum</l></n>%%
alt-authorlist:  <n><f>Adam</f><l>Blum</l></n>
author-in-focus-score:  1.0
title: Microsoft English Query 7.5: Automatic Extraction of Semantics from Relational Databases and OLAP Cubes


author-in-focus:  <n><f>D</f><l>Allen</l></n>
authorlist:      <n><f>D</f><l>Allen</l></n>%%<n><f>T</f><l>Simon</l></n>%%<n><f>F</f><l>Sablitzky</l></n>%%<n><f>K</f><l>Rajewsky</l></n>%%<n><f>A</f><l>Cumano</l></n>
alt-authorlist:  <n><f>D</f><l>Allen</l></n>%%<n><f>T</f><l>Simon</l></n>%%<n><f>F</f><l>Sablitzky</l></n>%%<n><f>K</f><l>Rajewsky</l></n>%%<n><f>A</f><l>Cumano</l></n>
altTitle:  Fundamental Immunology
author-in-focus-score:  0.8168709

editor:  ed. W.E. Paul, Lippincott, Williams and Wilkins. Fundamental immunology, Paul, W.E. ed.,
title: response,
     */
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

/*
object CorefDimensionDomain extends EnumDomain {
  override def size=1000
  this.ensureSize(1000)
  override def frozen=false
  override def getValue(category:String): ValueType = {
    _frozen=false
    super.getValue(category)
  }
  //val Bias, ExactMatch, SuffixMatch, EntityContainsMention, EditDistance2, EditDistance4, NormalizedEditDistance9, NormalizedEditDistance5, Singleton = Value
}
object CorefDomain extends CategoricalVectorDomain[String] {
  override lazy val dimensionDomain = CorefDimensionDomain
  def printWeightsOld(model:TemplateModel){
    for(template<-model.familiesOfClass(classOf[DotFamily])){
      for(i<-template.weights.activeDomain){
        if(template.weights(i)!=0){
          val feature = dimensionDomain.getCategory(i)
          println(template.weights(i)+": "+feature)
        }
      }
    }
  }
  def printWeights(model:TemplateModel){
    println("\n====PRINTING WEIGHTS====")
    val positive = new ArrayBuffer[(String,Double)]
    val negative = new ArrayBuffer[(String,Double)]
    for(template<-model.familiesOfClass(classOf[DotFamily])){
      for(i<-template.weights.activeDomain){
        if(template.weights(i)>0)positive += dimensionDomain.getCategory(i) -> template.weights(i)
        else if(template.weights(i)<0)negative += dimensionDomain.getCategory(i) -> template.weights(i)
      }
    }
    val sortedPositive = positive.sortWith(_._2 < _._2)
    val sortedNegative = negative.sortWith(_._2 < _._2)
    for((feature,weight) <- sortedPositive)println(weight+":"+feature)
    for((feature,weight) <- sortedNegative)println(weight+":"+feature)
    println("POSITIVE: "+positive.size+" "+sortedPositive.size)
    println("NEGATIVE: "+negative.size+" "+negative.size)
  }
}
class AuthorFeatureVector extends FeatureVectorVariable[String] {
  def domain = CorefDomain
}

class ParameterizedAuthorCorefModel extends TemplateModel{
  this ++= (new AuthorCorefModel).templates
  this += new ChildParentTemplate[BagOfCoAuthors] with DotStatistics1[AuthorFeatureVector#Value]{
    val name="cpt-bag-coauth-"
    override def statisticsDomains = List(CorefDimensionDomain)
    //override def unroll1(er:EntityRef) = if(er.dst!=null)Factor(er, er.src.attr[BagOfCoAuthors], er.dst.entityRoot.attr[BagOfCoAuthors]) else Nil
    override def unroll2(childBow:BagOfCoAuthors) = Nil
    override def unroll3(parentBow:BagOfCoAuthors) = Nil
    //def statistics (values:Values) = Stat(new AffinityVector(values._2, values._3).value)
    def statistics(values:Values) = {
      val features = new AuthorFeatureVector
      val childBow = values._2
      val parentBow = values._3
      //val dot = childBow.deductedDot(parentBow,childBow)
      //if(dot == 0.0)
      //  features.update(name+"intersect=empty",childBow.l2Norm*parentBow.l2Norm - 0.25)
      if(childBow.size>0 && parentBow.size>0){
        val cossim=childBow.cosineSimilarity(parentBow,childBow)
        val binNames = FeatureUtils.bin(cossim,name+"cosine")
        for(binName <- binNames){
          features.update(binName,1.0)
          //if(parentBow.l1Norm==2)features.update(binName+"ments=2",1.0)
          //if(parentBow.l1Norm>=2)features.update(binName+"ments>=2",1.0)
          //if(parentBow.l1Norm>=4)features.update(binName+"ments>=4",1.0)
          //if(parentBow.l1Norm>=8)features.update(binName+"ments>=8",1.0)
        }
        //if(dot>0)features.update(name+"dot>0",1.0) else features.update(name+"dot=0",1.0)
      }
      //features.update(name+"cosine-no-shift",cossim)
      //features.update(name+"cossine-shifted",cossim-0.5)
      Stat(features.value)
    }
  }
  this += new ChildParentTemplate[BagOfVenues] with DotStatistics1[AuthorFeatureVector#Value]{
    override def statisticsDomains = List(CorefDimensionDomain)
    val name="cpt-bag-ven-"
    //override def unroll1(er:EntityRef) = if(er.dst!=null)Factor(er, er.src.attr[BagOfVenues], er.dst.entityRoot.attr[BagOfVenues]) else Nil
    override def unroll2(childBow:BagOfVenues) = Nil
    override def unroll3(childBow:BagOfVenues) = Nil
    def statistics(values:Values) = {
      val features = new AuthorFeatureVector
      val childBow = values._2
      val parentBow = values._3
      //val dot = childBow.deductedDot(parentBow,childBow)
      //if(dot == 0.0)
      //  features.update(name+"intersect=empty",childBow.l2Norm*parentBow.l2Norm)
      if(childBow.size>0 && (parentBow.size-childBow.size>0)){
        val cossim=childBow.cosineSimilarity(parentBow,childBow)
        val binNames = FeatureUtils.bin(cossim,name+"cosine")
        for(binName <- binNames)features.update(binName,1.0)
        //if(dot>0)features.update(name+"dot>0",1.0) else features.update(name+"dot=0",1.0)
      }
      Stat(features.value)
    }
  }
  this += new ChildParentTemplate[BagOfKeywords] with DotStatistics1[AuthorFeatureVector#Value]{
    override def statisticsDomains = List(CorefDimensionDomain)
    val name = "cpt-bag-keyw-"
    //override def unroll1(er:EntityRef) = if(er.dst!=null)Factor(er, er.src.attr[BagOfKeywords], er.dst.entityRoot.attr[BagOfKeywords]) else Nil
    override def unroll2(childBow:BagOfKeywords) = Nil
    override def unroll3(childBow:BagOfKeywords) = Nil
    def statistics(values:Values) ={
      val features = new AuthorFeatureVector
      val childBow = values._2
      val parentBow = values._3
      //val dot = childBow.deductedDot(parentBow,childBow)
      //if(dot == 0.0)
      //  features.update(name+"intersect=empty",childBow.l2Norm*parentBow.l2Norm)
      if(childBow.size>0 && (parentBow.size-childBow.size>0)){
        val cossim=childBow.cosineSimilarity(parentBow,childBow)
        val binNames = FeatureUtils.bin(cossim,name+"cosine")
        for(binName <- binNames)features.update(binName,1.0)
        //if(dot>0)features.update(name+"dot>0",1.0) else features.update(name+"dot=0",1.0)
      }
      Stat(features.value)
    }
  }
  this += new EntityBagTemplate[BagOfFirstNames]{
    def name:String = "ebt-firstName-"
    pwFeatures += new PWNameFeatures
  }
  this += new EntityBagTemplate[BagOfMiddleNames]{
    def name:String = "ebt-middleName-"
    pwFeatures += new PWNameFeatures
  }
  /*
  this += new Template1[BagOfCoAuthors] with DotStatistics1[AuthorFeatureVector#Value]{
    override def statisticsDomains = List(CorefDimensionDomain)
    val name = "bag-coauth-"
    def statistics(values:Values) ={
      val features = new AuthorFeatureVector
      val bag = values._1
      val size = bag.size.toDouble
      val weight = bag.l1Norm.toDouble
      val spread = if(size==0.0)0.0 else weight/size
      features.update(name+"spread", Math.exp(-spread))
      Stat(features.value)
    }
  }*/

/*
  this += new Template1[BagOfFirstNames] with DotStatistics1[AuthorFeatureVector#Value]{
    override def statisticsDomains = List(CorefDimensionDomain)
    val name = "bag-firstnames-"
    def statistics(values:Values) ={
      val features = new AuthorFeatureVector
      val bag = values._1
      val bagSeq = bag.iterator.toSeq
      var firstLetterMismatches = 0
      var nameMismatches = 0
      var i=0;var j=0;
      while(i<bagSeq.size){
        val (wordi,weighti) = bagSeq(i)
        j=i+1
        while(j<bagSeq.size){
          val (wordj,weightj) = bagSeq(j)
          if(wordi.charAt(0) != wordj.charAt(0))firstLetterMismatches += 1 //weighti*weightj
          else if(FeatureUtils.isInitial(wordi) && FeatureUtils.isInitial(wordj) && wordi.length==wordj.length && wordi!=wordj)firstLetterMismatches += 1
          if(wordi.length>1 && wordj.length>1 && !FeatureUtils.isInitial(wordi) && !FeatureUtils.isInitial(wordj) && wordi!=wordj)nameMismatches += 1 //wordi.editDistance(wordj)
          j += 1
        }
        i += 1
      }
      //if(firstLetterMismatches>1 || nameMismatches>1)
      //  println("FIRST NAME: "+firstLetterMismatches+" bag: "+bag)
      if(firstLetterMismatches>=1)features.update(name+"firstLetterMMGT1",1.0)
      //if(bag.variable.asInstanceOf[BagOfFirstNames].entity.attr[BagOfTruths].value.size==1)println("FIRST LETTER MM BAG: "+bag)
      //if(firstLetterMismatches>=2)features.update(name+"firstLetterMMGT2",1.0)
      //if(firstLetterMismatches>=4)features.update(name+"firstLetterMMGT4",1.0)
      //if(firstLetterMismatches>=8)features.update(name+"firstLetterMMGT8",1.0)
      if(nameMismatches>=1)features.update(name+"nameMMGT1",1.0)
      //if(nameMismatches>=2)features.update(name+"nameMMGT2",nameMismatches
      //if(nameMismatches>=4)features.update(name+"nameMMGT4",1.0)
      //if(nameMismatches>=8)features.update(name+"nameMMGT8",1.0)
      Stat(features.value)
    }
  }
  this += new Template1[BagOfMiddleNames] with DotStatistics1[AuthorFeatureVector#Value]{
    override def statisticsDomains = List(CorefDimensionDomain)
    val name = "bag-middlenames-"
    def statistics(values:Values) ={
      val features = new AuthorFeatureVector
      val bag = values._1
      val bagSeq = bag.iterator.toSeq
      var firstLetterMismatches = 0
      var nameMismatches = 0
      var i=0;var j=0;
      while(i<bagSeq.size){
        val (wordi,weighti) = bagSeq(i)
        j=i+1
        while(j<bagSeq.size){
          val (wordj,weightj) = bagSeq(j)
          if(wordi.charAt(0) != wordj.charAt(0))firstLetterMismatches += 1 //weighti*weightj
          else if(FeatureUtils.isInitial(wordi) && FeatureUtils.isInitial(wordj) && wordi.length==wordj.length && wordi!=wordj)firstLetterMismatches += 1
          if(wordi.length>1 && wordj.length>1 && !FeatureUtils.isInitial(wordi) && !FeatureUtils.isInitial(wordj) && wordi!=wordj)nameMismatches += 1 //wordi.editDistance(wordj)
          j += 1
        }
        i += 1
      }
      if(firstLetterMismatches>=1)features.update(name+"firstLetterMMGT1",1.0)
      //if(firstLetterMismatches>=2)features.update(name+"firstLetterMMGT2",1.0)
      //if(firstLetterMismatches>=4)features.update(name+"firstLetterMMGT4",1.0)
      //if(firstLetterMismatches>=8)features.update(name+"firstLetterMMGT8",1.0)
      if(nameMismatches>1)features.update(name+"nameMMGT1",1.0)
      //if(nameMismatches>2)features.update(name+"nameMMGT2",1.0)
      //if(nameMismatches>4)features.update(name+"nameMMGT4",1.0)
      //if(nameMismatches>8)features.update(name+"nameMMGT8",1.0)
      Stat(features.value)
    }
  }
  */

  this += new ChildParentTemplate[FullName] with DotStatistics1[AuthorFeatureVector#Value] {
    override def statisticsDomains = List(CorefDimensionDomain)
    val name = "cpt-fullname-"
    def statistics(v:Values) = {
      val features = new AuthorFeatureVector
      val childName = v._2
      val parentName = v._3
      val childFirst = FeatureUtils.normalizeName(childName(0)).toLowerCase
      val childMiddle = FeatureUtils.normalizeName(childName(1)).toLowerCase
      val childLast = FeatureUtils.normalizeName(childName(2)).toLowerCase
      val parentFirst = FeatureUtils.normalizeName(parentName(0)).toLowerCase
      val parentMiddle = FeatureUtils.normalizeName(parentName(1)).toLowerCase
      val parentLast = FeatureUtils.normalizeName(parentName(2)).toLowerCase
      if(childLast != parentLast)features.update(name+"cl!=pl",1.0)
      if(initialsMisMatch(childFirst,parentFirst))features.update(name+"initials:cf!=pf",1.0)
      if(initialsMisMatch(childMiddle,parentMiddle))features.update(name+"initials:cm!=pm",1.0)
      if(nameMisMatch(childFirst,parentFirst)){
        features.update(name+"nmm:cf!=pf",1.0)
        //if(parentName.entity.attr[BagOfTruths].value.size==1){
        //  println("NMM:CF!=PF")
        //  println("  "+childFirst+" "+parentFirst)
        //}
      }
      if(nameMisMatch(childMiddle,parentMiddle))features.update(name+"nmm:cm!=pm",1.0)
      //if(parentMiddle.length > childMiddle.length)result += 0.5
      //if(parentFirst.length > childFirst.length)result += 0.5
      //if((childMiddle.length==0 && parentMiddle.length>0) || (parentMiddle.length==0 && childMiddle.length>0))result -= 0.25
      Stat(features.value)
    }
    def initialsMisMatch(c:String,p:String):Boolean = (c!=null && p!=null && c.length>0 && p.length>0 && c.charAt(0)!=p.charAt(0))
    def nameMisMatch(c:String,p:String):Boolean = (c!=null && p!=null && !FeatureUtils.isInitial(c) && !FeatureUtils.isInitial(p) && c != p)
  }
  this += new Template3[EntityExists,IsEntity,IsMention] with DotStatistics1[AuthorFeatureVector#Value]{
    override def statisticsDomains = List(CorefDimensionDomain)
    val name = "structural-"
    def unroll1(exists:EntityExists) = Factor(exists,exists.entity.attr[IsEntity],exists.entity.attr[IsMention])
    def unroll2(isEntity:IsEntity) = Factor(isEntity.entity.attr[EntityExists],isEntity,isEntity.entity.attr[IsMention])
    def unroll3(isMention:IsMention) = throw new Exception("An entitie's status as a mention should never change.")
    def statistics(v:Values) ={
      val features = new AuthorFeatureVector
      val exists:Boolean = v._1.booleanValue
      val isEntity:Boolean = v._2.booleanValue
      val isMention:Boolean = v._3.booleanValue
      if(exists && isEntity) features.update(name+"entity",1.0)
      //if(exists && !isEntity && !isMention)result -= subEntityExistenceCost
      Stat(features.value)      
    }
  }
  this += new StructuralPriorsTemplate(0.0,-0.25)
  //this += new StructuralPriorsTemplate(1.0,0.25)
  //this += new StructuralPriorsTemplate(4.0,0.25)
}


trait PWBagFeatures{
  var accumulated = 0.0
  var conditionalNormalizer = 0.0
  def reset:Unit = {
    accumulated = 0.0
    conditionalNormalizer = 0.0
  }
  def exists:Boolean = (accumulated>0.0)
  def forAll:Boolean = (accumulated==conditionalNormalizer)
  def avg:Double = accumulated/conditionalNormalizer
  def name:String
  def condition(si:String,wi:Double,sj:String,wj:Double):Boolean
  def value(si:String,wi:Double,sj:String,wj:Double):Double
  def process(si:String,wi:Double,sj:String,wj:Double, selfCompare:Boolean):Unit ={
    if(condition(si,wi,sj,wj)){
      val v = value(si,wi,sj,wj)
      //println("si: "+si+" sj: "+sj)
      //println("  value: "+v)
      val weight = if(selfCompare)wi*(1.0 - wi) else wi * wj
      accumulated += v * weight
      conditionalNormalizer += 1.0 * weight
    }
  }
  def defaultFeatures:Iterable[(String,Double)] = {
    val result = new ArrayBuffer[(String,Double)]
    if(conditionalNormalizer>0.0){
      if(exists)result += (name+"exists") -> 1.0 else result += (name+"!exists") -> 1.0
      //if(forAll)result += (name+"forall") -> 1.0 else result += (name+"!forall") -> 1.0
      result += (name+"avg") -> avg
    }
    result
  }
}

class PWNameFeatures extends PWBagFeatures{
  def name = "pwnf-"
  def condition(si:String,wi:Double,sj:String,wj:Double):Boolean = (si.length>0 && sj.length>0)
  def value(si:String,wi:Double,sj:String,wj:Double):Double = if((!(!FeatureUtils.isInitial(si) ^ FeatureUtils.isInitial(sj)) && si!=sj) || (si.charAt(0) != sj.charAt(0)))1.0 else 0.0 //if(si!=sj) wi*wj//1.0 else 0.0
}

abstract class EntityBagTemplate[V<:BagOfWordsVariable with EntityAttr](implicit m:Manifest[V]) extends Template2[V,IsEntity] with DotStatistics1[AuthorFeatureVector#Value]{
  override def statisticsDomains = List(CorefDimensionDomain)
  def unroll1(bag:V) = if(bag.entity.isRoot)Factor(bag,bag.entity.attr[IsEntity]) else Nil
  def unroll2(isEntity:IsEntity) = if(isEntity.entity.isRoot)Factor(isEntity.entity.attr[V],isEntity) else Nil
  //def unroll1(bag:V) = if(bag.entity.isRoot)Factor(bag) else Nil
  def name:String
  val pwFeatures = new ArrayBuffer[PWBagFeatures]
  def statistics(values:Values) ={
    val features = new AuthorFeatureVector
    val bag = values._1
    val bagSeq = bag.iterator.toSeq
    var firstLetterMismatches = 0
    var nameMismatches = 0
    var i=0;var j=0;
    for(pwf<-pwFeatures)pwf.reset
    while(i<bagSeq.size){
      val (wordi,weighti) = bagSeq(i)
      j=i
      while(j<bagSeq.size){
        val (wordj,weightj) = bagSeq(j)
        for(pwf<-pwFeatures)pwf.process(wordi,weighti,wordj,weightj, (i==j))
        j += 1
      }
      i += 1
    }
    for(pwf<-pwFeatures)
      for((featureName,featureWeight) <- pwf.defaultFeatures)
        features.update(name+featureName, featureWeight)
    Stat(features.value)
  }
}
*/


