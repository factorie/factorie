package cc.factorie.app.bib
import cc.factorie.app.nlp.coref._
import com.mongodb.DBCollection
import cc.factorie.db.mongo.MutableCubbieCollection
import collection.mutable.{HashSet, HashMap, ArrayBuffer}
import java.lang.StringBuffer
import cc.factorie.util.{DefaultCmdOptions, Cubbie}
import cc.factorie.app.topics.lda.{Document, LDA,SparseLDAInferencer}
import cc.factorie.{CategoricalSeqDomain, DiffList}
import cc.factorie.generative.GenerativeModel._
import java.io.{PrintWriter, FileWriter, File, BufferedReader, InputStreamReader, FileInputStream}
import cc.factorie.generative.{DiscreteMixtureCounts, GenerativeModel}
import cc.factorie.la.SparseIndexedTensor

object FeatureUtils{
/*
  def cosineDistance(v1:SparseIndexedTensor,v2:SparseIndexedTensor):Double ={
    val numerator = v1 dot v2
    val denominator = v1.twoNorm * v2.twoNorm
    if(denominator==0.0 || denominator != denominator) 0.0 else numerator/denominator
  }
  def cosineDistance(v1:SparseIndexedTensor,v2:SparseIndexedTensor):Double ={

  }*/
  /*
    def cosineSimilarity(that:BagOfWords,deduct:BagOfWords):Double ={
    //val smaller = if(this.size<that.size)this else that
    //val larger = if(that.size<this.size)this else that
    val numerator:Double = this.deductedDot(that,deduct)
    if(numerator!=0.0){
      val thatL2Norm = Math.sqrt(deduct.l2Norm*deduct.l2Norm+that.l2Norm*that.l2Norm - 2*(deduct * that))
      val denominator:Double = this.l2Norm*thatL2Norm
      if(denominator==0.0 || denominator != denominator) 0.0 else numerator/denominator
    } else 0.0
  }
  def cosineSimilarity(that:BagOfWords):Double = {
    val numerator:Double = this * that
    val denominator:Double = this.l2Norm*that.l2Norm
    if(denominator==0.0 || denominator != denominator) 0.0 else numerator/denominator
  }
        case v:SparseIndexedTensor => {
        val v1 = if (this._npos < v._npos) this else v
        val v2 = if (v._npos< this._npos) v else this
        var i = 0; var j = -1; var j2 = 0
        var result = 0.0
        while (i < v1._npos) {
          j2 = v2.position(v1._indexs(i), j+1)
          if (j2 >= 0) { result += v1._values(i) * v2._values(j2); j = j2 }
          i += 1
        }
        result
      }
   */

  //venue projections
  //def isInitial(s:String) = s.matches("[a-z]( [a-z])?")
  def isInitial(s:String) = (s.length==1 && s(0)>='a' && s(0)<='z') || (s.length==3 && s.charAt(1)==' ' && s(0)>='a' && s(0)<='z' && s(2)>='a' && s(2)<='z')
  val venueP0 = "\\(.+\\)";
  val venueP1 = "(in )?[Pp]roceedings( of)?( the)?";
  val venueP2 = "[0-9\\-]+(th|st|rd)?"
  val venueP3 = "([Ee]leventh|[Tt]welfth|[Tt]hirteenth|[Ff]ourteenth|[Ff]ifteenth|[Ss]ixteenth|[Ss]eventeenth|[Ee]ighteenth|[Nn]ineteenth|[Tt]wentieth|[Tt]hirtieth|[Ff]ourtieth)?([Tt]wenty|[Tt]hirty|[Ff]orty)?[- ]?([Ff]irst|[Ss]econd|[Tt]hird|[Ff]ourth|[Ff]ifth|[Ss]ixth|[Ss]eventh|[Ee]ighth|[Nn]ineth)?"
  val venueP4 = "(in the )?[Pp]roceedings of (the )?[a-z0-9]+ "
  val venueP5 = "(([Aa]dvances( ?in ?)?|[Pp]roceedings|[Pp]roc\\.? )) ?"
  val venuePost = " ?([Ee]ndowment|[Ee]ndow|Proceedings|Meeting)\\.?"
  val venForAuthStops = "(advances|association|meeting|assoc|annual|proceedings|proc|endowment|acm|ieee|.iprioendow|conference|journal|workshop|international|symposium|appear|[a-z]+eenth|[a-z]+tieth|first|second|third|fourth|fifth|sixth|seventh|eighth|nineth|tenth|eleventh|twelfth|in|the|of|to)"
  val tokenFilterString = "[^A-Za-z0-9]"
  def normalizeName(name:String) = deAccent(name).replaceAll("[^A-Za-z ]","").replaceAll("[ ]+"," ")
  def filterFieldNameForMongo(s:String) = s.replaceAll("[$\\.]","")
  def venueBag(s:String):Seq[String] = {val toks = new ArrayBuffer[String];toks++=tokenizeVenuesForAuthors(s);toks ++= getVenueAcronyms(s).map(_._1);toks.map(_.toLowerCase).toSeq}
  def deAccent(s:String):String = java.text.Normalizer.normalize(s,java.text.Normalizer.Form.NFD).replaceAll("\\p{InCombiningDiacriticalMarks}+","")
  def filterOnToken(s:String,stopWordsRegEx:String):String = s.split(" ").filter(!_.matches(stopWordsRegEx)).mkString(" ")
  def tokenizeVenuesForAuthors(s:String):Seq[String] ={
    var filtered = s.toLowerCase.replaceAll("\\-"," ").replaceAll("[^a-z ]","")
    filtered = filterOnToken(filtered,venForAuthStops)
    filtered.split("[ ]+")
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
    val filtered = s.replaceAll("\\-"," ").replaceAll("[^A-Za-z\\(\\) ]+","").replaceAll(venueP3,"").replaceAll(venueP5,"").replaceAll(venuePost,"").replaceAll("  +","")
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
    var str = s.toLowerCase.replaceAll("[^a-z0-9 ]","")
    str = str.replaceAll("(a|the|of|in) ","")
    str = str.replaceAll("[ ]+"," ")
    var split = str.split("(proceedings|journal|proc) ")
    if(split.length==1){
      var finalString:String = if(split.length==1) split(0) else split(split.length-1)
      split = finalString.split(" ")
      var result = ""
      if(split.length>=3 && split.length <=5){
        for(init<-split.filter(_.length>0))
          result += init.charAt(0)
      }
      result.toUpperCase
    } else ""
  }
  def firstInitialLastName(author:AuthorEntity):String ={
    var word:String = author.fullName.firstName
    if(word!=null && word.length>0)word=word.charAt(0)+"" else word = ""
    word += author.fullName.lastName
    word.toLowerCase.replaceAll("[^A-Za-z]","")
  }
//  def bin(d:Double, name:String, bins:Seq[Double] = Seq(0.0,0.01,0.1,0.25,0.5,0.75,0.9,0.99,1.0)):Seq[String] = {
  /*
  def bin(d:Double, name:String, bins:Seq[Double] = Seq(0.0,0.5,1.0)):Seq[String] = {
    val result = new ArrayBuffer[String]
    for(bin <- bins){
      if(d>=bin)result += (name+"-bin>"+bin)
      if(d<=bin)result += (name+"-bin<"+bin)
    }
    result
  }
  */
  def bin(d:Double,name:String,ltBins:Seq[Double]=Seq(0.0,0.1,0.5),gtBins:Seq[Double]=Seq(0.0,0.1,0.5)) = binLT(d,name,ltBins) ++ binGT(d,name,gtBins)
  //def bin(d:Double,name:String,ltBins:Seq[Double]=Seq(0.0,0.1,0.25,0.5),gtBins:Seq[Double]=Seq(0.0,0.1,0.25,0.5,0.75,1.0)) = binLT(d,name,ltBins) ++ binGT(d,name,gtBins)
  def binGT(d:Double,name:String,bins:Seq[Double]=Seq(0.1,0.25,0.5,0.75,1.0)) ={
    val result = new ArrayBuffer[String]
    for(bin <- bins)if(d>=bin)result += (name+"-bin>="+bin)
    result
  }
  def binLT(d:Double,name:String,bins:Seq[Double]=Seq(0.0,0.25,0.5)) ={
    val result = new ArrayBuffer[String]
    for(bin <- bins)if(d<=bin)result += (name+"-bin<="+bin)
    result
  }

  def calculateTFIDF(papers:Iterable[PaperEntity],tokenizer:PaperEntity=>Seq[String],tfidf:HashMap[String,Double]):Unit ={
    for(paper <- papers)
      for(token <- tokenizer(paper))
        tfidf(token) = 1.0/(1.0/tfidf.getOrElse(token,0.0)+1.0)
  }
  def js(p:Seq[Double],q:Seq[Double]):Double ={
    val qp = new Array[Double](p.size)
    for(i<-0 until p.size)qp(i) = (q(i) + p(i))/2.0
    (kl(p,qp)+kl(q,qp))/0.5
  }
  def kl(p:Seq[Double],q:Seq[Double]):Double = {
    var result = 0.0
    for(i<-0 until p.length)if(p(i)>0)result += p(i)*Math.log(p(i)/q(i))
    result/Math.log(2)
  }
  /*
  def removeSpuriousNameTokens(name:String):Unit ={
    val names = name.split(" ").filter(_)
    val result = new StringBuffer
    var i=names.length
    var running=true
    whle(i>0 && running){
      i -= 1
      if(names(i).matches("[A-Z][a-z]+") || names(i).matches("[A-Z]\\.?"))running =false
    }
  }*/

}

object EntityUtils{
  def makeSingletons(entities:Seq[AuthorEntity]):Seq[AuthorEntity] ={
    for(e <- entities)
      e.setParentEntity(null)(null)
    entities.filter(_.isObserved).toSeq
  }
  def collapseOnCanopyAndTopics(entities:Seq[AuthorEntity]) = collapseOn(entities,(e:AuthorEntity) => {
    val fmlCanopy = new AuthorFLNameCanopy(e)
    val topics = e.bagOfTopics.value.asHashMap.toList.sortBy(_._2).reverse.take(2).map(_._1)
    if(topics.length==0)None else Some(fmlCanopy.canopyName+topics.mkString(" "))
  })

  def collapseOnTruth(entities:Seq[AuthorEntity]) = collapseOn(entities,(e:AuthorEntity)=>{e.groundTruth})
  def collapseOn(entities:Seq[AuthorEntity], collapser:AuthorEntity=>Option[String]):Seq[AuthorEntity] ={
    val result = new ArrayBuffer[AuthorEntity]
    result ++= makeSingletons(entities)
    val key2entities = new HashMap[String,ArrayBuffer[AuthorEntity]]
    for(e <- entities.filter(collapser(_) != None)){
      val rep = collapser(e).get
      key2entities.getOrElse(rep,{val r = new ArrayBuffer[AuthorEntity];key2entities(rep)=r;r}) += e
    }
    for((label,trueCluster) <- key2entities){
      if(trueCluster.size>1){
        val root = new AuthorEntity
        result += root
        for(e<-trueCluster){
          e.setParentEntity(root)(null)
          for(bag <- e.attr.all[BagOfWordsVariable])root.attr(bag.getClass).add(bag.value)(null)
/*
          root.attr[BagOfCoAuthors].add(e.attr[BagOfCoAuthors].value)(null)
          root.attr[BagOfVenues].add(e.attr[BagOfVenues].value)(null)
          root.attr[BagOfKeywords].add(e.attr[BagOfKeywords].value)(null)
          root.attr[BagOfFirstNames].add(e.attr[BagOfFirstNames].value)(null)
          root.attr[BagOfMiddleNames].add(e.attr[BagOfMiddleNames].value)(null)
          root.attr[BagOfTruths].add(e.attr[BagOfTruths].value)(null)
          */
        }
        root.fullName.setFullName(trueCluster.head.fullName)(null)
      }
    }
    result
  }
  def null2empty(s:String):String = if(s==null)"" else s
  def reExtractNames(fullName:FullName):Unit ={
    var first=""
    var middle=""
    var last=""
    var suffix:String=fullName.suffix
    val name = (null2empty(fullName.firstName)+" "+null2empty(fullName.middleName)+" "+null2empty(fullName.lastName))//+" "+null2empty(fullName.suffix))
      .replaceAll("[Ss]ir ","")
      .replaceAll("[A-Z][a-z]\\.","") //dr., md., etc..
      .replaceAll(" +"," ")
      .trim
    var names = name.split(" ").filter((s:String) => {
      s.matches("[A-Z][a-z]+") || s.matches("[A-Za-z]\\.?")
    })
    if(names.length==0){
      //println("Relaxing capitalization condition for name: "+name)
      names = name.split(" ")
    }
    if(names.length>0){
      // if(names.exists(_.matches("[A-Z][a-z]+")))names = name.replaceAll("")
      last = names(names.length-1)
      if(names.length>=2){
        first=names(0)
      }
      if(names.length>=3){
        for(i<-1 until names.length-1)middle+=names(i)+" "
        middle = middle.trim
      }
      /*
      println("\n====String: "+name+"====")
      println("--Original parse--")
      println("  first : "+fullName.firstName)
      println("  middle: "+fullName.middleName)
      println("  last  : "+fullName.lastName)
      */
      fullName.setFirst(first)(null)
      fullName.setMiddle(middle)(null)
      fullName.setLast(last)(null)
      fullName.setSuffix(suffix)(null)
      /*
      println("--New parse--")
      println("  first : "+fullName.firstName)
      println("  middle: "+fullName.middleName)
      println("  last  : "+fullName.lastName)
      */
    }
    //else println("Blank name")

  }
  /*
    False negative pair
  M1: first: Simon
  M1: last : Jones
  M1: canop: sjones
  M2: first: Simon
  M2: last : Jones (simonpj@dcs.gla.ac.uk

    M1: first: Simon
  M1: last : Jones
  M1: canop: sjones
  M2: first: S
  M2: last : Peyton-Jones
  M2: canop: speytonjones
     */
    //Peyton Jones al

  def makeTruth(entities:Seq[AuthorEntity]):Seq[AuthorEntity] ={
    val result = new ArrayBuffer[AuthorEntity]
    result ++= makeSingletons(entities)
    val key2entities = new HashMap[String,ArrayBuffer[AuthorEntity]]
    for(e <- entities.filter(_.groundTruth != None))
      key2entities.getOrElse(e.groundTruth.get,{val r = new ArrayBuffer[AuthorEntity];key2entities(e.groundTruth.get)=r;r}) += e
    for((label,trueCluster) <- key2entities){
      if(trueCluster.size>1){
        val root = new AuthorEntity
        result += root
        for(e<-trueCluster){
          e.setParentEntity(root)(null)
          root.attr[BagOfCoAuthors].add(e.attr[BagOfCoAuthors].value)(null)
          root.attr[BagOfVenues].add(e.attr[BagOfVenues].value)(null)
          root.attr[BagOfKeywords].add(e.attr[BagOfKeywords].value)(null)
          root.attr[BagOfFirstNames].add(e.attr[BagOfFirstNames].value)(null)
          root.attr[BagOfMiddleNames].add(e.attr[BagOfMiddleNames].value)(null)
          root.attr[BagOfTruths].add(e.attr[BagOfTruths].value)(null)
        }
        root.fullName.setFullName(trueCluster.head.fullName)(null)
        //printAuthors(Seq(root))
        //println("trueCluster: "+trueCluster.size)
      }
    }
    result
  }

  def createBagsForMergeUp(e1:Entity,e2:Entity,parent:Entity)(implicit d:DiffList):Unit ={
    //parent.attr[BagOfTopics].add(e1.attr[BagOfTopics].value)(d)
    //parent.attr[BagOfTopics].add(e2.attr[BagOfTopics].value)(d)
    for(bag <- e1.attr.all[BagOfWordsVariable])parent.attr(bag.getClass).add(bag.value)(d)
    for(bag <- e2.attr.all[BagOfWordsVariable])parent.attr(bag.getClass).add(bag.value)(d)
    //
//    for(bag <- e1.attr.all[BagOfWordsTensorVariable])parent.attr(bag.getClass).increment(bag.value)(d)
//    for(bag <- e2.attr.all[BagOfWordsTensorVariable])parent.attr(bag.getClass).increment(bag.value)(d)
  }

  def propagateBagUp(entity:Entity)(implicit d:DiffList):Unit ={
    //var e = entity.parentEntity
    //while(e!=null){
    //  e.attr[BagOfTopics].add(entity.attr[BagOfTopics].value)(d)
    //  e=e.parentEntity
    //}
    var e = entity.parentEntity
    while(e!=null){
      for(bag <- entity.attr.all[BagOfWordsVariable])
        e.attr(bag.getClass).add(bag.value)(d)
//      for(bag <- entity.attr.all[BagOfWordsTensorVariable])
//        e.attr(bag.getClass).increment(bag.value)(d)
      e=e.parentEntity
    }
  }
  def propagateRemoveBag(parting:Entity,formerParent:Entity)(implicit d:DiffList):Unit ={
   // var e = formerParent
    //while(e!=null){
    //  e.attr[BagOfTopics].remove(parting.attr[BagOfTopics].value)(d)
    //  e=e.parentEntity
    //}
    var e = formerParent
    while(e!=null){
      for(bag <- parting.attr.all[BagOfWordsVariable])
        e.attr(bag.getClass).remove(bag.value)(d)
//      for(bag <- parting.attr.all[BagOfWordsTensorVariable])
//        e.attr(bag.getClass).increment(bag.value * -1.0)(d)
      e=e.parentEntity
    }
  }
  def printAuthors(entities:Seq[AuthorEntity],includeSingletons:Boolean=true):Unit ={
    printEntities(entities,includeSingletons,
      (e:Entity)=>{
        var result:String = e.attr[FullName].toString+" (first:"+bagToString(e.attr[BagOfFirstNames].value)+" middle:"+bagToString(e.attr[BagOfMiddleNames].value)+")"
        if(e.asInstanceOf[AuthorEntity].groundTruth!=None)result="truth:"+e.asInstanceOf[AuthorEntity].groundTruth.get+"  "+result
        result},
      (e:Entity)=>{"{"+bagToString(e.attr[BagOfCoAuthors].value)+"}"})
  }
  def printAuthorsForAnalysis(entities:Seq[AuthorEntity],errorsOnly:Boolean=true,includeSingletons:Boolean=false):Unit ={
    println("Printing authors for error analysis.")
    //val es = entities
    val es = if(errorsOnly)entities.filter(_.attr[BagOfTruths].value.size>1) else entities
    if(errorsOnly)println("Printing clusters with errors: "+ es.size+" out of "+entities.size+".")
    printEntities(es,includeSingletons,
      (e:Entity)=>{
        var result:String = e.attr[FullName].toString+" (first:"+bagToString(e.attr[BagOfFirstNames].value)+" middle:"+bagToString(e.attr[BagOfMiddleNames].value)+")"
        if(e.asInstanceOf[AuthorEntity].groundTruth!=None)result="truth:"+e.asInstanceOf[AuthorEntity].groundTruth.get+"  "+result
        result},
      (e:Entity)=>{
        var result = ""
        if(e.asInstanceOf[AuthorEntity].isObserved){
          if(e.attr[BagOfCoAuthors].value.size>0)result += "{coa: "+bagToString(e.attr[BagOfCoAuthors].value,20)+"} "
          if(e.attr[BagOfKeywords].value.size>0)result += "{keyw: "+bagToString(e.attr[BagOfKeywords].value,20)+"} "
        }else{
        if(e.attr[BagOfCoAuthors].value.size>0)result += "{coa: "+bagToString(e.attr[BagOfCoAuthors].value)+"} "
        if(e.attr[BagOfKeywords].value.size>0)result += "{keyw: "+bagToString(e.attr[BagOfKeywords].value)+"} "
        }
        result
      })
  }
  def printPapers(entities:Seq[PaperEntity],includeSingletons:Boolean=true):Unit ={
    printEntities(entities,includeSingletons,(e:Entity)=>e.attr[Title].value.toString)
  }
  def printEntities(entities:Seq[Entity],includeSingletons:Boolean=true,represent:Entity=>String=(e:Entity)=>"",context:Entity=>String=(e:Entity)=>""):Unit = {
    var count = 0
    for(e <- entities.filter((e:Entity) => {e.isRoot && e.isConnected})){
      if(!e.isObserved || includeSingletons)
        println(entityString(e,represent,context))
      count += 1
    }
    println("Printed " + count + " entities.")
  }
  def entityString(e:Entity,represent:Entity=>String,context:Entity=>String):String = {
    if(e==null)return "null"
    val result = new StringBuffer
    entityString(e,result,0,represent,context)
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
    for(childEntity <- e.childEntitiesIterator)entityString(childEntity,result,depth+1,represent,context)
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


object LDAUtils{
  object WordSeqDomain extends CategoricalSeqDomain[String]
  val model = GenerativeModel()
//cc.factorie.app.strings.StringSegmenter
  def inferTopicsForPapers(papers:Iterable[PaperEntity],lda:LDA,mySegmenter:cc.factorie.app.strings.RegexSegmenter=new cc.factorie.app.strings.RegexSegmenter("\\p{Alpha}+".r)):Unit ={
    var count = 0
    for(paper <- papers.par){
      val doc = Document.fromString(WordSeqDomain,paper.id,DEFAULT_DOCUMENT_GENERATOR(paper),segmenter=mySegmenter)
      if(doc.ws.length>0){
        lda.inferDocumentTheta(doc)
        doc.theta.value.toSeq.zip(0 until lda.phis.size).filter((t:(Double,Int))=>{t._1>0.1}).foreach((t:(Double,Int))=>{paper.bagOfTopics.add(t._2+"",t._1)(null)})
        //doc.theta.value.toSeq.zip(0 until lda.phis.size).filter((t:(Double,Int))=>{t._1>0.001}).foreach((t:(Double,Int))=>{paper.bagOfTopics.add(t._2+"",t._1)(null)})
        //println("PAPER: " + DEFAULT_DOCUMENT_GENERATOR(paper)+"\n  topics: "+doc.theta.value.toSeq.zip(0 until lda.phis.size).filter((t:(Double,Int))=>{t._1>0.0}).mkString(" "))
        count += 1
        if(count%1000==0)print(".");if(count%20000==0)println(count)
      }
    }
    //println("TOPICS: \n"+lda.topicsSummary(10))
  }

  def saveAlphaAndPhi(lda:LDA,file:File){
    val pw = new PrintWriter(file)
    pw.println(lda.phis.size)
    pw.println("/alphas")
    pw.println(lda.alphas.tensor.mkString(" "))
    pw.println()
    for(phi <- lda.phis){
      pw.println("/topic")
      phi.value.foreachActiveElement((index,count)=>{
        val word:String = WordSeqDomain.elementDomain.category(index)
        if(count!=0.0){
          pw.println(word)
          pw.println(count)
        }
      })
    }
    pw.close()
  }

  def loadLDAModelFromAlphaAndPhi(file:File):LDA ={
    println("Loading topic model from phis and alphas.")
    val reader = new BufferedReader(new InputStreamReader(new FileInputStream(file)))
    val numTopics = reader.readLine.toInt
    val lda = new LDA(WordSeqDomain, numTopics)(model)
    lda.phis.foreach(_.tensor.zero())
    reader.mark(512)
    val alphasName = reader.readLine()
    if (alphasName == "/alphas") { // If they are present, read the alpha parameters.
      val alphasString = reader.readLine(); lda.alphas.tensor := alphasString.split(" ").map(_.toDouble) // set lda.alphas
      reader.readLine() // consume delimiting newline
      println("Read alphas "+lda.alphas.tensor.mkString(" "))
    }
    var line = reader.readLine()
    var topicCount= -1
    while(line!=null){
      if(line=="/topic"){
        topicCount += 1
        line=reader.readLine
      }
      val word = line
      val count = reader.readLine.toDouble
      lda.phis(topicCount).tensor.masses.+=(lda.wordDomain.index(word),count)
      line=reader.readLine
    }
    println("Topics: \n"+lda.topicsSummary(10))
    lda
  }

  def main(args:Array[String]) = {
    println("Args: "+args.length)
    for(arg <- args)
      println("  "+arg)
    object opts extends DefaultCmdOptions{
      val ldaDir = new CmdOption("ldaDir","/Users/mwick/data/rexa2/paper_text/","FILE","File where the documents for LDA will be saved/loaded.")
      val bibDirectory = new CmdOption("bibDir","/Users/mwick/data/thesis/all3/","FILE","Pointer to a directory containing .bib files.")
      val dblpLocation = new CmdOption("dblpFile","none","FILE","Pointer to the dblp .xml file")
      //
      val numTopics =     new CmdOption("num-topics", 't', 100, "N", "Number of topics.")
      val alpha =         new CmdOption("alpha", 0.1, "N", "Dirichlet parameter for per-document topic proportions.")
      val beta =          new CmdOption("beta", 0.01, "N", "Dirichlet parameter for per-topic word proportions.")
      val numThreads =    new CmdOption("num-threads", 1, "N", "Number of threads for multithreaded topic inference.")
      val numIterations = new CmdOption("num-iterations", 'i', 200, "N", "Number of iterations of inference.")
      val optimizeBurnIn =new CmdOption("optimize-burn-in", 10, "N", "Number of iterations to run before the first estimation of the alpha parameters")
      val writeModel =     new CmdOption("write-model", "lda-model.txt", "FILENAME", "Save LDA state, writing alphas and phis")
      val readModel =     new CmdOption("read-model", "lda-model.txt", "FILENAME", "Save LDA state, writing alphas and phis")
      val tokenRegex =    new CmdOption("token-regex", "\\p{Alpha}+", "REGEX", "Regular expression for segmenting tokens.")
    }
    opts.parse(args)
    val mySegmenter = new cc.factorie.app.strings.RegexSegmenter(opts.tokenRegex.value.r)
    val papers = new ArrayBuffer[PaperEntity]
    if(opts.dblpLocation.value.toLowerCase!="none"){
      StopWatch.start("Load DBLP")
      papers ++= DBLPLoader.loadDBLPData(opts.dblpLocation.value)
      val time = StopWatch.stop/1000L
      println("Loading DBLP took: "+time+" seconds.")
    }
    if(opts.bibDirectory.value.toLowerCase!="none"){
      StopWatch.start("Load BibTex")
      papers ++= BibReader.loadBibTexDirMultiThreaded(new File(opts.bibDirectory.value))
      val time = StopWatch.stop/1000L
      println("Loading BibTeX took: "+time+" seconds.")
    }
    println("Number of papers: "+papers.size)
    if(opts.readModel.wasInvoked){
      StopWatch.start("Reading model")
      val lda = this.loadLDAModelFromAlphaAndPhi(new File(opts.readModel.value))
      StopWatch.stop
      StopWatch.start("Inferring topics")
      this.inferTopicsForPapers(papers,lda,mySegmenter)
      StopWatch.stop
    }

    if(opts.writeModel.wasInvoked){
      StopWatch.start("Training LDA...")
      val lda = new LDA(WordSeqDomain, opts.numTopics.value, opts.alpha.value, opts.beta.value, opts.optimizeBurnIn.value)(model)
      for(paper<-papers){
        val doc = Document.fromString(WordSeqDomain,paper.id,DEFAULT_DOCUMENT_GENERATOR(paper),segmenter=mySegmenter)
        if (doc.length >= 3) lda.addDocument(doc)
        if (lda.documents.size % 1000 == 0) { print(" "+lda.documents.size); Console.flush() }; if (lda.documents.size % 10000 == 0) println()
      }
      if (opts.numIterations.value > 0) {
        val startTime = System.currentTimeMillis
        if (opts.numThreads.value > 1)
          lda.inferTopicsMultithreaded(opts.numThreads.value, opts.numIterations.value, diagnosticInterval = 10, diagnosticShowPhrases = false)
        else
          lda.inferTopics(opts.numIterations.value, fitAlphaInterval = Int.MaxValue, diagnosticInterval = 10, diagnosticShowPhrases = false)
        println("Finished in " + ((System.currentTimeMillis - startTime) / 1000.0) + " seconds")
      }
      StopWatch.stop
      println(lda.topicsSummary(20))
      StopWatch.start("Saving LDA...")
      this.saveAlphaAndPhi(lda,new File(opts.writeModel.value))
      StopWatch.stop
    }
    StopWatch.printTimes
  }

  def DEFAULT_DOCUMENT_GENERATOR(paper:PaperEntity):String = {
    paper.title.value+" "+FeatureUtils.venueBag(paper.venueName.value).mkString(" ")
  }
/*
  def createDocumentsForLDA(papers:Iterable[PaperEntity], outputDir:File, documentGenerator:PaperEntity=>String=DEFAULT_DOCUMENT_GENERATOR(_)):Unit ={
    var count = 0
    var startTime = System.currentTimeMillis
    var batchTime = startTime
    for(paper<-papers){
      count += 1
      if(count % 1000 == 0)print(".")
      if(count % (1000*25) == 0){
        val elapsed = (System.currentTimeMillis - startTime)/1000L
        val elapsedBatch = (System.currentTimeMillis - batchTime)/1000L
        val docsPerSec = if(elapsed==0L) -1 else count/elapsed
        val docsPerSecBatch = if(elapsedBatch==0L) -1 else count/elapsedBatch
        println("#Docs: "+count+". Elapsed: "+elapsed+"sec. Batch elapsed: "+elapsedBatch+" sec. DPS:"+docsPerSec+". Batch DBP: "+docsPerSecBatch+".")
        batchTime = System.currentTimeMillis
      }
      createDocumentForLDA(paper,new File(outputDir.getAbsolutePath+"/"+paper.id+".paper"),documentGenerator)
    }
  }
  def createDocumentForLDA(paper:PaperEntity,file:File,documentGenerator:PaperEntity=>String=DEFAULT_DOCUMENT_GENERATOR(_)):Unit ={
    val writer = new PrintWriter(file)
    writer.println(documentGenerator(paper))
    writer.flush
    writer.close
  }
  */
}




class SecondOrderSummaryStatistics{
  protected val weights:HashMap[String,Double] = new HashMap[String,Double]
  def key(a:String,b:String):String = if(a.compareTo(b)<=0)a+"-"+b else b+"-"+a
  def apply(a:String,b:String):Double = weights(key(a,b))
  def add(a:String,b:String,weight:Double=1.0):Unit = {
    val k = key(a,b)
    if(weights.contains(k))weights(k) += weight else weights(k) = weight
  }
  def remove(a:String,b:String,weight:Double=1.0):Unit ={
    val k = key(a,b)
    if(weights.getOrElse(k,0.0)==weight)weights.remove(k) else weights(k) = weights(k) - weight
  }
  def add(words:Seq[String]):Unit = for(i<-0 until words.size)for(j<-i+1 until words.size)add(words(i),words(j))
  def add(wordsa:Iterable[String],wordsb:Iterable[String]):Unit = for(a<-wordsa)for(b<-wordsb)add(a,b)
  def add(wordsa:Set[(String,Double)],wordsb:Set[(String,Double)]):Unit = for(a<-wordsa)for(b<-wordsb)add(a._1,b._1,a._2*b._2)
  def remove(wordsa:Iterable[String],wordsb:Iterable[String]):Unit = for(a<-wordsa)for(b<-wordsb)remove(a,b)
  def remove(wordsa:Set[(String,Double)],wordsb:Set[(String,Double)]):Unit = for(a<-wordsa)for(b<-wordsb)remove(a._1,b._1,a._2*b._2)
  def adjustFromDiff(difflist:DiffList):Unit ={
    for(diff <- difflist){
      diff match{
        case d:BagOfVenues#BagOfWordsVariableAddStringDiff => add(Set(d.added->d.w),collapse(d.variable.value.iterator.toSeq))
        case d:BagOfVenues#BagOfWordsVariableRemoveStringDiff => remove(Set(d.removed->d.w),collapse(d.variable.value.iterator.toSeq))
        case d:BagOfVenues#BagOfWordsVariableAddBagDiff => add(collapse(d.added.iterator.toSeq),collapse(d.variable.value.iterator.toSeq))
        case d:BagOfVenues#BagOfWordsVariableRemoveBagDiff => remove(collapse(d.removed.iterator.toSeq),collapse(d.variable.value.iterator.toSeq))
      }
    }
  }
  protected def collapse(it:Iterable[(String,Double)]):Set[(String,Double)] = {
    val result = new HashMap[String,Double]
    for(item <- it)result(item._1) = result.getOrElse(item._1,0.0) + item._2
    result.toSet
  }
}

object StopWatch{
  val timeRecords = new HashMap[String,Long]
  protected val timers = new HashMap[String,Long]
  protected var lastName:String = null
  def time(name:String,code:Unit=>Unit) = timeAndReturn[Unit](name,code)
  def timeAndReturn[O](name:String,code:Unit=>O):(O,Long) ={
    var t = System.currentTimeMillis
    val result = code.apply()
    t = System.currentTimeMillis - t
    timeRecords(name) = timeRecords.getOrElse(name,0L) + t
    (result,t)
  }
  def start(name:String):Unit = {
    timers(name) = System.currentTimeMillis
    lastName = name
  }
  def stop:Long = stop(null.asInstanceOf[String])
  def stop(s:String):Long = {
    val name:String = if(s==null)lastName else s
    if(!timers.contains(name))throw new Exception("Cannot stop the watch because start(\""+name+"\") has not been called.")
    timers.remove(s)
    val elapsed = System.currentTimeMillis() - timers(name)
    timeRecords(name) = timeRecords.getOrElse(name,0L)+elapsed
    elapsed
  }

  def printTimes:Unit = {
    println("Total time")
    for ((k,v) <- timeRecords){
      println("  "+k+":"+(v/1000L)+" sec.")
    }
  }
}


/*
trait SummaryStatistic{
  def key:String
  def weight:Double
  def kind:String
  override def hashCode = key.hashCode
}
class FirstOrderSummary(val word:String,val weight:Double,val kind:String="") extends SummaryStatistic{
  val key=word
}
class SecondOrderSummary(val word:String,occursWith:String,weight:Double,val kind:String) extends SummaryStatistic{
  val key = if(a.compareTo(b))word+"-"+occursWith else occursWith+"-"+word
}

object SummaryStatistics{

}

class SummaryStatistics[S<:SummaryStatistic](val weights:HashMap[S,Double] = new HashMap[S,Double]){
  def updateDelta(add:Iterable[S],remove:Iterable[S]){

    for(a<-add){
      if(weights.contains(a))weights(a).weight += a.weight else
    }
  }
  protected def add(summaries:Iterable[S]) = {
    for(s<-summaries)weight
  }
}
*/

/*
class SummaryCubbie extends Cubbie
class SecondOrderSummaryCubbie(a:String,b:String,weight:Double,kind:String="") extends SummaryCubbie{
  val _id = StringSlot("_id")
  val _occursWith = StringSlot("ow")
  val _weight = DoubleSlot("wt")
  val _kind = StringSlot("tp")
  if(a.compareTo(b)<=0){
    _id := a
    _occursWith := b
  } else{
    _id := b
    _occursWith := a
  }
  _weight := w
  _kind := kind
}
trait SummaryCollection[S<:SummaryCubbie]{
  protected def newSummaryCubbie:S
  protected def cubbieCollection:MutableCubbieCollection[S]
}
trait SecondOrderSummaryCollection{
  var pairs2counts:HashMap[String,Double]=null
  def apply(a:String,b:String):Double = {
    val key:String = if(a.compareTo(b)>0)a+"-"+b else b+"-"+a
    pairs2counts.getOrElse(key,0.0)
  }
  def loadCounts(strings:Iterable[String]):Unit = pairs2counts = load(strings)
  protected def load(strings:Iterable[String]):HashMap[String,Double]
}
class MongoSummaryCollection[S<:SummaryCubbie] extends SummaryCollection[S]{
  val cubbieCollection = new MongoCubbieCollection(mongoDB.getCollection(collectionName),() => newSummaryCubbie,(stat:E) => Seq(Seq(stat._occursWith),Seq(stat._kind))) with LazyCubbieConverter[E]
}
*/

  //extends MongoCubbieCollection(mongoDB.getCollection(collectionName),() => new CoocCubbie,(stat:CoocCubbie) => Seq(Seq(stat._occursWith),Seq(stat._kind))) with LazyCubbieConverter[CoocCubbie]




/*
object GeneralUtils{
  def updateIDF(papers:Iterable[PaperEntity],tokenExtractor:PaperEntity=>Seq[String],val idfTYPE:String):Unit ={
    for(paper <- papers){
      
    }
  }
}*/