package cc.factorie.app.bib
import collection.mutable.{HashMap, ArrayBuffer}
import cc.factorie.app.nlp.coref.Entity


object FeatureUtils{
  //venue projections
  def isInitial(s:String) = s.matches("[a-z]( [a-z])?")
  val venueP0 = "\\(.+\\)";
  val venueP1 = "(in )?[Pp]roceedings( of)?( the)?";
  val venueP2 = "[0-9\\-]+(th|st|rd)?"
  val venueP3 = "([Ee]leventh|[Tt]welfth|[Tt]hirteenth|[Ff]ourteenth|[Ff]ifteenth|[Ss]ixteenth|[Ss]eventeenth|[Ee]ighteenth|[Nn]ineteenth|[Tt]wentieth|[Tt]hirtieth|[Ff]ourtieth)?([Tt]wenty|[Tt]hirty|[Ff]orty)?[- ]?([Ff]irst|[Ss]econd|[Tt]hird|[Ff]ourth|[Ff]ifth|[Ss]ixth|[Ss]eventh|[Ee]ighth|[Nn]ineth)?"
  val venueP4 = "(in the )?[Pp]roceedings of (the )?[a-z0-9]+ "
  val venueP5 = "(([Aa]dvances( ?in ?)?|[Pp]roceedings|[Pp]roc\\.? )) ?"
  val venuePost = " ?([Ee]ndowment|[Ee]ndow|Proceedings|Meeting)\\.?"
  val venForAuthStops = "(proceedings|proc|endowment|endow|conference|in|the|of|[a-z]+eenth|[a-z]+tieth|first|second|third|fourth|fifth|sixth|seventh|eighth|nineth|tenth|eleventh|twelfth)"
  val tokenFilterString = "[^A-Za-z0-9]"
  def normalizeName(name:String) = name.replaceAll("[^A-Za-z ]","").replaceAll("[ ]+"," ")
  def filterFieldNameForMongo(s:String) = s.replaceAll("[$\\.]","")
  def venueBag(s:String):Seq[String] = {val toks = new ArrayBuffer[String];toks++=tokenizeVenuesForAuthors(s);toks ++= getVenueAcronyms(s).map(_._1);toks.map(_.toLowerCase).toSeq}
  def tokenizeVenuesForAuthors(s:String):Seq[String] ={
    var filtered = s.toLowerCase.replaceAll("[^a-z ]","")
    filtered = filtered.replaceAll(venForAuthStops,"")
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
}

object EntityUtils{
  def makeSingletons(entities:Seq[AuthorEntity]):Seq[AuthorEntity] ={
    for(e <- entities)
      e.setParentEntity(null)(null)
    entities.filter(_.isObserved).toSeq
  }
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
  def printAuthors(entities:Seq[AuthorEntity],includeSingletons:Boolean=true):Unit ={
    printEntities(entities,includeSingletons,
      (e:Entity)=>{
        var result:String = e.attr[FullName].toString+" (first:"+bagToString(e.attr[BagOfFirstNames].value)+" middle:"+bagToString(e.attr[BagOfMiddleNames].value)+")"
        if(e.asInstanceOf[AuthorEntity].groundTruth!=None)result="truth:"+e.asInstanceOf[AuthorEntity].groundTruth.get+"  "+result
        result},
      (e:Entity)=>{"{"+bagToString(e.attr[BagOfCoAuthors].value)+"}"})
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
