package cc.factorie.app.bib
import cc.factorie.app.bib.parser.Dom
import cc.factorie.app.bib.parser.Dom.Entry
import collection.mutable.{ArrayBuffer,HashMap}
import javax.xml.parsers.{DocumentBuilder, DocumentBuilderFactory}
import org.w3c.dom.{Node, NodeList, Document}
import java.io.{FileInputStream, InputStreamReader, BufferedReader, File}

object RexaCitationLoader{
  var numPapers = 0
  var numReferences = 0
  def loadFromList(file:File):Seq[PaperEntity] ={
    println("About to load REXA data from " + file)
    val result = new ArrayBuffer[PaperEntity]
    val reader = new BufferedReader(new InputStreamReader(new FileInputStream(file)))
    var line = reader.readLine
    while(line!=null){
      result ++= loadFile(new File(line))
      line=reader.readLine
    }
    println("Finished loading "+numPapers + " papers and "+numReferences + " references. Total: "+(numPapers+numReferences)+".")
    result
  }
  def loadFile(file:File):Seq[PaperEntity] ={
    val docFactory:DocumentBuilderFactory = DocumentBuilderFactory.newInstance
    val docBuilder:DocumentBuilder = docFactory.newDocumentBuilder
    if(!file.exists()) error("Couldn't find file " + file + ", aborting load.")
    val result = new ArrayBuffer[PaperEntity]
    val doc:Document = docBuilder.parse(file)
    val content = doc.getElementsByTagName("content")
    if(content.getLength==1){
      val contentChildren = content.item(0).getChildNodes
      for(i<-0 until contentChildren.getLength){
        val contentType = contentChildren.item(i).getNodeName
        val contentFieldNodes = contentChildren.item(i).getChildNodes
        if(contentType == "headers"){
          val paper = processPaper(contentChildren.item(i))
          result += paper
          numPapers += 1
        }
        if(contentType == "biblio"){
          for(j<-0 until contentFieldNodes.getLength){
            val biblioNode = contentFieldNodes.item(j)
            result += processPaper(biblioNode)
            numReferences += 1
          }
        }
      }
    }
    result
  }
  protected def processPaper(paperNode:Node):PaperEntity ={
    val paperAttributeNodes = paperNode.getChildNodes
    val paper = new PaperEntity("",true)
    for(j <- 0 until paperAttributeNodes.getLength){
      val name = paperAttributeNodes.item(j).getNodeName
      val text = paperAttributeNodes.item(j).getTextContent
      name match{
        case "title" => {paper.title := text.replaceAll("[\r\n\t ]+"," ").replaceAll("[^A-Za-z0-9\\-\\.,\\(\\) ]","")}
        case "date" => {} //more than a year, e.g., September 21, 2006
        case "authors" => {
          paperAttributeNodes.item(j).getChildNodes
          val authorNodes = paperAttributeNodes.item(j).getChildNodes
          for(k <- 0 until authorNodes.getLength){
            val authorMention = getAuthor(authorNodes.item(k))
            authorMention.paper = paper
            paper.authors += authorMention
          }
        }
        case "note" => {}
        case "address" =>{}
        case "institution" => {}
        case "abstract" => {}
        case _ => {}
      }
    }
    paper
  }
  protected def getAuthor(authorNode:Node):AuthorEntity ={
    var (f,m,l) = ("","","")
    val name = authorNode.getNodeName
    val text = FeatureUtils.normalizeName(authorNode.getTextContent)
    name match{
      case "author-first" => f=text
      case "author-middle" => m=text
      case "author-last" => l=text
      case _ => {}
    }
    val result = new AuthorEntity(f,m,l,true)
    result
  }
}

object AclAnthologyReader{
  def loadAnnFile(file:File):Seq[PaperEntity] ={
    val fileText = scala.io.Source.fromFile(file).toArray.mkString
    val records = """id.+?[\r\n]author.+?[\r\n]title.+?[\r\n]venue.+?[\r\n]year.+?}""".r.findAllIn(fileText).toSeq
    records.map(makePaperEntity(_))
  }
  def makePaperEntity(record:String):PaperEntity ={
    val paperEntity = new PaperEntity("",true)

    val Matcher = """id\s=\s\{.+?\}\s*[\r\n]"""+
       """author\s=\s\{(.*?)\}\s*[\r\n]"""+
       """title\s=\s\{(.*?)\}\s*[\r\n]"""+
       """venue\s=\s\{(.*?)\}\s*[\r\n]"""+
       """year\s=\s\{(\d{4}?)\}""" r

    val Matcher(authorList,title,venue,year) = record

    getAuthors(authorList).foreach(paperEntity.authors+=_)
    if(!title.equals("")) paperEntity.title.set(title)(null)
    paperEntity.attr[VenueName]:=venue
    paperEntity.attr[Year] := year.toInt

    paperEntity

    //    val AuthorMatcher = """author\s=\s\{(.+?)\}""".r
    //    val TitleMatcher = """title\s=\s\{(.+?)\}""".r
    //    val TitleMatcher(title) = record
    //    val VenueMatcher = """venue\s=\s\{(.+?)\}""".r
    //    val VenueMatcher(venue) = record(3)
    //    val YearMatcher = """year\s=\s\{(\d{4})\}""".r
    //    val YearMatcher(year) = record(4)
  }
  def getAuthors(authorList:String):Seq[AuthorEntity]={
    val MatcherType1 = """\s*(.+?)\s*(Jr.)\s*,\s*(.+?)\s+(.+?)\s*""".r
    val MatcherType2 = """\s*(.+?)\s*(Jr.)\s*,\s*(.+?)\s*""".r
    val MatcherType3 = """\s*(.+?)\s*,\s*(.+?)\s+(.+?)\s*""".r
    val MatcherType4 = """\s*(.+?)\s*,\s*(.+?)\s*""".r
    val MatcherType5 = """\s*,?\s*(.+?)\s*""".r
    println("authors list: "+authorList)
    authorList.split(";").flatMap(xs => {
      println("  xs: "+xs)
      xs match {
        case MatcherType1(last,jr,first,middle)=> {
          //println("  Case 1 (l,j,f,m): f="+first+" m="+middle+" l="+last+" jr="+jr)
          Seq(new AuthorEntity(first,middle,last,true))
        }
        case MatcherType2(last,jr,first)=> {
          //println("  Case 2 (l,j,f): f="+first+" l="+last+" jr="+jr)
          Seq(new AuthorEntity(first,"",last,true))
        }
        case MatcherType3(last,first,middle)=> {
          //println("  Case 3: (l,f,m) f="+first+" m="+middle+" l="+last)
          Seq(new AuthorEntity(first,middle,last,true))
        }
        case MatcherType4(last,first)=> {
          //println("  Case 4: (l,f) f="+first+" l="+last)
          Seq(new AuthorEntity(first,"",last,true))
        }
        case MatcherType5(last)=> {
          //println("  Case 5 (l): l="+last)
          Seq(new AuthorEntity("","",last,true))
        }
        case _ => {
          println("  NIL")
          Nil
        }
      }
    })
  }
}


object BibReader{
  var skipped=0
  var numParsed=0
  var readErrors = new HashMap[String,Int]
  def loadBibTexDirMultiThreaded(bibDir:File):Seq[PaperEntity] ={
    var count = 0
    //var result = new ArrayBuffer[PaperEntity]
    if(!bibDir.isDirectory)return loadBibTeXFile(bibDir)
    val files = bibDir.listFiles.filter(_.isFile)
    val result = new Array[Seq[PaperEntity]](files.length)
    println("Mult-threaded load, about to load "+files.length + " .bib files")
    val startTime = System.currentTimeMillis
    for(file <- files.par){
      val bibs = loadBibTeXFile(file)
      synchronized{
        result(count)=bibs
        count += 1
        if(count % 10 == 0)print(".")
        //if(count % 200 == 0 || count==files.size)println(" Processed "+ count + " documents, but skipped "+BibReader.skipped + ". Time: "+((System.currentTimeMillis-startTime)/1000L))
        if(count % 200 == 0 || count==files.size)println(" Processed "+ count + " documents, but skipped "+BibReader.skipped + ". Papers parsed:"+result.size+". Time: "+((System.currentTimeMillis-startTime)/1000L))
      }
    }
    var errCount=0
    println("Read errors:")
    for((k,v) <- readErrors){
      println(v+":"+k)
      errCount += v
    }
    println("Total read errors caught: "+errCount)
    result.filter(_ != null).flatMap(_.toSeq).toSeq
    //result
  }
  def loadBibTexDir(bibDir:File):Seq[PaperEntity] = {
    var count = 0
    var result = new ArrayBuffer[PaperEntity]
    val files = bibDir.listFiles.filter(_.isFile)
    val startTime = System.currentTimeMillis
    for(file <- files){
      result ++= loadBibTeXFile(file)
      count += 1
      if(count % 10 == 0)print(".")
      if(count % 200 == 0 || count==files.size)println(" Processed "+ count + " documents, but skipped "+BibReader.skipped + ". Time: "+((System.currentTimeMillis-startTime)/1000L))
    }
    result
  }
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
          //readErrors(error) = readErrors.getOrElse(error,0) + 1
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
        readErrors(e.getMessage) = readErrors.getOrElse(e.getMessage,0) + 1
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
      val paperEntity = new PaperEntity("",true)
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
            if(xv.length>4)xv=xv.substring(4)
            if(xv.length==0 || xv.length>4)xv="-1"
            if(xv == "-1")println("BibTeX Load error: unrecognizable year format: "+value)
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
}

object RexaLabeledLoader{
  var noAuthorInFocusCount = 0
  def load(rexaDir:File):Seq[PaperEntity]={
    var result = new ArrayBuffer[PaperEntity]
    for(ambiguousNameDir <- rexaDir.listFiles){
      if(ambiguousNameDir.listFiles!=null){
        for(entityDir <- ambiguousNameDir.listFiles){
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
    val paper = new PaperEntity("",true)
    var authorInFocus = new Array[String](3)
    val loadedFile = scala.io.Source.fromFile(file)
    for(line <- loadedFile.getLines.toSeq.reverse){ //for some reason scala loads files in reverse order...
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
//            for(keyword <- value.toLowerCase.split(" +"))paper.bagOfKeywords += FeatureUtils.filterFieldNameForMongo(keyword)
          }
          case "altTitle" => {}
          case "editor" => {}
          case "body" => {}
          case "abstract" => {}
          case "keywords" => for(keyword <- value.split(", "))paper.bagOfKeywords += FeatureUtils.filterFieldNameForMongo(keyword.toLowerCase)
          case "keyword" => for(keyword <- value.split(", "))paper.bagOfKeywords += FeatureUtils.filterFieldNameForMongo(keyword.toLowerCase)
          case "journal" => {
            val jsplit = value.split("year:")
            paper.venueName.set(jsplit(0))(null)
            if(jsplit.length==2){
              //println("Found year field... about to parse")
              var xv = jsplit(1).replaceAll("[^0-9]","")
              if(xv.length>4)xv=xv.substring(4)
              if(xv.length==0 || xv.length>4)xv="-1"
              if(xv == "-1")println("Rexa Load error: unrecognizable year format: "+value)
              paper.attr[Year] := xv.toInt
            }
          }
          case "institution" => for(keyword <- value.toLowerCase.split(", "))paper.bagOfKeywords += FeatureUtils.filterFieldNameForMongo(keyword)
          case "email" => for(email <- value.split(", "))paper.bagOfKeywords += email.toLowerCase.replaceAll("\\.","DOT")
          case "year" => {
            //println("Found year field... about to parse")
            var xv = value.replaceAll("[^0-9]","")
            if(xv.length>4)xv=xv.substring(4)
            if(xv.length==0 || xv.length>4)xv="-1"
            if(xv == "-1")println("Rexa Load error: unrecognizable year format: "+value)
            paper.attr[Year] := xv.toInt

            //if(value.matches("[0-9]+"))paper.attr[Year] := value.toInt else println("Rexa load error: "+value + " is not in date format.")
          }//paper.year.set(value)(null)
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
    loadedFile.close
    paper
  }
  def extractFlatSGML(string:String,tag:String):String ={
    var start = string.indexOf("<"+tag+">")
    if(start == -1)return ""
    start += ("<"+tag+">").length
    val end = string.indexOf("</"+tag+">")
    string.substring(start,end)
  }
}


/*
object AronDataLoader{
  var skipped:Int = 0
  var numParsed:Int = 0
  def load(dir:File):Seq[PaperEntity] ={
    var files = directory.listFiles
    for(file<-files){
      //db.storeCitations(loadFromXMLFile(file.toString()))

    }
  }
  def loadFromXMLFile(file:String):Seq[PaperEntity]={
    val result = new ArrayBuffer[PaperEntity]
    val xmlDocParser: RexaDataLoader = new RexaDataLoader();
    try{
      var references: ArrayBuffer[ReferenceData] = xmlDocParser.loadData(file);
      for(i<-0 until references.size){
        result += xml2mention(references.apply(i))
      }
      numParsed += 1
    }
    catch{
      case e:Exception =>{
        skipped += 1
        println("\n=================================")
        e.printStackTrace
        println("=================================")
        println("=================================")
        println("ill-formated xml entry in file: " + file)
        println("  total skipped: " + skipped)
      }
    }
    result;
  }
  def xml2mention(entry:ReferenceData):PaperEntity ={
    //val entryType = entry.getEntryType.toLowerCase     //?
    //val key = entry.getEntryKey       //?
    val paperMention = new PaperEntity
    paperMention.citationString=entry.toString   //?
    var entryAuthors: Array[AuthorData] = entry.authors;
    val authors = new ArrayBuffer[AuthorEntity]
    for(j<-0 until entry.authors.length){
      val person = entry.authors.apply(j);
      var last = person.lastName;if(last==null)last=""
      val authorMention = new AuthorEntity(person.firstName,person.middleName,last)
      authorMention.paperMention = paperMention
      authorMention.authorIndex = j
      if(entry.authors.apply(j).clusterId != -1)
        authorMention.setClusterID(entry.authors.apply(j).clusterId);
      authors += authorMention;
      println("added author " + person.firstName + " " + last);
      //if(person.getLineage!=null)
      //  personObj.put(PersonRecord.LINEAGE,person.getLineage)
    }
    paperMention.authorMentions ++= authors
    paperMention.title=entry.strTitle;
    if(entry.titleClusterId != -1)
      paperMention.setClusterID(entry.titleClusterId);
/*
    val venueMention = new VenueMention
    venueMention.name = entry.strVenue
    if(entry.venueClusterId != -1)
      paperMention.setClusterID(entry.venueClusterId);
    paperMention.venueMention = venueMention
    */
    if(paperMention.title==null)paperMention.title=""
    paperMention.createAllSingletonEntities
    if(entry.titleClusterId != -1)
      paperMention.entity.setClusterID(entry.titleClusterId)
    if(entry.venueClusterId != -1)
      paperMention.entity.venue.setClusterID(entry.venueClusterId)
    for(i <- 0 until paperMention.entity.authors.size){
      println("adding author mention")
      paperMention.entity.authors(i).setClusterID(entry.authors(i).clusterId)
    }
    //paperMention.createSingletonPaperEntity()
    //paperMention.createSingletonAuthorEntities()
    paperMention
  }
}
*/


object DBLPLoader{
  def loadDBLPData(location:String) : Seq[PaperEntity] ={
    println("Loading dblp data from: "+location)
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
          case "phdthesis" => {addFieldsForDBLP(node,paperMention)}
          case "mastersthesis" => {addFieldsForDBLP(node,paperMention)}
          case "techreport" => {addFieldsForDBLP(node,paperMention)}
          case "book" => {addFieldsForDBLP(node,paperMention)}
          case "inbook" => {addFieldsForDBLP(node,paperMention)}
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
      //println("node:"+node)
      val text = node.getTextContent
      //println("    Node name: "+node.getNodeName)
      //println("    Node text: "+text)
      node.getNodeName match{
        //all fields
        case "author" => {
          //println("found an author")
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
          //println("  found a node:"+paperMention.authors.size)
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
