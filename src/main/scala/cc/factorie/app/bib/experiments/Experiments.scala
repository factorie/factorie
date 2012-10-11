package cc.factorie.app.bib.experiments
import cc.factorie.app.bib._
import java.util.Date
import java.io._
import collection.mutable.{ArrayBuffer,HashSet}
import java.text.DateFormat
import cc.factorie.util.{CmdOption, DefaultCmdOptions}
import com.mongodb.Mongo
import cc.factorie._
import app.nlp.coref.{EntropyBagOfWordsPriorWithStatistics, StructuralPriorsTemplate, ChildParentCosineDistance, HierEntity}

class AuthorSamplerWriter(model:Model[Variable], val labeledData:Seq[AuthorEntity], val evidenceBatches:Seq[Seq[AuthorEntity]],var initialSteps:Int=0,var snapshotInterval:Int=1000, var addDataInterval:Int=10000) extends AuthorSampler(model){
  protected var pwOption:Option[PrintWriter]=None
  var mentionCount = labeledData.size
  def processExperiment(pw:PrintWriter,steps:Int):Unit ={
    pwOption=Some(pw)
    pw.println("time #samples #accepted p r f1 #mentions")
    process(initialSteps)
    for(evidenceBatch <- evidenceBatches){
      this.process(steps)
    }
  }
  override def proposalHook(proposal:Proposal) ={
    super.proposalHook(proposal)
    if(proposalCount % snapshotInterval == 0){
      val scores = Evaluator.pairF1LabeledOnly(labeledData)
      snapshot(totalTime,proposalCount,numAccepted,scores)
    }
  }
  def snapshot(time:Long,numSamples:Int,numAccepted:Int,scores:Iterable[Double]):Unit ={
    for(pw <- pwOption){
      val line = (time+" "+numSamples+" "+numAccepted+" "+scores.foreach(_ + " ")).trim+" "+mentionCount
      pw.println(line)
      pw.flush()
    }
  }
}

object EpiDBExperimentOptions extends MongoOptions with DataOptions with InferenceOptions with AuthorModelOptions{
  val outputDir = new CmdOption("outputDir","/Users/mwick/data/rexa2/experiments/","FILE","Root output directory containing intermediate results and final results")
  val scratchDir = new CmdOption("scratchDir","scratch/","FILE","Directory for intermediate results of experiment")
  val resultsDir = new CmdOption("resultsDir","results/","FILE","Directory for final results of experiment")
  val metaDir = new CmdOption("metaDir","meta/","FILE","Directory for meta information about results (parameters, settings, configurations used etc.)")
  val experimentName = new CmdOption("name","NONE","FILE","Name of experiment to run")
  def main(args:Array[String]):Unit ={
    this.parse(args)
    writeOptions(new File(this.metaDir.value+experimentName.value))
    println("Dropping database.")
    if(dropDB.value){
      val mongoConn = new Mongo(server.value,port.value.toInt)
      val mongoDB = mongoConn.getDB(database.value)
      mongoDB.getCollection("authors").drop
      mongoDB.getCollection("papers").drop
      mongoConn.close
    }
    val authorCorefModel = new AuthorCorefModel
    authorCorefModel += new ChildParentCosineDistance[BagOfTopics](4.0,-0.25)
    authorCorefModel += new ChildParentCosineDistance[BagOfCoAuthors](4.0,-0.25)
    authorCorefModel += new ChildParentCosineDistance[BagOfVenues](4.0,-0.25)
    authorCorefModel += new StructuralPriorsTemplate(2.0,0.25)
    authorCorefModel += new EntropyBagOfWordsPriorWithStatistics[BagOfTopics](0.75)
    authorCorefModel += new EntropyBagOfWordsPriorWithStatistics[BagOfCoAuthors](0.25)
    authorCorefModel += new EntropyBagOfWordsPriorWithStatistics[BagOfVenues](0.25)

    val epiDB = new EpistemologicalDB(authorCorefModel,server.value,port.value.toInt,database.value)
    println("About to add data.")
    var papers = new ArrayBuffer[PaperEntity]
    if(bibDirectory.value.toLowerCase != "none"){
      println("Adding mentions from BibTeX directory: "+bibDirectory.value.toLowerCase)
      papers ++= BibReader.loadBibTexDirMultiThreaded(new File(bibDirectory.value))
    }
    if(rexaData.value.toLowerCase != "none"){
      println("Loading labeled data from: " + rexaData.value)
      papers ++= RexaLabeledLoader.load(new File(rexaData.value))
    }
    if(dblpLocation.value.toLowerCase != "none"){
      println("Loading dblp data from: "+dblpLocation.value)
      papers ++= DBLPLoader.loadDBLPData(dblpLocation.value)
    }
    if(this.filterPapersOnLabeledCanopies.value){
      println("About to filter papers so only those in a labeled author's canopy are inserted into DB.")
      val labeledAuthors = epiDB.authorColl.loadLabeled
      val canopySet = new HashSet[String]
      canopySet ++= labeledAuthors.flatMap(_.canopyAttributes).map(_.canopyName)
      var i =0
      val filteredPapers = new ArrayBuffer[PaperEntity]
      while(i<papers.size){ //while loop much much faster than a map for DBLP size data
        val p = papers(i)
        val canopies = p.authors.flatMap(_.canopyAttributes).map(_.canopyName).toSet
        if(canopySet.intersect(canopies).size>0)filteredPapers += p
      }
      papers = filteredPapers
    }
    println("About to add "+papers.size+"papers.")
    epiDB.add(papers)
    println("Finished adding papers.")
  }


  def randomSubset[T](seq:Seq[T],n:Int):Seq[T] = random.shuffle(seq).take(n)
  def runInference(authors:Seq[AuthorEntity],inferencer:AuthorSamplerWriter,pw:PrintWriter,numSteps:Int) ={
    inferencer.setEntities(authors)
    inferencer.processExperiment(pw,numSteps)
  }
}

trait ExperimentOptions extends DefaultCmdOptions{
  def writeOptions(file:File):Unit = {
    //if(!file.exists)file.mkDirs
    val pw = new PrintWriter(file)
    writeOptions(pw)
    pw.close
  }
  def writeOptions(pw:PrintWriter):Unit ={
    //pw.println("Experiment Parameters: "+DateFormat.getDateInstance(DateFormat.SHORT).format(now))
    this.foreach(o => pw.println("--"+o.name+"="+o.value))
    pw.flush()
  }
  def readOptions(file:File):Unit ={
    import scala.io.Source
    val contents = Source.fromFile(file).mkString
    val args = contents.split("\\s+")
    parse(args)
  }
}


trait MongoOptions extends ExperimentOptions{
  val server = new CmdOption("server","localhost","FILE","Location of Mongo server.")
  val port = new CmdOption("port","27017","FILE","Port of Mongo server.")
  val database = new CmdOption("database","rexa2-cubbies","FILE","Name of mongo database.")
}
  //db creation options
trait DataOptions extends ExperimentOptions{
  val bibDirectory = new CmdOption("bibDir","/Users/mwick/data/thesis/all3/","FILE","Pointer to a directory containing .bib files.")
  val rexaData = new CmdOption("rexaData","/Users/mwick/data/rexa/rexaAll/","FILE","Location of the labeled rexa2 directory.")
  val dblpLocation = new CmdOption("dblpFile","none","FILE","Location of DBLP xml file.")
  val aronData = new CmdOption("aronData","/data/thesis/rexa1/rexa_coref_datav0.5/","FILE","Location of Aron's labeled data")
  val ldaModel = new CmdOption("ldaModel","lda-model.txt","FILE","Location of lda model")
  val filterPapersOnLabeledCanopies = new CmdOption("filter-for-labeled",false,"FILE","True: only insert papers into the DB that contain an author in a canopy of another labeled mention")
  val saveDB = new CmdOption("saveDB",true,"BOOLEAN","true: saves inference results, false: discard inference results")
  val dropDB = new CmdOption("dropDB",true,"BOOLEAN","true: saves inference results, false: discard inference results")

}
trait InferenceOptions extends ExperimentOptions{
  //inference options
  val numEpochs = new CmdOption("epochs","1","FILE","Number of inference round-trips to DB.")
  val batchSize = new CmdOption("batchSize","10000","FILE","Number of entities used to retrieve canopies from.")
  val stepMultiplierA = new CmdOption("a","0.0","FILE","Runs for n^2 steps (n=number of mentions to do inference on.)")
  val stepMultiplierB = new CmdOption("b","0.0","FILE","Runs for n steps (n=number of mentions to do inference on.)")
  val stepMultiplierC = new CmdOption("c","1000000.0","FILE","Runs for c steps (c=constant)")
  val evaluateOnly = new CmdOption("evaluate","false","FILE","Loads labeled data, evaluates the accuracy of coreference, and exits.")
  //model
}
trait AuthorModelOptions extends ExperimentOptions{
  /*
    authorCorefModel += new ChildParentCosineDistance[BagOfTopics](4.0,-0.25)
    authorCorefModel += new ChildParentCosineDistance[BagOfCoAuthors](4.0,-0.125)
    authorCorefModel += new ChildParentCosineDistance[BagOfVenues](4.0,-0.125)
    authorCorefModel += new ChildParentCosineDistance[BagOfKeywords](2.0,-0.125)
    authorCorefModel += new StructuralPriorsTemplate(2.0,0.25)
    authorCorefModel += new EntropyBagOfWordsPriorWithStatistics[BagOfTopics](0.75)
    authorCorefModel += new EntropyBagOfWordsPriorWithStatistics[BagOfCoAuthors](0.125)
    authorCorefModel += new EntropyBagOfWordsPriorWithStatistics[BagOfVenues](0.125)
    authorCorefModel += new EntropyBagOfWordsPriorWithStatistics[BagOfKeywords](0.25)

   */
  //co-authors
  val bagCoAuthorWeight = new CmdOption("model-author-bag-coauthors-weight", 4.0, "N", "Penalty for bag-of-co-authors cosine distance template (author coreference model).")
  val bagCoAuthorShift = new CmdOption("model-author-bag-coauthors-shift", -0.125, "N", "Shift for bag-of-co-authors cosine distance template  (author coreference model).")
  val bagCoAuthorEntropy = new CmdOption("model-author-bag-coauthors-entropy", 0.125, "N", "Penalty on bag-of-co-author entropy (author coreference model).")
  val bagCoAuthorPrior = new CmdOption("model-author-bag-coauthors-prior", 0.25, "N", "Bag of co-author prior penalty, formula is bag.size/bag.oneNorm*weight.")
  //venues
  val bagVenuesWeight = new CmdOption("model-author-bag-venues-weight", 4.0, "N", "Penalty for bag-of-venues cosine distance template (the author coreference model).")
  val bagVenuesShift = new CmdOption("model-author-bag-venues-shift", -0.125, "N", "Shift for bag-of-venues cosine distance template (author coreference model).")
  val bagVenuesEntropy = new CmdOption("model-author-bag-venues-entropy", 0.125, "N", "Penalty on bag-of-venue entropy (author coreference model).")
  val bagVenuesPrior = new CmdOption("model-author-bag-venues-prior", 0.25, "N", "Bag of co-author prior penalty, formula is bag.size/bag.oneNorm*weight.")
  //keywords
  val bagKeywordsWeight = new CmdOption("model-author-bag-keywords-weight", 2.0, "N", "Penalty for bag-of-keywords template  (the author coreference model).")
  val bagKeywordsShift = new CmdOption("model-author-bag-keywords-shift", -0.125, "N", "Bag-of-keywords shift for  (author coreference model).")
  val bagKeywordsEntropy = new CmdOption("model-author-bag-keywords-entropy", 0.25, "N", "Penalty on bag of keywrods entropy(author coreference model).")
  val bagKeywordsPrior = new CmdOption("model-author-bag-keywords-prior", 0.25, "N", "Bag of co-author prior penalty, formula is bag.size/bag.oneNorm*weight.")
  //topics
  val bagTopicsWeight = new CmdOption("model-author-bag-topics-weight", 4.0, "N", "Penalty for bag-of-topics cosine distance template (author coreference model).")
  val bagTopicsShift = new CmdOption("model-author-bag-topics-shift", -0.25, "N", "Shift for bag-of-topics cosine distance template (author coreference model).")
  val bagTopicsEntropy = new CmdOption("model-author-bag-topics-entropy", 0.75, "N", "Penalty on bag of topics entropy  (author coreference model).")
  val bagTopicsPrior = new CmdOption("model-author-bag-topics-prior", 0.25, "N", "Bag of co-author prior penalty, formula is bag.size/bag.oneNorm*weight.")
  //author names
//  val bagFirstInitialWeight = new CmdOption("model-author-bag-first-initial-weight", 4.0, "N", "Penalty for first initial mismatches.")
//  val bagFirstNameWeight = new CmdOption("model-author-bag-first-name-weight", 4.0, "N", "Penalty for first name mismatches")
//  val bagMiddleInitialWeight = new CmdOption("model-author-bag-middle-initial-weight", 4.0, "N", "Penalty for first initial mismatches.")
//  val bagMiddleNameWeight = new CmdOption("model-author-bag-middle-name-weight", 4.0, "N", "Penalty for first name mismatches")
  //structural priors
  val entityExistencePenalty = new CmdOption("model-author-entity-penalty", 2.0, "N", "Penalty for a top-level entity existing")
  val subEntityExistencePenalty = new CmdOption("model-author-subentity-penalty", 0.25, "N", "Penalty for a subentity existing")
}
/*
trait Experiment{
  val now = new Date
  def experimentName:String
  def dataOutDir:String
  def dataWorkingDir:String = dataOutDir+"/"+working+"/"
  def dataResultsDir:String = dataOutputDir+"/"+results+"/"
  def parametersLocation:String = dataResultsDir+"/"+experimentName+".parameters"
  def writeParameters(args:Array[String]):Unit = {
    val pw = new PrintWriter(new File(parameters))
    pw.println("Experiment Parameters: "+DateFormat.getDateInstance(DateFormat.SHORT).format(now))
    args.foreach(pw.println(_))
    pw.flush()
    pw.close()
  }
  def readParameters(params:String):Array[String] ={
    val reader = new BufferedReader(new InputStreamReader(new FileInputStream(params)))
    var line = reader.readLine()
    val result = new ArrayBuffer[String]
    while(line!=null){
      line = reader.readLine
      result += line
    }
    reader.close
    result.toArray
  }
}


*/

/*
object EpiDBExperiment{
  def subsetExperiments(numFolds:Int=10,randomSeed:Int,numTrials:Int):Unit ={
    
  }
  def incrementalEvidenceExperiments(initialBatchPortion:Double,numIncrementalBatches:Int,initialInferenceSteps:Int,incrementalInferenceSteps:Int,numTrials:Int):Unit ={
    
  }
}

trait EpiDBExperiment{
  var printWriter:PrintWriter = null
  var snapshotFrequency = 1000
  def proposalCount:Int
  def numAccepted:Int
  def totalTime:Long
  def labeledData:Seq[HierEntity]
  def process(n:Int)
  
  def runExperiment(experimentName:String,trialNumber:Int,experimentParams:Array[String],outputDirectory:File) ={
    initializeOutputFile(outputDirectory, experimentName,trialNumber,experimentParams)
    this.process()
    finalizeOutputFile
  }

  def initializeOutputFile(directory:String,experimentName:String,trialNumber:Int,args:Array[String]):Unit ={
    val fileName = directory+experimentName+"-"+trialNumber+".ssv"
    printWriter = new PrintWriter(new File(fileName))
    printWriter.println("Experiment name: "+experimentName)
    printWriter.println("Experiment params: ")
    for(arg <- args)printWriter.println(  "*"+arg)
    printWriter.println("--END-HEADER---")
    printWriter.println()
  }
  def snapshot(time:Long,numSamples:Int,numAccepted:Int,scores:Iterable[Double]):Unit ={
    val line = (time+" "+numSamples+" "+numAccepted+" "+scores.foreach(_ + " ")).trim
    printWriter.println(line)
  }
  def finalizeOutputFile:Unit ={
    printWriter.flush
    printWriter.close
  }
  abstract override def proposalHook(proposal:Proposal) ={
    super.proposalHook
    if(proposalCount % snapshotFrequency == 0){
      val scores = pairF1LabeledOnly(labeledData)
      snapshot(totalTime,proposalCount,numAccepted,scores)
    }
  }

}
*/