package cc.factorie.app.bib.experiments
import cc.factorie.app.bib._
import java.util.Date
import java.io._
import collection.mutable.{ArrayBuffer,HashSet,HashMap}
import java.text.DateFormat
import cc.factorie.util.{CmdOption, DefaultCmdOptions}
import com.mongodb.Mongo
import cc.factorie._
import app.nlp.coref._
import io.Source

class AuthorSamplerWriter(model:Model, val initialDB:Seq[AuthorEntity], val evidenceBatches:Seq[Seq[AuthorEntity]], val initialDBNameOpt:Option[String]=None,val evidenceBatchNames:Option[Seq[String]]=None,var initialSteps:Int=0,stepsPerBatch:Int=10000) extends AuthorSampler(model){
  protected var pwOption:Option[PrintWriter]=None
  val labeledData = initialDB.filter(_.groundTruth != None)
  def snapshotInterval = 10000
  var batchCount = 0
  var mentionCount = labeledData.filter(_.isObserved).size
  var gtEntityCount = labeledData.filter((e:AuthorEntity) => {e.isObserved && e.groundTruth != None}).map(_.groundTruth.get).toSet.size
  var curScore:Double = 0.0
  var maxScore:Double = 0.0
  private var currentBatchName = "initial"
  if(initialDBNameOpt!=None)currentBatchName = initialDBNameOpt.get
  def processExperiment(pw:PrintWriter):Unit ={
    batchCount = 0
    pwOption=Some(pw)
    println("LABELED DATA SIZE "+labeledData.size)
    setEntities(initialDB)
    pw.println("time samples accepted f1 p r batch-count mentions entities batch-name score maxscore")
    //snapshot(totalTime,proposalCount,numAccepted,Evaluator.pairF1LabeledOnly(labeledData))
    println("Inferring initial database.")
    timeAndProcess(initialSteps)
    snapshot(totalTime,proposalCount,numAccepted,Evaluator.pairF1LabeledOnly(labeledData))
    println("About to process evidence stream.")
    for(evidenceBatch <- evidenceBatches){
      batchCount += 1
      evidenceBatch.foreach(this.addEntity(_))
      if(evidenceBatchNames != None)currentBatchName = evidenceBatchNames.get(batchCount-1)
      println("\n---Adding new evidence batch---")
      println("Batch name: "+currentBatchName)
      println("Mentions added: ")
      for(mention <- evidenceBatch)println(EntityUtils.prettyPrintAuthor(mention))

      mentionCount += evidenceBatch.filter(_.isObserved).size
      gtEntityCount = entities.filter((e:AuthorEntity) => {e.isObserved && e.groundTruth != None}).map(_.groundTruth.get).toSet.size
      process(stepsPerBatch)
      //snapshot(totalTime,proposalCount,numAccepted,Evaluator.pairF1LabeledOnly(labeledData))
    }
  }
  override def proposalHook(proposal:Proposal) ={
    super.proposalHook(proposal)
    curScore += proposal.modelScore
    if(curScore>maxScore)maxScore=curScore
    if(proposalCount % snapshotInterval == 0){
      val scores = Evaluator.pairF1LabeledOnly(labeledData)
      snapshot(totalTime,proposalCount,numAccepted,scores)
    }
  }
  def snapshot(time:Long,numSamples:Int,numAccepted:Int,scores:Iterable[Double]):Unit ={
    for(pw <- pwOption){
      val line = ((System.currentTimeMillis - time)/1000L + " "+numSamples+" "+numAccepted+" "+scores.mkString(" ")+" "+batchCount+" "+mentionCount+" "+gtEntityCount+" "+currentBatchName+" "+curScore+" "+maxScore)
      pw.println(line)
      pw.flush()
    }
  }
}


trait HumanEditOptions extends ExperimentOptions{
  val heExperimentType = new CmdOption("he-experiment-type","merge-correct","FILE","Experiment to run for human edits. Options are merge-correct, merge-incorrect, merge-all, split-correct, split-incorrect")
  val heShouldLinkReward = new CmdOption("he-should-link-reward",8.0,"DOUBLE","Should link reward for human edit template.")
  val heShouldNotLinkPenalty = new CmdOption("he-should-not-link-penalty",8.0,"DOUBLE","Should not link penalty for human edit template.")
  val heNumSynthesisSamples = new CmdOption("he-num-synthesis-samples",1000000,"DOUBLE","Should link reward for human edit template.")
}

object EpiDBExperimentOptions extends MongoOptions with DataOptions with InferenceOptions with AuthorModelOptions with HumanEditOptions{
  val advanceSeed = new CmdOption("advance-seed",0,"INT","Number of times to call random.nextInt to advance the seed.")
  val outputFile = new CmdOption("outputFile","experiment.log","FILE","Output file for experimental results.")
  val outputDir = new CmdOption("outputDir","/Users/mwick/data/rexa2/experiments/","FILE","Root output directory containing intermediate results and final results")
  val scratchDir = new CmdOption("scratchDir","scratch/","FILE","Directory for intermediate results of experiment")
  val resultsDir = new CmdOption("resultsDir","results/","FILE","Directory for final results of experiment")
  val metaDir = new CmdOption("metaDir","meta/","FILE","Directory for meta information about results (parameters, settings, configurations used etc.)")
  val experimentName = new CmdOption("name","NONE","FILE","Name of experiment to run")
  val initialDBPercent = new CmdOption("initial-db-pct",0.5,"","Percentage of labeled data to include in the initial database")
  val numFolds = new CmdOption("num-folds",3,"","Number of folds for CV, this indirectly determines the size of the initial DB: initialDB.size=labeledData.size/numFolds")
  val fold = new CmdOption("fold",0,"","Specifies which fold to use as the inital DB.")
  val evidenceBatchSize = new CmdOption("evidence-batch-size",10,"","Size of each streaming batch of evidence")
  val inferenceStepsPerBatch = new CmdOption("inference-steps-per-batch",100000,"","Number of inference steps per batch of incoming evidence")
  val inferenceInitialSteps = new CmdOption("inference-steps-initial",0,"","Nubmer of steps of inference to run on the initial DB")
  val evidenceStreamType = new CmdOption("evidence-stream-type","random","","Types of evidence streams, current options are: random, byyear, human edits.")


  def main(argsIn:Array[String]):Unit ={
    var args:Array[String]=new Array[String](0)
    if(argsIn.length>0 && argsIn.head.startsWith("--config")){
      val contents = scala.io.Source.fromFile(new File(argsIn.head.split("=")(1))).mkString
      args = contents.split("\\s+") ++ argsIn
    } else args=argsIn
    println("Args: "+args.length)
    for(arg <- args)
      println("  "+arg)
    parse(args)
    //writeOptions(new File(this.metaDir.value+experimentName.value))
    if(ldaModel.wasInvoked)Coref.ldaFileOpt = Some(ldaModel.value)
    for(i<-0 until advanceSeed.value)random.nextInt
    if(dropDB.value){
      println("Dropping database.")
      val mongoConn = new Mongo(server.value,port.value.toInt)
      val mongoDB = mongoConn.getDB(database.value)
      mongoDB.getCollection("authors").drop
      mongoDB.getCollection("papers").drop
      mongoDB.getCollection("venues").drop
      mongoConn.close
    }
    println("server: "+server.value+" port: "+port.value.toInt+" database: "+database.value)
    val authorCorefModel = new AuthorCorefModel
    def opts = this
    if(opts.entitySizeWeight.value != 0.0)authorCorefModel += new EntitySizePrior(opts.entitySizeWeight.value,opts.entitySizeExponent.value)
    if(opts.bagTopicsWeight.value != 0.0)authorCorefModel += new ChildParentCosineDistance[BagOfTopics](opts.bagTopicsWeight.value,opts.bagTopicsShift.value)
    if(opts.bagTopicsEntropy.value != 0.0)authorCorefModel += new EntropyBagOfWordsPriorWithStatistics[BagOfTopics](opts.bagTopicsEntropy.value)
    if(opts.bagTopicsPrior.value != 0.0)authorCorefModel += new BagOfWordsPriorWithStatistics[BagOfTopics](opts.bagTopicsPrior.value)
    if(opts.bagCoAuthorWeight.value != 0.0)authorCorefModel += new ChildParentCosineDistance[BagOfCoAuthors](opts.bagCoAuthorWeight.value,opts.bagCoAuthorShift.value)
    if(opts.bagCoAuthorEntropy.value != 0.0)authorCorefModel += new EntropyBagOfWordsPriorWithStatistics[BagOfCoAuthors](opts.bagCoAuthorEntropy.value)
    if(opts.bagCoAuthorPrior.value != 0.0)authorCorefModel += new BagOfWordsPriorWithStatistics[BagOfCoAuthors](opts.bagCoAuthorPrior.value)
    if(opts.bagVenuesWeight.value != 0.0)authorCorefModel += new ChildParentCosineDistance[BagOfVenues](opts.bagVenuesWeight.value,opts.bagVenuesShift.value)
    if(opts.bagVenuesEntropy.value != 0.0)authorCorefModel += new EntropyBagOfWordsPriorWithStatistics[BagOfVenues](opts.bagVenuesEntropy.value)
    if(opts.bagVenuesPrior.value != 0.0)authorCorefModel += new BagOfWordsPriorWithStatistics[BagOfVenues](opts.bagVenuesPrior.value)
    if(opts.bagKeywordsWeight != 0.0)authorCorefModel += new ChildParentCosineDistance[BagOfKeywords](opts.bagKeywordsWeight.value,opts.bagKeywordsShift.value)
    if(opts.bagKeywordsEntropy.value != 0.0)authorCorefModel += new EntropyBagOfWordsPriorWithStatistics[BagOfKeywords](opts.bagKeywordsEntropy.value)
    if(opts.bagKeywordsPrior.value != 0.0)authorCorefModel += new BagOfWordsPriorWithStatistics[BagOfKeywords](opts.bagKeywordsPrior.value)
    if(opts.entityExistencePenalty.value!=0.0 && opts.subEntityExistencePenalty.value!=0.0)authorCorefModel += new StructuralPriorsTemplate(opts.entityExistencePenalty.value, opts.subEntityExistencePenalty.value)
    val epiDB = new EpistemologicalDB(authorCorefModel,server.value,port.value.toInt,database.value)
    println("About to add data.")
    var papers = new ArrayBuffer[PaperEntity]
    if(bibDirectory.value.toLowerCase != "none"){
      println("Adding mentions from BibTeX directory: "+bibDirectory.value.toLowerCase)
      papers ++= BibReader.loadBibTexDirMultiThreaded(new File(bibDirectory.value))
      println("  total papers: "+papers.size)
    }
    if(rexaData.value.toLowerCase != "none"){
      println("Loading labeled data from: " + rexaData.value)
      papers ++= RexaLabeledLoader.load(new File(rexaData.value))
      println("  total papers: "+papers.size)
    }
    if(dblpLocation.value.toLowerCase != "none"){
      println("Loading dblp data from: "+dblpLocation.value)
      papers ++= DBLPLoader.loadDBLPData(dblpLocation.value)
      println("  total papers: "+papers.size)
    }
    /*
    if(this.filterPapersOnLabeledCanopies.value){
      println("About to filter papers so only those in a labeled author's canopy are inserted into DB.")
      val labeledAuthors = epiDB.authorColl.loadLabeledAndCanopies
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
    */

    println("About to add "+papers.size+" papers.")
    epiDB.add(papers)
    println("Finished adding papers.")
    var authors:Seq[AuthorEntity] = random.shuffle(epiDB.authorColl.loadLabeledAndCanopies)
    var initialDB:Seq[AuthorEntity] = null
    var evidenceBatches:Seq[Seq[AuthorEntity]] = null
    var evidenceBatchNamesOpt:Option[Seq[String]] = None
    var initialDBNameOpt:Option[String] = None
    println("Authors.size" +authors.size)
    EntityUtils.checkIntegrity(authors)
    Evaluator.eval(authors)
    println("Evidence stream: "+evidenceStreamType.value)
    if(!evidenceStreamType.wasInvoked)throw new Exception("Remember to specify the type of evidence you want to stream.")
    if(evidenceStreamType.value=="human-edits"){
      //do human edit experiment
      authorCorefModel += new HumanEditTemplate(opts.heShouldLinkReward.value,opts.heShouldNotLinkPenalty.value)
      opts.heExperimentType.value match{
        case "merge-correct" =>{
          authors = authors.filter(_.groundTruth != None)
          //create evidence stream by running inference, then considering all possible merges that would increase F1 score
          val samplerForCreatingInitialDB = new AuthorSampler(authorCorefModel){temperature = 0.001}
          samplerForCreatingInitialDB.setEntities(authors)
          samplerForCreatingInitialDB.timeAndProcess(opts.heNumSynthesisSamples.value)
          initialDB = samplerForCreatingInitialDB.getEntities
          val edits = GenerateExperiments.allMergeEdits[AuthorEntity](initialDB,_ => new AuthorEntity, (entities:Seq[AuthorEntity])=>{Evaluator.pairF1(entities).head}).filter(_.isCorrect)
          evidenceBatches = new ArrayBuffer[Seq[AuthorEntity]]
          for(edit <- edits)evidenceBatches.asInstanceOf[ArrayBuffer[Seq[AuthorEntity]]] += edit.mentions
        }
        case "merge-incorrect" => {

        }
        case _ => throw new Exception("Human edit experiment type "+opts.heExperimentType.value + " not implemented.")
      }
      println("Finished generating  human edit experiment")
    }
    else if(this.evidenceStreamType.value=="random"){
      val (initialDB2,evidence) = split(authors,numFolds.value,fold.value)
      initialDB=initialDB2
      println("initialDB.size "+initialDB.size+" evidence.size: "+evidence.size)
      evidence.foreach(_.groundTruth=None) //so it `won't get evaluated during the experiment
      evidenceBatches = randomEqualPartitioning(evidence,evidenceBatchSize.value)
    }
    else if(this.evidenceStreamType.value=="byyear"){
      val years = this.partitionByYear(authors)
      println("Paper counts by year.")
      println("   year: count")
      var yearCount = 0
      for((k,v) <- years.toList.sortBy(_._1)){
        println("   "+k+": "+v.size)
        if(k != -1)yearCount += v.size
      }
      println("Number of papers with year: "+yearCount)
      val targetSize = yearCount/numFolds.value
      val initialDBBuffer = new ArrayBuffer[AuthorEntity]
      val evidenceBatchesBuffer = new ArrayBuffer[Seq[AuthorEntity]]
      val evidenceBatchNames = new ArrayBuffer[String]
      evidenceBatchNamesOpt = Some(evidenceBatchNames)
      var initialDBName:String = ""
      for((k,v) <- years.toList.sortBy(_._1)){
        if(k != -1){
          if(initialDBBuffer.size<targetSize){
            if(initialDBName.length==0)initialDBName = k.toString
            println("Adding year "+ k + " to initial DB.")
            initialDBBuffer ++= v
          } else {
            if(initialDBName.length<=4)initialDBName += "-"+k.toString
            evidenceBatchesBuffer += v
            evidenceBatchNames += k.toString
          }
        }
      }
      initialDBNameOpt=Some(initialDBName)
      println("Initial DB name: "+initialDBName)
      if(years.contains(-1)){
        evidenceBatchesBuffer += years(-1)
        evidenceBatchNames += "????"
      }
      initialDB = initialDBBuffer
      evidenceBatches = evidenceBatchesBuffer
      println("  initialDB.size: " + initialDB.size)
      println("  num evidence batches: "+evidenceBatches.size)
    }
    else println("unrecognized evidence stream: "+evidenceStreamType.value)
    val sampler = new AuthorSamplerWriter(authorCorefModel,initialDB,evidenceBatches,initialDBNameOpt,evidenceBatchNamesOpt,inferenceInitialSteps.value,inferenceStepsPerBatch.value){temperature = 0.001}
    sampler.processExperiment(new PrintWriter(new File(outputFile.value)))
  }
  def split[T](seq:Seq[T],numFolds:Int,fold:Int):(Seq[T],Seq[T]) = {
    val folds = randomEqualPartitioning(seq,(scala.math.ceil(seq.size.toDouble/numFolds.toDouble)).toInt)
    val other = new ArrayBuffer[T]
    for(i<-0 until folds.size)if(i!=fold)other ++= folds(i)
    (folds(fold),other)
  }
  def randomSubset[T](seq:Seq[T],pct:Double):Seq[T] = randomSubset[T](seq,(seq.size.toDouble*pct).toInt)
  def randomSubset[T](seq:Seq[T],n:Int):Seq[T] = random.shuffle(seq).take(n)
  def randomEqualPartitioning[T](seq:Seq[T],partitionSize:Int):Seq[Seq[T]] = {
    val result = new ArrayBuffer[Seq[T]]
    var batch = new ArrayBuffer[T]
    random.shuffle(seq)
    var i =0
    while(i<seq.size){
      if(i % partitionSize==0){
        batch = new ArrayBuffer[T]
        result += batch
      }
      batch += seq(i)
      i+=1
    }
    println("SEQ SIZE: "+seq.size+" batches: " + result.size)
    result
  }
  def partitionByYear[T<:Attr](seq:Seq[T]):HashMap[Int,Seq[T]] ={
    val result = new HashMap[Int,ArrayBuffer[T]]
    for(s <- seq){
      val b = result.getOrElseUpdate(s.attr[Year].intValue,new ArrayBuffer[T])
      b += s
    }
    result.asInstanceOf[HashMap[Int,Seq[T]]]
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
  val entitySizeExponent = new CmdOption("model-author-size-prior-exponent", 1.2, "N", "Exponent k for rewarding entity size: w*|e|^k")
  val entitySizeWeight = new CmdOption("model-author-size-prior-weight", 0.05, "N", "Weight w for rewarding entity size: w*|e|^k.")
  //author names
//  val bagFirstInitialWeight = new CmdOption("model-author-bag-first-initial-weight", 4.0, "N", "Penalty for first initial mismatches.")
//  val bagFirstNameWeight = new CmdOption("model-author-bag-first-name-weight", 4.0, "N", "Penalty for first name mismatches")
//  val bagMiddleInitialWeight = new CmdOption("model-author-bag-middle-initial-weight", 4.0, "N", "Penalty for first initial mismatches.")
//  val bagMiddleNameWeight = new CmdOption("model-author-bag-middle-name-weight", 4.0, "N", "Penalty for first name mismatches")
  //structural priors
  val entityExistencePenalty = new CmdOption("model-author-entity-penalty", 2.0, "N", "Penalty for a top-level author entity existing")
  val subEntityExistencePenalty = new CmdOption("model-author-subentity-penalty", 0.25, "N", "Penalty for an author subentity existing")
  val bagFirstNamePenalty = new CmdOption("model-author-firstname-penalty", 16.0, "N", "Penalty for having multiple first names")
  val bagMiddleNamePenalty = new CmdOption("model-author-middlename-penalty", 16.0, "N", "Penalty for having multiple middle names")
}

trait PaperModelOptions extends ExperimentOptions{
  val paperBagOfTitlesWeight = new CmdOption("model-paper-bag-titles-weight", 4.0, "N", "Penalty for bag-of-papers cosine distance template (paper coreference model).")
  val paperBagOfTitlesShift = new CmdOption("model-paper-bag-titles-shift", -0.125, "N", "Shift for bag-of-papers cosine distance template (paper coreference model).")
  val paperBagTitlesEntropy = new CmdOption("model-paper-bag-titles-entropy", 0.0, "N", "Entropy penalty for bag-of-titles cosine distance template (paper coreference model).")
  val paperBagTitlesPrior = new CmdOption("model-paper-bag-titles-prior", 0.0, "N", "Prior for bag-of-titles cosine distance template (paper coreference model).")
  val paperBagOfAuthorsWeight = new CmdOption("model-paper-bag-authors-weight", 4.0, "N", "Penalty for bag-of-authors cosine distance template (paper coreference model).")
  val paperBagOfAuthorsShift = new CmdOption("model-paper-bag-authors-shift", -0.125, "N", "Shift for bag-of-authors cosine distance template (paper coreference model).")
  val paperBagAuthorsEntropy = new CmdOption("model-paper-bag-authors-entropy", 0.0, "N", "Entropy penalty for bag-of-authors cosine distance template (paper coreference model).")
  val paperBagAuthorsPrior = new CmdOption("model-paper-bag-authors-prior", 0.0, "N", "Prior for bag-of-authors cosine distance template (paper coreference model).")
  val paperYearPenalty = new CmdOption("model-paper-year-penalty-title", 4, "N", "Penalizes mismatching years (paper coreference model).")
  val paperEntityExistencePenalty = new CmdOption("model-paper-entity-penalty", 2.0, "N", "Penalty for a top-level paper entity existing (paper coreference model).")
  val paperSubEntityExistencePenalty = new CmdOption("model-paper-subentity-penalty", 0.25, "N", "Penalty for a paper subentity existing (paper coreference model).")
}