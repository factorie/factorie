package cc.factorie.app.bib.experiments
import cc.factorie.app.bib._
import java.util.Date
import java.io._
import collection.mutable.{ArrayBuffer,HashSet,HashMap}
import java.text.DateFormat
import cc.factorie.util.{CmdOption, DefaultCmdOptions}
import com.mongodb.{MongoClient, Mongo}
import cc.factorie._
import app.nlp.hcoref._
import io.Source
import scala.util.Random
import cc.factorie.util.{Attr,Cubbie}
import Utils.random
import cc.factorie.variable.DiffList
import cc.factorie.model.{TemplateModel, Model}
import cc.factorie.infer.Proposal

class DebugDiffList extends DiffList{
  override def scoreAndUndo(model:Model): Double = {
    for(family <- model.asInstanceOf[TemplateModel].families){
      family match{
        case f:DebugableTemplate => f.debugOn
        case _ => {}
      }
    }
    if (this.length == 0) return 0.0  // short-cut the simple case
    println("=====DEBUGGING MODEL SCORE=====")
    println("----NEXT WORLD----")
    var s = model.currentScore(this)
    println("  next: "+ s)
    //log(Log.DEBUG)("DiffList scoreAndUndo  pre-undo score=" + s)
    this.undo()
    // We need to re-calculate the Factors list because the structure may have changed
    println("----CURRENT WORLD----")
    val s2 = model.currentScore(this)
    println("  current: "+s2)
    s -= s2
    println("TOTAL SCORE: "+s)
    for(family <- model.asInstanceOf[TemplateModel].families){
      family match{
        case f:DebugableTemplate => f.debugOff
        case _ => {}
      }
    }
    //log(Log.DEBUG)("DiffList scoreAndUndo post-undo score=" + s)
    s
  }}
class AuthorSamplerWriter(model:Model, val initialDB:Seq[AuthorEntity], evBatches:Seq[Seq[AuthorEntity]], val initialDBNameOpt:Option[String]=None,val evidenceBatchNames:Option[Seq[String]]=None,var initialSteps:Int=0,stepsPerBatch:Int=10000,initInstructionsOpt:Option[Seq[Seq[()=>Unit]]]) extends AuthorSampler(model) with HumanEditDebugUtils{
  protected var evidenceBatches:Seq[Seq[AuthorEntity]] = null
  this.evidenceBatches=evBatches
  protected var pwOption:Option[PrintWriter]=None
  val labeledData = initialDB.filter(_.groundTruth != None)
  def snapshotInterval = 10000
  var batchCount = 0
  var mentionCount = labeledData.count(_.isObserved)
  var gtEntityCount = labeledData.filter((e:AuthorEntity) => {e.isObserved && e.groundTruth != None}).map(_.groundTruth.get).toSet.size
  var curScore:Double = 0.0
  var maxScore:Double = 0.0
  private var currentBatchName = "initial"
  if(initialDBNameOpt!=None)currentBatchName = initialDBNameOpt.get
  var evidenceSoFar = new ArrayBuffer[AuthorEntity]


  def processExperiment(pw:PrintWriter,newEvidenceBatches:Seq[Seq[AuthorEntity]]){
    this.evidenceBatches=newEvidenceBatches
    processExperiment(pw)
  }

  def processExperiment(pw:PrintWriter):Unit ={
    if(stepsPerBatch==0)return
    //batchCount = 0
    pwOption=Some(pw)
    println("LABELED DATA SIZE "+labeledData.size)
    setEntities(initialDB)
    if(this.batchCount==0)pw.println("time samples accepted f1 p r batch-count mentions entities batch-name score maxscore")
    //snapshot(totalTime,proposalCount,numAccepted,Evaluator.pairF1LabeledOnly(labeledData))
    println("Inferring initial database.")
    timeAndProcess(initialSteps)
    snapshot(totalTime,proposalCount.toInt,numAccepted.toInt,Evaluator.pairF1LabeledOnly(labeledData))
    println("About to process evidence stream with "+evidenceBatches.size + " evidence batches.")
    for(evidenceBatch <- evidenceBatches){
      batchCount += 1
      evidenceBatch.foreach(this.addEntity(_))
      if(initInstructionsOpt!=None){
        println("Executing initialization instructions")
        for(initInstruction <- initInstructionsOpt.get.apply(batchCount-1)){
          initInstruction.apply()
        }
      }
      if(evidenceBatchNames != None)currentBatchName = evidenceBatchNames.get(batchCount-1)
      println("\n---Adding new evidence batch---")
      println("  Batch name: "+currentBatchName)
      println("  Mentions added: "+evidenceBatch.size)
      if(evidenceBatch.size<=2){
        println("  Mentions added: ")
        evidenceBatch.map((e:AuthorEntity)=>EntityUtils.prettyPrintAuthor(e.entityRoot.asInstanceOf[AuthorEntity]))
        println("  Mentions added: "+evidenceBatch.map(_.id).toSeq)
      }
      println("  About to sample for "+stepsPerBatch + " steps.")
      //for(mention <- evidenceBatch)println(EntityUtils.prettyPrintAuthor(mention))
      checkEntities()
      mentionCount += evidenceBatch.count(_.isObserved)
      gtEntityCount = entities.filter((e:AuthorEntity) => {e.isObserved && e.groundTruth != None}).map(_.groundTruth.get).toSet.size
      process(stepsPerBatch)
      //this.performMaintenance(this.entities)
      println("\n---DEBUG---")
//      println("Evidence ended up:")
//      evidenceBatch.foreach((e:AuthorEntity)=>EntityUtils.prettyPrintAuthor(e.entityRoot.asInstanceOf[AuthorEntity]))
      //model.familiesOfClass[HumanEditTemplate].head.debugFlag=true
      evidenceSoFar ++= evidenceBatch
      postInferenceHook(evidenceSoFar)
      val debugged = new HashSet[AuthorEntity]
      for(e<-evidenceBatch){
        for(gf <- e.generatedFrom)
          println("Purity of edit source: "+EntityUtils.purity(gf))
        /*
        println("Checking affinity to generated-from entity")
        debugEditAffinityToGenerator(e)
        println("Checking should-link constraint")
        debugEditAffinityToLinked(e)
        println("Checking if edit worked as intended")
        debugEdit(e)
	postInferenceHook(evidenceSoFar)
        println("Checking SNL constraint")
        debugEdit(e)
        */
        if(!debugged.contains(e)){
          debugged += e
          for(l<-e.linkedMention)debugged += l.asInstanceOf[AuthorEntity]
          debugSNLConstraint(e)
        }
      }
      //model.familiesOfClass[HumanEditTemplate].head.debugFlag=false
      //snapshot(totalTime,proposalCount,numAccepted,Evaluator.pairF1LabeledOnly(labeledData))
    }
    /*
    println("Streamed SNL statistics")
    printSNLStatistics
    reset
    println("Recomputing statistics")
    val debugged = new HashSet[AuthorEntity]
    for(evidenceBatch <- evidenceBatches){
      for(e<-evidenceBatch){
        if(!debugged.contains(e)){
          debugged += e
          for(l<-e.linkedMention)debugged += l.asInstanceOf[AuthorEntity]
          debugSNLConstraint(e)
        }
      }
    }
    println("Final SNL statistics")
    printSNLStatistics
    */
  }
  /* this method can be overridden in subclasses to do extra processing.  It will
   * be used to do the user reliability experiments */
  def postInferenceHook(evidenceSoFar: Seq[AuthorEntity]):Unit = {
    val edits = new HashSet[HumanEditExperiments.ExperimentalEdit[AuthorEntity]]
    for(e<-evidenceSoFar){
      if(e.memberOfEdit!=None){
        val edit = e.memberOfEdit.get.asInstanceOf[HumanEditExperiments.ExperimentalEdit[AuthorEntity]]
        edits += edit
      }
    }
    println("amount of evidence "+evidenceSoFar.size)
    println("amount of edits "+edits.size)
    ExperimentsEditTracker.computeAndPrintStats(edits)
  }

  /*
  def debugEditAffinityToLinked(e:AuthorEntity):Unit ={
    if(!(e.entityRoot eq e.linkedMention.get.entityRoot)){
      val d = new DebugDiffList
      this.mergeUp(e.entityRoot.asInstanceOf[AuthorEntity],e.linkedMention.get.entityRoot.asInstanceOf[AuthorEntity])(d)
      val score = d.scoreAndUndo(model)
      println("  Failed test 2. Affinity diff-score to link: "+score)
      if(score<=0.0)println("    test2 failure: model") else println("    test2 failure: inference")
    } else println("  Passed test 2: should link edits already in same entity.")
  }
  def debugEditAffinityToGenerator(e:AuthorEntity):Unit ={
    if(!(e.entityRoot eq e.generatedFrom.get.entityRoot)){
      val d = new DebugDiffList
      if(!e.generatedFrom.get.entityRoot.isLeaf)this.mergeLeft(e.generatedFrom.get.entityRoot.asInstanceOf[AuthorEntity],e)(d)
      else this.mergeUp(e.generatedFrom.get.asInstanceOf[AuthorEntity],e.entityRoot.asInstanceOf[AuthorEntity])(d)
      val score = d.scoreAndUndo(model)
      println("  Failed test 1. Affinity diff-score to generator: "+score)
      if(score<=0.0)println("    test1 failure: model") else println("    test1 failure: inference")
    } else println("  Passed test 1: edit in generated entity.")
  }
  def debugEdit(e:AuthorEntity):Unit ={
    if(!(e.entityRoot eq e.linkedMention.get.entityRoot) && !(e.entityRoot eq e.generatedFrom.get.entityRoot)){
      val d = new DebugDiffList
      if(!e.generatedFrom.get.entityRoot.isLeaf)this.mergeLeft(e.generatedFrom.get.entityRoot.asInstanceOf[AuthorEntity],e)(d)
      else this.mergeUp(e.generatedFrom.get.asInstanceOf[AuthorEntity],e.entityRoot.asInstanceOf[AuthorEntity])(d)
      this.mergeUp(e.entityRoot.asInstanceOf[AuthorEntity],e.linkedMention.get.entityRoot.asInstanceOf[AuthorEntity])(d)
      val score = d.scoreAndUndo(model)
      println("  Failed test 3. Edit did not accomplish merge: "+score)
      if(score<=0.0)println("    test3 failure: model") else println("    test3 failure: inference")
    } else println("  Passed test 3.")
  }
  */
  override def proposalHook(proposal:Proposal[Null]) ={
    super.proposalHook(proposal)
    curScore += proposal.modelScore
    if(curScore>maxScore)maxScore=curScore
    if(proposalCount % snapshotInterval == 0){
      val scores = Evaluator.pairF1LabeledOnly(labeledData)
      snapshot(totalTime,proposalCount.toInt,numAccepted.toInt,scores)
    }
  }
  def snapshot(time:Long,numSamples:Int,numAccepted:Int,scores:Iterable[Double]):Unit ={
    for(pw <- pwOption){
      val line = (System.currentTimeMillis - time) / 1000L + " " + numSamples + " " + numAccepted + " " + scores.mkString(" ") + " " + batchCount + " " + mentionCount + " " + gtEntityCount + " " + currentBatchName + " " + curScore + " " + maxScore
      pw.println(line)
      pw.flush()
    }
  }
  def checkEntities(): Unit = {
    val eids = new HashSet[String]
    eids ++= entities.map(_.id.toString)
    if(eids.size!=entities.size){
      throw new Exception("Error, duplicate entity detected: hashset:"+eids.size+" set: "+entities.size)
    }
  }
}

class AuthorSamplerWriterWithReliability(model:Model, initialDB:Seq[AuthorEntity], evidenceBatches:Seq[Seq[AuthorEntity]], initialDBNameOpt:Option[String]=None, evidenceBatchNames:Option[Seq[String]]=None, initialSteps:Int=0, stepsPerBatch:Int=10000, initInstructionsOpt:Option[Seq[Seq[()=>Unit]]]) extends AuthorSamplerWriter(model, initialDB, evidenceBatches, initialDBNameOpt, evidenceBatchNames, initialSteps, stepsPerBatch, initInstructionsOpt) {
  var averageReliability = new UserReliabilityVariable
  var users:Seq[UserReliabilityVariable]=null
  var averageCorrect:Double = 0.0
  override def reset() = {averageReliability = new UserReliabilityVariable;super.reset()}
  def setUsers(us:Seq[UserReliabilityVariable]):Unit ={
    this.users = us
    for(user<-users){
      averageReliability.totalImpactfulEdits += user.totalImpactfulEdits
      averageReliability.totalEdits += user.totalEdits
    }
    averageReliability.updateValue(null)
  }
  def positiveEdit(editMention:AuthorEntity):Boolean = {
    if(editMention.editType==HumanEditMention.ET_SHOULD_LINK)positiveSLEdit(editMention)
    else positiveSNLEdit(editMention)
  }
  def positiveSLEdit(editMention:AuthorEntity): Boolean = {
    val linked = editMention.linkedMention.get.asInstanceOf[AuthorEntity]
    val sourceOfEdit = editMention.generatedFrom.get.asInstanceOf[AuthorEntity]
    val sourceOfLinked = linked.generatedFrom.get.asInstanceOf[AuthorEntity]
    if(sourceOfEdit.entityRoot eq sourceOfLinked.entityRoot) true  //the merge edit had the effect of merging the original entities into the same cluster
    else false //the merge edit did not have the desired effect of merging htem into the same cluster
  }
  def positiveSNLEdit(editMention:AuthorEntity): Boolean = {
    val linked = editMention.linkedMention.get.asInstanceOf[AuthorEntity]
    val sourceOfEdit = editMention.generatedFrom.get.asInstanceOf[AuthorEntity]
    val sourceOfLinked = linked.generatedFrom.get.asInstanceOf[AuthorEntity]
    if(!sourceOfEdit.entityRoot.eq(sourceOfLinked.entityRoot)) true  //the merge edit had the effect of merging the original entities into the same cluster
    else false //the merge edit did not have the desired effect of merging htem into the same cluster
  }
  override def postInferenceHook(evidence: Seq[AuthorEntity]): Unit = {
    super.postInferenceHook(evidence)
    val considered = new HashSet[AuthorEntity]
    val checked = new HashSet[AuthorEntity]
    averageReliability.totalImpactfulEdits=0.0
    averageReliability.totalEdits=0.0
    for(user <- users){
      user.totalImpactfulEdits=1.0
      user.totalEdits = 2.0
      user.updateValue(null)
      averageReliability.totalImpactfulEdits+=1.0
      averageReliability.totalEdits+=2.0
    }
    averageReliability.updateValue(null)
    for (edit <- evidence.filter(_.isEdit)) {
      if(!checked.contains(edit)){
        checked += edit
        checked += edit.linkedMention.get.asInstanceOf[AuthorEntity]
        val urv = edit.attr[UserReliabilityVariable]
        if (positiveEdit(edit)) {
          urv.totalImpactfulEdits += 1
          averageReliability.totalImpactfulEdits += 1
        }
        averageReliability.totalEdits += 1
        urv.totalEdits += 1
      }
    }
    averageReliability.updateValue(null)
    for(urv <- users){
      urv.updateValue(null)
      urv.set(urv.percentImpactful/averageReliability.percentImpactful)(null)
    }
    println("Average REL: pct-correct:"+ averageReliability.percentImpactful +" num-correct: "+averageReliability.totalImpactfulEdits+" total: "+averageReliability.totalEdits)
    for(user <- users){
      println("REL: truth: "+user.truth+" adjusted: "+user.doubleValue+" pct-correct: "+ user.percentImpactful +" num-correct: "+user.totalImpactfulEdits+" total: "+user.totalEdits)
    }
    printRelAcc()
    /* PRINT STATEMENTS FOR DEBUGGING */
    //for(edit <- evidence.filter(_.isEdit)){
    //  println("Edit is correct? " + edit.memberOfEdit.get.isCorrect+" score? "+edit.memberOfEdit.get.score+" owner reliability: "+edit.attr[UserReliabilityVariable].doubleValue)
    //}
    //for (edit <- evidence) println("Edit Owner Reliability: " + edit.attr[UserReliabilityVariable].totalImpactfulEdits / edit.attr[UserReliabilityVariable].totalEdits)
  }
  def printRelAcc(): Unit = {
    //this code assumes urv.doubleValue is adjusted by the average
    var relXrel=0.0
    var relXunr=0.0
    var unrXrel=0.0
    var unrXunr=0.0
    //var averageTruth = 0.0;for(urv <- users)averageTruth += urv.truth/users.size.toDouble
    var totalTruth = 0.0;for(urv <- users)totalTruth += urv.truthTotal
    var averageTruth = 0.0;for(urv <- users)averageTruth += urv.truthCorrect/totalTruth
    for(urv <- users){
      if(urv.truth>=averageTruth && urv.doubleValue>=1.0)relXrel += 1
      else if(urv.truth>=averageTruth && urv.doubleValue<1.0)unrXrel += 1
      else if(urv.truth<averageTruth && urv.doubleValue < 1.0)unrXunr += 1
      else relXunr += 1
    }
    val tp = relXrel+unrXunr
    val fp = unrXrel
    val fn = relXunr
    val precision = tp/(tp+fp)
    val recall = tp/(tp+fn)
    val f1 = 2*precision*recall/(precision+recall)
    val accuracy = tp/(tp+fp+fn)
    println("  EVALRE: Accuracy: "+accuracy)
    println("  EVALRE: F1: "+f1)
    println("  EVALRE: precition: "+precision)
    println("  EVALRE: recall: "+recall)
    println("    EVALRE STAT tp: "+tp)
    println("    EVALRE STAT fp: "+fp)
    println("    EVALRE STAT fn: "+fn)
    println("    EVALRE STAT   CONFUSION MATRIX")
    println("    EVALRE STAT          rel  unr")
    println("    EVALRE STAT   rel: "+relXrel.toInt+" "+relXunr.toInt)
    println("    EVALRE STAT   unr: "+unrXrel.toInt+" "+unrXunr.toInt)
    println("RELACC")
    var totalError = 0.0
    var numMaliciousUsers=0.0
    var numBenevolentUsers=0.0
    var errorForMalicious = 0.0
    var errorForBenevolent=0.0
    for(urv <- users){
      val error = scala.math.pow(urv.doubleValue - urv.truth/averageTruth,2)
      println(" RELACC: predicted: "+urv.doubleValue+" truth: "+(urv.truth/averageTruth)+"  l2err: "+error)
      totalError += error
      if(urv.truth>=0.8){
        errorForBenevolent += error
        numBenevolentUsers += 1.0
      } else {
        errorForMalicious += error
        numMaliciousUsers += 1.0
      }
    }
    totalError /= (numBenevolentUsers+numMaliciousUsers)
    errorForMalicious /= numMaliciousUsers
    errorForBenevolent /= numBenevolentUsers
    println("  AVGRELAC: average reliability errors (mean squared). total: "+totalError+" bene: "+errorForBenevolent+" mali: "+errorForMalicious)
  }
}

trait HumanEditOptions extends ExperimentOptions{
  val heExperimentType = new CmdOption("he-experiment-type","merge-correct","FILE","Experiment to run for human edits. Options are merge-correct, merge-incorrect, merge-all, split-correct, split-incorrect")
  val heShouldLinkReward = new CmdOption("he-should-link-reward",8.0,"DOUBLE","Should link reward for human edit template.")
  val heShouldNotLinkPenalty = new CmdOption("he-should-not-link-penalty",8.0,"DOUBLE","Should not link penalty for human edit template.")
  val heSpecializedTemperature = new CmdOption("he-specialized-temperature",1.0,"DOUBLE","A value other than 1 will cause the program to use a specialized model that places a temperature only on factors involving an attribute from a human submitted edit.")
  val heNumSynthesisSamples = new CmdOption("he-num-synthesis-samples",1000000,"DOUBLE","Should link reward for human edit template.")
  val heNumBatches = new CmdOption("he-num-batches",-1,"INT","Number of batches to stream. Default of -1 means n batches.")
  val heMergeExpEMinSize = new CmdOption("he-merge-exp-entity-min-size",3,"INT","Number of batches to stream. Default of -1 means n batches.")
  val heMergeExpEMinPurity = new CmdOption("he-merge-exp-entity-min-purity",0.0,"DOUBLE","Number of batches to stream. Default of -1 means n batches.")
  val heAdvanceSeed = new CmdOption("he-advance-seed",0,"INT","Number of times to call random.nextInt to advance the seed.")
  val heUseParallel = new CmdOption("he-use-parallel",false,"BOOL","If true, will use a parallel sampler to perform the initialization.")
  val heSaveInitialDB = new CmdOption("he-save-initial-db",false,"BOOL","If true, save the initial database.")
  val heInitializeEdits = new CmdOption("he-initialize-edits",false,"BOOL","If true, initialize edits to be in entities they were generated from.")
  val heUseNIncorrectSplits = new CmdOption("he-use-n-incorrect-splits",1,"INT","Relevant only for experiments that use split-incorrect edits: if true, all incorrect splits will be returned, if false, only one per entity.")
  val heAddPctConflict = new CmdOption("he-add-pct-conflict",0.0,"DOUBLE","Generates and adds conflicting edits for a percentage of the edits")
  val heReliabilityUsers = new CmdOption("he-reliability-users",0,"INT","Sets the number of users to estimate reliabilities for. If zero then  reliability estimation is disabled. Not currently implemented for all experiments.")
  val heMoreEvidenceAtEndPct = new CmdOption("he-more-evidence-at-end-pct",0.0,"DOUBLE","The amount of evidence to stream at the end of inference.")
  val heNumEditRounds = new CmdOption("he-num-edit-rounds",1,"INT","Number of edit rounds to perform, currently only implemented for mixed.")
  val heIncludeCorruptiveAttributeEdits = new CmdOption("he-include-corruptive-attribute-edits",false,"BOOL","If true, include corruptive attribute edits.")
  val heReliabilitySensitivity = new CmdOption("he-reliability-sensitivity",1.0,"DOUBLE","Sensitivity to user reliabilities (sigmma in our paper).")
  val heProminentReq = new CmdOption("he-prominent-req",10,"DOUBLE","Number of mentions required to be considered a prominent entity.")
}


object UserEditExperiments extends MongoOptions with DataOptions with InferenceOptions with AuthorModelOptions with HumanEditOptions with EDBExpOpts{
  def main(argsIn:Array[String]) ={
    implicit val random = new scala.util.Random(0)
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
    for(i<-0 until advanceSeed.value)random.nextInt()
    if(dropDB.value){
      println("Dropping database.")
      val mongoConn = new MongoClient(server.value,port.value.toInt)
      val mongoDB = mongoConn.getDB(database.value)
      mongoDB.getCollection("authors").drop()
      mongoDB.getCollection("papers").drop()
      mongoDB.getCollection("venues").drop()
      mongoConn.close()
    }
    println("server: "+server.value+" port: "+port.value.toInt+" database: "+database.value)
    def opts = this
    val authorCorefModel:AuthorCorefModel = if(opts.heSpecializedTemperature.wasInvoked)new HEAuthorCorefModel(opts.heSpecializedTemperature.value) else new AuthorCorefModel(false)
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
    if(opts.bagKeywordsWeight.value != 0.0)authorCorefModel += new ChildParentCosineDistance[BagOfKeywords](opts.bagKeywordsWeight.value,opts.bagKeywordsShift.value)
    if(opts.bagKeywordsEntropy.value != 0.0)authorCorefModel += new EntropyBagOfWordsPriorWithStatistics[BagOfKeywords](opts.bagKeywordsEntropy.value)
    if(opts.bagKeywordsPrior.value != 0.0)authorCorefModel += new BagOfWordsPriorWithStatistics[BagOfKeywords](opts.bagKeywordsPrior.value)
    if(opts.entityExistencePenalty.value!=0.0 || opts.subEntityExistencePenalty.value!=0.0)authorCorefModel += new StructuralPriorsTemplate(opts.entityExistencePenalty.value, opts.subEntityExistencePenalty.value)
    if(opts.bagFirstWeight.value != 0.0)authorCorefModel += new EntityNameTemplate[BagOfFirstNames](opts.bagFirstInitialWeight.value,opts.bagFirstNameWeight.value,opts.bagFirstWeight.value,opts.bagFirstSaturation.value)
    if(opts.bagMiddleWeight.value != 0.0)authorCorefModel += new EntityNameTemplate[BagOfMiddleNames](opts.bagMiddleInitialWeight.value,opts.bagMiddleNameWeight.value,opts.bagMiddleWeight.value,opts.bagMiddleSaturation.value)

    val epiDB = new EpistemologicalDB(authorCorefModel,server.value,port.value.toInt,database.value)
    println("About to add data.")
    var papers = new ArrayBuffer[PaperEntity]
    if(bibDirectory.value.toLowerCase != "none"){
      println("Adding mentions from BibTeX directory: "+bibDirectory.value.toLowerCase)
      papers ++= BibReader.loadBibTexDirMultiThreaded(new File(bibDirectory.value),true,false)
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
    println("About to add "+papers.size+" papers.")
    epiDB.add(papers,FeatureUtils.venueBag(_))
    println("Finished adding papers.")
    var authors:Seq[AuthorEntity] = random.shuffle(epiDB.authorColl.loadLabeledAndCanopies)
    //fill these in below
    var sampler:AuthorSamplerWriter = null
    var initialDB:Seq[AuthorEntity] = null
    var evidenceBatches:Seq[Seq[AuthorEntity]] = null
    var evidenceBatchNamesOpt:Option[Seq[String]] = None
    var initialDBNameOpt:Option[String] = None
    var initInstructionsOpt:Option[Seq[Seq[()=>Unit]]] = None
    ExperimentsEditTracker.beginStats(outputFile.value)
    println("Authors.size" +authors.size)
    EntityUtils.checkIntegrity(authors)
    Evaluator.eval(authors)
    authors = authors.filter((a:AuthorEntity) => {a.groundTruth != None || a.bagOfTruths.size>0})
    //declare vars
    val numGreedyUsers = 5
    val pwbl1 = HumanEditExperiments.getExperimentPrintWriter(new File(outputFile.value+".baseline1"))
    val pwbl2 = HumanEditExperiments.getExperimentPrintWriter(new File(outputFile.value+".baseline2"))
    val pw = new PrintWriter(new File(outputFile.value))
    initialDB = authors
    //generate user edits
    val userNames = random.shuffle(HumanEditExperiments.getUserNamesFromProminentAuthors(authors,heProminentReq.value)).map(_._1)
    val greedyUsers = userNames.take(numGreedyUsers)
    println("ALL USERS: "+userNames)
    println("Greedy users: "+greedyUsers)
    val edits = new ArrayBuffer[HumanEditExperiments.ExperimentalEdit[AuthorEntity]]
    for(userName <- greedyUsers)edits ++= HumanEditExperiments.simulateGreedyUser(authors,userName,userNames,20)
    for(i<-0 until userNames.size)if(i>numGreedyUsers)edits ++= HumanEditExperiments.simulateBenevolentSelfInterestedUser(authors,userNames(i))
    val corrective = edits.filter(_.isCorrect)
    if(heAddPctConflict.value>0.0 && heAddPctConflict.value<=1.0){
      val contradictoryEdits = HumanEditExperiments.generateContradictingEdits(corrective.take((corrective.size.toDouble*heAddPctConflict.value).toInt))
      println("Adding contradictory: "+contradictoryEdits.size+"  (pct: "+heAddPctConflict.value+")")
      edits ++= contradictoryEdits
      HumanEditExperiments.assignUnlabeledToUsers(contradictoryEdits,greedyUsers)
    }
    evidenceBatches = HumanEditExperiments.edits2evidenceBatches(random.shuffle(edits),opts.heNumBatches.value)
    evidenceBatches = random.shuffle(evidenceBatches)
    //print statistics
    println("InitialDB size. Num nodes:"+initialDB.size+" num entities: "+initialDB.count(_.isEntity.booleanValue)+".")
    if(opts.heReliabilityUsers.value != 0){
      println("Users")
      println("  -number of author users: "+userNames.size)
      println("  -number of greedy users: "+numGreedyUsers+"  ("+(numGreedyUsers.toDouble/ userNames.size.toDouble)+"%)")
    }
    println("Edits")
    println("  -total: "+edits.size)
    println("  -corrective: "+corrective.size)
    println("  -corruptive: "+edits.count(!_.isCorrect))
    println("  -snl: "+edits.count(_.isInstanceOf[HumanEditExperiments.ExpMergeEdit[AuthorEntity]]))
    println("  -sl: "+edits.count(_.isInstanceOf[HumanEditExperiments.ExpSplitEdit[AuthorEntity]]))
    //write baselines
    HumanEditExperiments.baseline1(initialDB,evidenceBatches,pwbl1)
    HumanEditExperiments.baseline2(initialDB,evidenceBatches,pwbl2)
    //run main experiment system
    if(opts.heReliabilityUsers.value != 0){
      authorCorefModel += new HumanEditTemplateWithReliability(opts.heShouldLinkReward.value,opts.heShouldNotLinkPenalty.value,heReliabilitySensitivity.value)
      val users = HumanEditExperiments.createUsersFromAuthorEdits(edits,userNames)
      val relsamp = new AuthorSamplerWriterWithReliability(authorCorefModel,initialDB,evidenceBatches,initialDBNameOpt,evidenceBatchNamesOpt,inferenceInitialSteps.value,inferenceStepsPerBatch.value, None){temperature = 0.001}
      relsamp.setUsers(users)
      sampler=relsamp
    } else {
      authorCorefModel += new HumanEditTemplate(opts.heShouldLinkReward.value,opts.heShouldNotLinkPenalty.value)
      sampler = new AuthorSamplerWriter(authorCorefModel,initialDB,evidenceBatches,initialDBNameOpt,evidenceBatchNamesOpt,inferenceInitialSteps.value,inferenceStepsPerBatch.value, None){temperature = 0.001}
    }
    sampler.processExperiment(pw)
    initialDB = sampler.getEntities
    pw.flush();pw.close()
    pwbl1.flush();pwbl1.close()
    pwbl2.flush();pwbl2.close()
    ExperimentsEditTracker.endStats()
  }
}
trait EDBExpOpts extends ExperimentOptions{
    val advanceSeed = new CmdOption("advance-seed",0,"INT","Number of times to call random.nextInt to advance the seed.")
  val outputFile = new CmdOption("outputFile","experiment.log","FILE","Output file for experimental results.")
  val outputDir = new CmdOption("outputDir","/Users/akobren/data/rexa2/experiments/","FILE","Root output directory containing intermediate results and final results")
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
}

object EpiDBExperimentOptions extends MongoOptions with DataOptions with InferenceOptions with AuthorModelOptions with HumanEditOptions with EDBExpOpts{


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
    for(i<-0 until advanceSeed.value)random.nextInt()
    if(dropDB.value){
      println("Dropping database.")
      val mongoConn = new MongoClient(server.value,port.value.toInt)
      val mongoDB = mongoConn.getDB(database.value)
      mongoDB.getCollection("authors").drop()
      mongoDB.getCollection("papers").drop()
      mongoDB.getCollection("venues").drop()
      mongoConn.close()
    }
    println("server: "+server.value+" port: "+port.value.toInt+" database: "+database.value)
    def opts = this
    val authorCorefModel:AuthorCorefModel = if(opts.heSpecializedTemperature.wasInvoked)new HEAuthorCorefModel(opts.heSpecializedTemperature.value) else new AuthorCorefModel(false)
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
    //
    if(opts.bagFirstWeight.value != 0.0)authorCorefModel += new EntityNameTemplate[BagOfFirstNames](opts.bagFirstInitialWeight.value,opts.bagFirstNameWeight.value,opts.bagFirstWeight.value,opts.bagFirstSaturation.value)
    if(opts.bagMiddleWeight.value != 0.0)authorCorefModel += new EntityNameTemplate[BagOfMiddleNames](opts.bagMiddleInitialWeight.value,opts.bagMiddleNameWeight.value,opts.bagMiddleWeight.value,opts.bagMiddleSaturation.value)

    val epiDB = new EpistemologicalDB(authorCorefModel,server.value,port.value.toInt,database.value)
    println("About to add data.")
    var papers = new ArrayBuffer[PaperEntity]
    if(bibDirectory.value.toLowerCase != "none"){
      println("Adding mentions from BibTeX directory: "+bibDirectory.value.toLowerCase)
      papers ++= BibReader.loadBibTexDirMultiThreaded(new File(bibDirectory.value),true,false)
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
    println("About to add "+papers.size+" papers.")
    epiDB.add(papers,FeatureUtils.venueBag(_))
    println("Finished adding papers.")
    var authors:Seq[AuthorEntity] = random.shuffle(epiDB.authorColl.loadLabeledAndCanopies)
    var initialDB:Seq[AuthorEntity] = null
    var evidenceBatches:Seq[Seq[AuthorEntity]] = null
    var evidenceBatchNamesOpt:Option[Seq[String]] = None
    var initialDBNameOpt:Option[String] = None
    var initInstructionsOpt:Option[Seq[Seq[()=>Unit]]] = None
    println("Authors.size" +authors.size)
    EntityUtils.checkIntegrity(authors)
    Evaluator.eval(authors)

    println("Evidence stream: "+evidenceStreamType.value)
    if(!evidenceStreamType.wasInvoked)throw new Exception("Remember to specify the type of evidence you want to stream.")
    if(evidenceStreamType.value=="human-edits"){
      for(i<-0 until heAdvanceSeed.value)random.nextInt()
      ExperimentsEditTracker.beginStats(outputFile.value)
      //do human edit experiment
      //val humanEditTemplate = new HumanEditTemplate(opts.heShouldLinkReward.value,opts.heShouldNotLinkPenalty.value)
      //authorCorefModel += humanEditTemplate
      opts.heExperimentType.value match{
        case "attributes" =>{ //use --he-num-batches=1, steps-per-batch=100000 (at least)
          val vandalizedFirst = "Arnold"
          val vandalizedMiddle = "Schwarzenegger"
          authors = authors.filter((a:AuthorEntity) => {a.groundTruth != None || a.bagOfTruths.size>0})
          initialDB=authors
          //evaluate initial kb
          val (initAccuracy,initVandalized) = HumanEditExperiments.evaluateInferredAccuracy(authors,vandalizedFirst,vandalizedMiddle)
          println("ATTR INITIAL KB: accuracy: "+initAccuracy+", percent vandalized: "+initVandalized)
          Evaluator.eval(authors)
          //generate edits
          val edits = random.shuffle(HumanEditExperiments.generateAttributeEdits(authors) ++ HumanEditExperiments.generateAttributeVandalism(authors,vandalizedFirst,vandalizedMiddle,0.5))
          //evaluate baseline
          val (baselineAccuracy, baselineVandalized) = HumanEditExperiments.evaluateCompleteTrustInEditsAccuracy(authors,edits,vandalizedFirst,vandalizedMiddle)
          println("ATTR FINAL BASELINE KB: accuracy: "+baselineAccuracy+", percent vandalized: "+baselineVandalized)
          //make edits into evidence, and run epistemological system
          evidenceBatches = HumanEditExperiments.edits2evidenceBatches(edits,opts.heNumBatches.value)
          val sampler = new AuthorSamplerWriter(authorCorefModel,initialDB,evidenceBatches,initialDBNameOpt,evidenceBatchNamesOpt,inferenceInitialSteps.value,inferenceStepsPerBatch.value, None){temperature = 0.001}
          sampler.processExperiment(new PrintWriter(new File(outputFile.value)))
          val newAuthors = sampler.getEntities
          val (finalAccuracy,finalVandalized) = HumanEditExperiments.evaluateInferredAccuracy(newAuthors,vandalizedFirst,vandalizedMiddle)
          println("ATTR FINAL EPI KB: accuracy: "+finalAccuracy+", percent vandalized: "+finalVandalized)
          //now try with user reliability estimates
          if(heReliabilityUsers.value>0){
            //setup user reliabilities
            val averageReliability = new UserReliabilityVariable
            val numUsers = heReliabilityUsers.value
            val users    = Array.fill(numUsers)(new UserReliabilityVariable)
            for(user <- users){
              user.totalImpactfulEdits = 1
              user.totalEdits = 2
              user.updateValue(null)
            }
            var correctId=0
            var partiallyCorrectId=numUsers/3
            var vandalismId=numUsers*2/3
            for (edit <- edits) {
              var userIdx = 0
              //if(random.nextDouble<=0.33)userIdx=2
              if(edit.isCorrect){
                userIdx = correctId
                correctId += 1;if(correctId>=numUsers/3)correctId=0
              }
              else if(!edit.isCorrect && edit.score == -1.0){
                userIdx=partiallyCorrectId
                partiallyCorrectId += 1;if(partiallyCorrectId>=numUsers)partiallyCorrectId=numUsers/3
              }else if(!edit.isCorrect && edit.score == -2.0){
                userIdx=vandalismId
                vandalismId += 1;if(vandalismId>=numUsers)vandalismId=numUsers/3
              }else println("ERROR, EDIT DOES NOT MEET THIS CRITERIA")
              edit.setOwner(userIdx)
              edit.mentions.foreach(_.attr += users(userIdx))
            }
            println("Num Edits: " + edits.length)
            for (idx  <- 0 until numUsers) {
              println("idx: "+idx+" num with idx: "+edits.count(_.owner == idx))
              //users(idx).truth = edits.filter(_.owner == idx).filter(_.isCorrect).length.toDouble / edits.filter(_.owner == idx).length.toDouble
              users(idx).truthCorrect = edits.filter(_.owner == idx).count(_.isCorrect).toDouble
              users(idx).truthTotal = edits.count(_.owner == idx).toDouble
            }
            for (user <- users) println("Users with reliability: " + user.truth)
            //compute reliabilities
            println("COMPUTING RELIABILITIES, RE-RUNNING INFERENCE WITH ESTIMATES.")
            for (idx  <- 0 until heReliabilityUsers.value) {
              val user = users(idx)
              val userEdits = edits.filter(_.owner == idx)
              user.totalImpactfulEdits = HumanEditExperiments.computeUserReliabilityForAttributeEdits(userEdits)
              user.totalEdits = userEdits.size
              user.updateValue(null)
            }
            for(user<-users){
              averageReliability.totalImpactfulEdits += user.totalImpactfulEdits
              averageReliability.totalEdits += user.totalEdits
            }
            averageReliability.updateValue(null)
            for(user<-users){
              averageReliability.totalImpactfulEdits += user.totalImpactfulEdits
              averageReliability.totalEdits += user.totalEdits
            }
            for(urv <- users){
              urv.updateValue(null)
              urv.set(urv.percentImpactful/averageReliability.percentImpactful)(null)
            }
            println("Average REL: pct-correct:"+ averageReliability.percentImpactful +" num-correct: "+averageReliability.totalImpactfulEdits+" total: "+averageReliability.totalEdits)
            for(user <- users){
              println("REL: truth: "+user.truth+" adjusted: "+user.doubleValue+" pct-correct: "+ user.percentImpactful +" num-correct: "+user.totalImpactfulEdits+" total: "+user.totalEdits)
            }
            val newAuthors2 = sampler.getEntities
            val (relAccuracy,relVandalized) = HumanEditExperiments.evaluateInferredAccuracy(newAuthors2,vandalizedFirst,vandalizedMiddle,true)
            println("ATTR FINAL EPI REL: accuracy: "+relAccuracy+", percent vandalized: "+relVandalized)
          }
          //evaluate epistemological approach
          Evaluator.eval(newAuthors)
          println("Done with attribute evaluation")
          System.exit(0)
        }
        case "merge-correct" =>{
          //do human edit experiment
          val humanEditTemplate = new HumanEditTemplate(opts.heShouldLinkReward.value,opts.heShouldNotLinkPenalty.value)
          authorCorefModel += humanEditTemplate
          authors = authors.filter((a:AuthorEntity) => {a.groundTruth != None || a.bagOfTruths.size>0})
          //create evidence stream by running inference, then considering all possible merges that would increase F1 score
          val samplerForCreatingInitialDB = if(heUseParallel.value) new ParallelAuthorSampler(authorCorefModel, 5){temperature=0.001} else new AuthorSampler(authorCorefModel){temperature = 0.001}
          samplerForCreatingInitialDB.setEntities(authors)
          samplerForCreatingInitialDB.timeAndProcess(opts.heNumSynthesisSamples.value)
          initialDB = samplerForCreatingInitialDB.getEntities
          if(heSaveInitialDB.value){
            epiDB.authorColl.store(samplerForCreatingInitialDB.getEntities ++ samplerForCreatingInitialDB.getDeletedEntities)
            println("Human edits merge-correct: saved initial db and exiting.")
            System.exit(0)
          }
          val edits = HumanEditExperiments.generateSubEntitySLEdits(initialDB,0.8,4,2).filter(_.isCorrect)
          //val edits = HumanEditExperiments.getAuthorEdits(initialDB,opts.heMergeExpEMinSize.value,opts.heMergeExpEMinPurity.value).filter(_.isCorrect)
          evidenceBatches = HumanEditExperiments.edits2evidenceBatches(edits,opts.heNumBatches.value)
          evidenceBatches = random.shuffle(evidenceBatches)
          HumanEditExperiments.mergeBaseline1(initialDB,evidenceBatches,new File(outputFile.value+".baseline1"))
          HumanEditExperiments.mergeBaseline2(initialDB,evidenceBatches,new File(outputFile.value+".baseline2"))
        }
        case "split-with-reliability" => {
          //do human edit experiment
          val humanEditTemplate = new HumanEditTemplateWithReliability(opts.heShouldLinkReward.value,opts.heShouldNotLinkPenalty.value)
          //humanEditTemplate.debugFlag=true
          authorCorefModel += humanEditTemplate
          authors = authors.filter((a:AuthorEntity) => {a.groundTruth != None || a.bagOfTruths.size>0})
          //create evidence stream by running inference, then considering all possible merges that would increase F1 score
          val samplerForCreatingInitialDB = if(heUseParallel.value) new ParallelAuthorSampler(authorCorefModel, 5){temperature=0.001} else new AuthorSampler(authorCorefModel){temperature = 0.001}
          samplerForCreatingInitialDB.setEntities(authors)
          samplerForCreatingInitialDB.timeAndProcess(opts.heNumSynthesisSamples.value)
          initialDB = samplerForCreatingInitialDB.getEntities
          if(heSaveInitialDB.value){
            epiDB.authorColl.store(samplerForCreatingInitialDB.getEntities ++ samplerForCreatingInitialDB.getDeletedEntities)
            println("Human edits split-with-reliability: saved initial db and exiting.")
            System.exit(0)
          }
          val correctEdits = HumanEditExperiments.correctAuthorSplitEdits(initialDB)
          val incorrectEdits = random.shuffle(HumanEditExperiments.incorrectAuthorSplitEdits(initialDB,opts.heUseNIncorrectSplits.value)).take(correctEdits.size)
          evidenceBatches = HumanEditExperiments.edits2evidenceBatches(correctEdits++incorrectEdits,opts.heNumBatches.value)
          val editsUsed = correctEdits ++ incorrectEdits
          /* create user reliabilities */
          val numUsers = 10                   // arbitrary and should probably be passed in as a command line argument
          val users    = Array.fill(numUsers)(new UserReliabilityVariable)
          for(user <- users){
            user.totalImpactfulEdits = 1
            user.totalEdits = 2
            user.updateValue(null)
          }
          /* Assign each edit to a user and then calculate reliabilities */
          var correctId=0
          var incorrectId=numUsers/2
          for (edit <- editsUsed) {
            var userIdx = 0
            //if(random.nextDouble<=0.33)userIdx=2
            if(edit.isCorrect){
              userIdx = correctId
              correctId += 1;if(correctId>=numUsers/2)correctId=0
            }
            else if(!edit.isCorrect){
              userIdx=incorrectId
              incorrectId += 1;if(incorrectId>=numUsers)incorrectId=numUsers/2
            }
            println("USR ID: "+userIdx)
            edit.setOwner(userIdx)
            edit.mention1.attr += users(userIdx)
            edit.mention2.attr += users(userIdx)
          }
          /* Assign each edit to a user and then calculate reliabilities */
          println("Num Edits: " + editsUsed.length)
          for (idx  <- 0 until numUsers) {
            println("idx: "+idx+" num with idx: "+editsUsed.count(_.owner == idx))
            users(idx).truthCorrect = editsUsed.filter(_.owner == idx).count(_.isCorrect).toDouble
            users(idx).truthTotal = editsUsed.count(_.owner == idx).toDouble
            //users(idx).truth = editsUsed.filter(_.owner == idx).filter(_.isCorrect).length.toDouble / editsUsed.filter(_.owner == idx).length.toDouble
          }
          for (user <- users) println("Users with reliability: " + user.truth)
          println("Num Correct: " + correctEdits.length.toString)
          println("Num Incorrect: " + incorrectEdits.length.toString)
          /* maybe make a new version of edits2evidenceBatches that doesn't cast and instead returns ExperimentalEdits */
          evidenceBatches = random.shuffle(evidenceBatches)
          HumanEditExperiments.splitBaseline1(initialDB,evidenceBatches,new File(outputFile.value+".baseline1"))
          if(opts.inferenceStepsPerBatch.value==0){
            println("About to exit because steps per batch is 0")
            System.exit(0)
          }
          val sampler = new AuthorSamplerWriterWithReliability (authorCorefModel,initialDB,evidenceBatches,initialDBNameOpt,evidenceBatchNamesOpt,inferenceInitialSteps.value,inferenceStepsPerBatch.value, None){temperature = 0.001}
          sampler.setUsers(users.toSeq)
          sampler.processExperiment(new PrintWriter(new File(outputFile.value)))
          System.out.println("Finished reliability experiment... exiting.")
          System.exit(0)
        }
        case "merge-with-reliability" => {
          //do human edit experiment
          val humanEditTemplate = new HumanEditTemplateWithReliability(opts.heShouldLinkReward.value,opts.heShouldNotLinkPenalty.value)
          //humanEditTemplate.debugFlag=true
          authorCorefModel += humanEditTemplate
          authors = authors.filter((a:AuthorEntity) => {a.groundTruth != None || a.bagOfTruths.size>0})
          //create evidence stream by running inference, then considering all possible merges that would increase F1 score
          val samplerForCreatingInitialDB = if(heUseParallel.value) new ParallelAuthorSampler(authorCorefModel, 5){temperature=0.001} else new AuthorSampler(authorCorefModel){temperature = 0.001}
          samplerForCreatingInitialDB.setEntities(authors)
          samplerForCreatingInitialDB.timeAndProcess(opts.heNumSynthesisSamples.value)
          initialDB = samplerForCreatingInitialDB.getEntities
          if(heSaveInitialDB.value){
            epiDB.authorColl.store(samplerForCreatingInitialDB.getEntities ++ samplerForCreatingInitialDB.getDeletedEntities)
            println("Human edits merge-with-reliability: saved initial db and exiting.")
            System.exit(0)
          }
          val edits = HumanEditExperiments.generateSubEntitySLEdits(initialDB,0.8,4,2)
          //val edits = HumanEditExperiments.getAuthorEdits(initialDB,opts.heMergeExpEMinSize.value,opts.heMergeExpEMinPurity.value)
          val correctEdits = edits.filter(_.isCorrect)
          val incorrectEdits =edits.filter(! _.isCorrect).sortBy(_.score).take(correctEdits.size)
          //val incorrectEdits = random.shuffle(edits.filter(! _.isCorrect)).take(correctEdits.size)
          //val correctEdits = HumanEditExperiments.correctAuthorSplitEdits(initialDB)
          //var incorrectEdits = HumanEditExperiments.incorrectAuthorSplitEdits(initialDB)
          val editsUsed = correctEdits ++ incorrectEdits
          evidenceBatches = HumanEditExperiments.edits2evidenceBatches(editsUsed,opts.heNumBatches.value)
          /* create user reliabilities */
          val numUsers = 10                  // arbitrary and should probably be passed in as a command line argument
          val users    = Array.fill(numUsers)(new UserReliabilityVariable)
          for(user <- users){
            user.totalImpactfulEdits = 1
            user.totalEdits = 2
            user.updateValue(null)
          }
          /* Assign each edit to a user and then calculate reliabilities */
          var correctId=0
          var incorrectId=numUsers/2
          for (edit <- editsUsed) {
            var userIdx = 0
            //if(random.nextDouble<=0.33)userIdx=2
            if(edit.isCorrect){
              userIdx = correctId
              correctId += 1;if(correctId>=numUsers/2)correctId=0
            }
            else if(!edit.isCorrect){
              userIdx=incorrectId
              incorrectId += 1;if(incorrectId>=numUsers)incorrectId=numUsers/2
            }
            println("USR ID: "+userIdx)
            edit.setOwner(userIdx)
            edit.mention1.attr += users(userIdx)
            edit.mention2.attr += users(userIdx)
          }
          /* DEBUG */
          println("Num Edits: " + editsUsed.length)
          for (idx  <- 0 until numUsers){
            //users(idx).truth = editsUsed.filter(_.owner == idx).filter(_.isCorrect).length.toDouble / edits.filter(_.owner == idx).length.toDouble
            users(idx).truthCorrect = editsUsed.filter(_.owner == idx).count(_.isCorrect).toDouble
            users(idx).truthTotal = editsUsed.count(_.owner == idx).toDouble
          }
          /* PRINT STATEMENTS FOR DEBUGGING */
          for (user <- users) println("Users with reliability: " + user.truth+" initial reliability: "+user.doubleValue)
          /* DEBUG */
          println("Num Correct: " + correctEdits.length.toString)
          println("Num Incorrect: " + incorrectEdits.length.toString)
          /* maybe make a new version of edits2evidenceBatches that doesn't cast and instead returns ExperimentalEdits */
          evidenceBatches = random.shuffle(evidenceBatches)
          HumanEditExperiments.mergeBaseline1(initialDB,evidenceBatches,new File(outputFile.value+".baseline1"))
          HumanEditExperiments.mergeBaseline2(initialDB,evidenceBatches,new File(outputFile.value+".baseline2"))
          if(opts.inferenceStepsPerBatch.value==0){
            println("Inference steps per batch is zero, about to exit")
            System.exit(0)
          }
          var sampler = new AuthorSamplerWriterWithReliability (authorCorefModel,initialDB,evidenceBatches,initialDBNameOpt,evidenceBatchNamesOpt,inferenceInitialSteps.value,inferenceStepsPerBatch.value, None){temperature = 0.001}
          sampler.setUsers(users.toSeq)
          sampler.processExperiment(new PrintWriter(new File(outputFile.value)))
          System.out.println("Finished reliability experiment... exiting.")
          System.exit(0)
        }
        case "merge-incorrect" => {
          //do human edit experiment
          val humanEditTemplate = new HumanEditTemplate(opts.heShouldLinkReward.value,opts.heShouldNotLinkPenalty.value)
          authorCorefModel += humanEditTemplate

          authors = authors.filter((a:AuthorEntity) => {a.groundTruth != None || a.bagOfTruths.size>0})
          //create evidence stream by running inference, then considering all possible merges that would increase F1 score
          val samplerForCreatingInitialDB = if(heUseParallel.value) new ParallelAuthorSampler(authorCorefModel, 5){temperature=0.001} else new AuthorSampler(authorCorefModel){temperature = 0.001}
          samplerForCreatingInitialDB.setEntities(authors)
          samplerForCreatingInitialDB.timeAndProcess(opts.heNumSynthesisSamples.value)
          initialDB = samplerForCreatingInitialDB.getEntities
          if(heSaveInitialDB.value){
            epiDB.authorColl.store(samplerForCreatingInitialDB.getEntities ++ samplerForCreatingInitialDB.getDeletedEntities)
            println("Human edits merge-incorrect: saved initial db and exiting.")
            System.exit(0)
          }
          val edits = HumanEditExperiments.generateSubEntitySLEdits(initialDB,0.8,4,2).filter(! _.isCorrect).sortBy(_.score).take(100)
          if(edits.size>=2){println("Top 2 edits (#1,#2): ("+edits(0).score+" "+edits(1).score+")")}
          //val edits = HumanEditExperiments.getAuthorEdits(initialDB,opts.heMergeExpEMinSize.value,opts.heMergeExpEMinPurity.value).filter(! _.isCorrect)
          evidenceBatches = HumanEditExperiments.edits2evidenceBatches(edits,opts.heNumBatches.value)
          evidenceBatches = random.shuffle(evidenceBatches)
          HumanEditExperiments.mergeBaseline1(initialDB,evidenceBatches,new File(outputFile.value+".baseline1"))
          HumanEditExperiments.mergeBaseline2(initialDB,evidenceBatches,new File(outputFile.value+".baseline2"))
        }
        case "merge-mixed" => {
          //do human edit experiment
          val humanEditTemplate = new HumanEditTemplate(opts.heShouldLinkReward.value,opts.heShouldNotLinkPenalty.value)
          authorCorefModel += humanEditTemplate

          authors = authors.filter((a:AuthorEntity) => {a.groundTruth != None || a.bagOfTruths.size>0})
          //create evidence stream by running inference, then considering all possible merges that would increase F1 score
          val samplerForCreatingInitialDB = if(heUseParallel.value) new ParallelAuthorSampler(authorCorefModel, 5){temperature=0.001} else new AuthorSampler(authorCorefModel){temperature = 0.001}
          samplerForCreatingInitialDB.setEntities(authors)
          samplerForCreatingInitialDB.timeAndProcess(opts.heNumSynthesisSamples.value)
          initialDB = samplerForCreatingInitialDB.getEntities
          if(heSaveInitialDB.value){
            epiDB.authorColl.store(samplerForCreatingInitialDB.getEntities ++ samplerForCreatingInitialDB.getDeletedEntities)
            println("Human edits merge-mixed: saved initial db and exiting.")
            System.exit(0)
          }
          val edits = HumanEditExperiments.generateSubEntitySLEdits(initialDB,0.8,4,2)
          //val edits = HumanEditExperiments.getAuthorEdits(initialDB,opts.heMergeExpEMinSize.value,opts.heMergeExpEMinPurity.value) //.filter(! _.isCorrect)
          val correctEdits = edits.filter(_.isCorrect)
          val incorrectEdits =edits.filter(! _.isCorrect).sortBy(_.score).take(correctEdits.size)
          //val incorrectEdits = random.shuffle(edits.filter(! _.isCorrect)).take(correctEdits.size)
          println("# correct: "+correctEdits.size + " # incorrect: "+incorrectEdits.size)
          evidenceBatches = HumanEditExperiments.edits2evidenceBatches(correctEdits++incorrectEdits,opts.heNumBatches.value)
          evidenceBatches = random.shuffle(evidenceBatches)
          HumanEditExperiments.mergeBaseline1(initialDB,evidenceBatches,new File(outputFile.value+".baseline1"))
          HumanEditExperiments.mergeBaseline2(initialDB,evidenceBatches,new File(outputFile.value+".baseline2"))
        }
        case "split-correct" => {
          val humanEditTemplate = new HumanEditTemplate(opts.heShouldLinkReward.value,opts.heShouldNotLinkPenalty.value)
          authorCorefModel += humanEditTemplate
          val initDiffList = new DiffList
          authors = authors.filter((a:AuthorEntity) => {a.groundTruth != None || a.bagOfTruths.size>0})
          //create evidence stream by running inference, then considering all possible merges that would increase F1 score
          val samplerForCreatingInitialDB = if(heUseParallel.value) new ParallelAuthorSampler(authorCorefModel, 5){temperature=0.001} else new AuthorSampler(authorCorefModel){temperature = 0.001}
          samplerForCreatingInitialDB.setEntities(authors)
          samplerForCreatingInitialDB.timeAndProcess(opts.heNumSynthesisSamples.value)
          initialDB = samplerForCreatingInitialDB.getEntities
          if(heSaveInitialDB.value){
            epiDB.authorColl.store(samplerForCreatingInitialDB.getEntities ++ samplerForCreatingInitialDB.getDeletedEntities)
            println("Human edits split-correct: saved initial db and exiting.")
            System.exit(0)
          }
          val edits = HumanEditExperiments.correctAuthorSplitEdits(initialDB)
          evidenceBatches = HumanEditExperiments.edits2evidenceBatches(edits,opts.heNumBatches.value)
          evidenceBatches = random.shuffle(evidenceBatches)
          if(opts.heInitializeEdits.value){
            val initInstructions = new ArrayBuffer[ArrayBuffer[()=>Unit]]
            for(batch <- evidenceBatches){
              val ibatch = new ArrayBuffer[()=>Unit]
              initInstructions += ibatch
              for(edit <- batch){
                ibatch += {() => {
                  println("linking edit to generator: "+edit.id)
                  println("   is parent null? "+(edit.parentEntity==null))
                  if(!edit.generatedFrom.get.isObserved)EntityUtils.linkChildToParent(edit,edit.generatedFrom.get)(null)
                  //EntityUtils.linkChildToParent(edit.linkedMention.get,edit.linkedMention.get.asInstanceOf[AuthorEntity].generatedFrom.get)(null)
                }}
              }
            }
            initInstructionsOpt = Some(initInstructions)
            /*
            for(edit <- edits.flatMap(_.mentions)){
              if(edit.parentEntity == null){
                EntityUtils.linkChildToParent(edit,edit.generatedFrom.get)(initDiffList)
                EntityUtils.linkChildToParent(edit.linkedMention.get,edit.linkedMention.get.asInstanceOf[AuthorEntity].generatedFrom.get)(initDiffList)
              }
            }
            */
          }
          HumanEditExperiments.splitBaseline1(initialDB,evidenceBatches,new File(outputFile.value+".baseline1"))
          //HumanEditExperiments.applySplitEdits(evidenceBatches)(initDiffList)
          //println("Diff score after applying split edits: "+initDiffList.score(authorCorefModel))
        }
        case "split-incorrect" => {
          val humanEditTemplate = new HumanEditTemplate(opts.heShouldLinkReward.value,opts.heShouldNotLinkPenalty.value)
          authorCorefModel += humanEditTemplate
          val initDiffList = new DiffList
          authors = authors.filter((a:AuthorEntity) => {a.groundTruth != None || a.bagOfTruths.size>0})
          val samplerForCreatingInitialDB = if(heUseParallel.value) new ParallelAuthorSampler(authorCorefModel, 5){temperature=0.001} else new AuthorSampler(authorCorefModel){temperature = 0.001}
          samplerForCreatingInitialDB.setEntities(authors)
          samplerForCreatingInitialDB.timeAndProcess(opts.heNumSynthesisSamples.value)
          initialDB = samplerForCreatingInitialDB.getEntities
          val edits = HumanEditExperiments.incorrectAuthorSplitEdits(initialDB,opts.heUseNIncorrectSplits.value)
          evidenceBatches = HumanEditExperiments.edits2evidenceBatches(edits,opts.heNumBatches.value)
          evidenceBatches = random.shuffle(evidenceBatches)
          if(opts.heInitializeEdits.value){
            val initInstructions = new ArrayBuffer[ArrayBuffer[()=>Unit]]
            for(batch <- evidenceBatches){
              val ibatch = new ArrayBuffer[()=>Unit]
              initInstructions += ibatch
              for(edit <- batch){
                ibatch += {() => {
                  println("linking edit to generator: "+edit.id)
                  println("   is parent null? "+(edit.parentEntity==null))
                  if(!edit.generatedFrom.get.isObserved)EntityUtils.linkChildToParent(edit,edit.generatedFrom.get)(null)
                  //EntityUtils.linkChildToParent(edit.linkedMention.get,edit.linkedMention.get.asInstanceOf[AuthorEntity].generatedFrom.get)(null)
                }}
              }
            }
            initInstructionsOpt = Some(initInstructions)
            /*
            for(edit <- edits.flatMap(_.mentions)){
              if(edit.parentEntity == null){
                EntityUtils.linkChildToParent(edit,edit.generatedFrom.get)(initDiffList)
                EntityUtils.linkChildToParent(edit.linkedMention.get,edit.linkedMention.get.asInstanceOf[AuthorEntity].generatedFrom.get)(initDiffList)
              }
            }
            */
          }
          HumanEditExperiments.splitBaseline1(initialDB,evidenceBatches,new File(outputFile.value+".baseline1"))
          //HumanEditExperiments.applySplitEdits(evidenceBatches)(initDiffList)
          //println("Diff score after applying split edits: "+initDiffList.score(authorCorefModel))
        }
         case "split-mixed" => {
          //do human edit experiment
          val humanEditTemplate = new HumanEditTemplate(opts.heShouldLinkReward.value,opts.heShouldNotLinkPenalty.value)
          authorCorefModel += humanEditTemplate
          authors = authors.filter((a:AuthorEntity) => {a.groundTruth != None || a.bagOfTruths.size>0})
          //create evidence stream by running inference, then considering all possible merges that would increase F1 score
          val samplerForCreatingInitialDB = if(heUseParallel.value) new ParallelAuthorSampler(authorCorefModel, 5){temperature=0.001} else new AuthorSampler(authorCorefModel){temperature = 0.001}
          samplerForCreatingInitialDB.setEntities(authors)
          samplerForCreatingInitialDB.timeAndProcess(opts.heNumSynthesisSamples.value)
          initialDB = samplerForCreatingInitialDB.getEntities
          if(heSaveInitialDB.value){
            epiDB.authorColl.store(samplerForCreatingInitialDB.getEntities ++ samplerForCreatingInitialDB.getDeletedEntities)
            println("Human edits split-with-reliability: saved initial db and exiting.")
            System.exit(0)
          }
          val correctEdits = HumanEditExperiments.correctAuthorSplitEdits(initialDB)
          val incorrectEdits = random.shuffle(HumanEditExperiments.incorrectAuthorSplitEdits(initialDB,opts.heUseNIncorrectSplits.value)).take(correctEdits.size)
          val editsUsed = correctEdits ++ incorrectEdits
          evidenceBatches = HumanEditExperiments.edits2evidenceBatches(editsUsed,opts.heNumBatches.value)
          println("Num Correct: " + correctEdits.length.toString)
          println("Num Incorrect: " + incorrectEdits.length.toString)
          evidenceBatches = random.shuffle(evidenceBatches)
          HumanEditExperiments.splitBaseline1(initialDB,evidenceBatches,new File(outputFile.value+".baseline1"))
        }
        case "mixed" => {
          val maxCorrective = 100
          val pctCorruptive = 0.25
          if(heReliabilityUsers.value==0){
            println("No users, defaulting to simple HumanEditTemplate")
            val humanEditTemplate = new HumanEditTemplate(opts.heShouldLinkReward.value,opts.heShouldNotLinkPenalty.value)
            authorCorefModel += humanEditTemplate
          }else {
            println("Users defined, creating HumanEditTemplateWithReliability")
            val humanEditTemplate = new HumanEditTemplateWithReliability(opts.heShouldLinkReward.value,opts.heShouldNotLinkPenalty.value)
            authorCorefModel += humanEditTemplate
          }
          val pwbl1 = HumanEditExperiments.getExperimentPrintWriter(new File(outputFile.value+".baseline1"))
          val pwbl2 = HumanEditExperiments.getExperimentPrintWriter(new File(outputFile.value+".baseline2"))
          val pw = new PrintWriter(new File(outputFile.value))
          var sampler:AuthorSamplerWriter = null
          //warning: wrote the below code while sick, have no idea if it work as intended
          authors = authors.filter((a:AuthorEntity) => {a.groundTruth != None || a.bagOfTruths.size>0})
          var additionalEvidenceBatches:Seq[Seq[AuthorEntity]] = null
          if(heMoreEvidenceAtEndPct.value>0.0){
            val numAddBatches=10
            //val ab:Array[ArrayBuffer[AuthorEntity]] = new Array[ArrayBuffer[AuthorEntity]](10);for(i<-0 until numAddBatches)ab(i)=new ArrayBuffer[AuthorEntity]
            val ab = new ArrayBuffer[ArrayBuffer[AuthorEntity]];for(i<-0 until numAddBatches)ab += new ArrayBuffer[AuthorEntity]
            authors = random.shuffle(authors)
            val a1 = new ArrayBuffer[AuthorEntity]
            val a2 = new ArrayBuffer[AuthorEntity]
            val numAuthors = (authors.size.toDouble * (1.0-heMoreEvidenceAtEndPct.value)).toInt
            for(i<-0 until authors.size)if(i<=numAuthors)a1+= authors(i) else a2 += authors(i)
            authors = a1
            var bcount=0
            for(author <- a2){
              ab(bcount) += author
              bcount += 1;if(bcount>=numAddBatches)bcount=0
            }
            //add empty batches just so a few more rounds of inference happens at the end
            for(i<-0 until 20)ab += new ArrayBuffer[AuthorEntity]
            additionalEvidenceBatches = ab.asInstanceOf[Seq[Seq[AuthorEntity]]]
            //additionalEvidenceBatches = for(author <- a2) yield Seq(author)//new ArrayBuffer[Seq[AuthorEntity]]
          }
          //create evidence stream by running inference, then considering all possible merges that would increase F1 score
          val samplerForCreatingInitialDB = if(heUseParallel.value) new ParallelAuthorSampler(authorCorefModel, 5){temperature=0.001} else new AuthorSampler(authorCorefModel){temperature = 0.001}
          samplerForCreatingInitialDB.setEntities(authors)
          samplerForCreatingInitialDB.timeAndProcess(opts.heNumSynthesisSamples.value)
          initialDB = samplerForCreatingInitialDB.getEntities
          if(heSaveInitialDB.value){
            epiDB.authorColl.store(samplerForCreatingInitialDB.getEntities ++ samplerForCreatingInitialDB.getDeletedEntities)
            println("Human edits merge-correct: saved initial db and exiting.")
            System.exit(0)
          }
          //initialize user reliabilities
          var users:Array[UserReliabilityVariable] = null
          val numUsers = heReliabilityUsers.value
          if(numUsers>0){
            users    = Array.fill(numUsers)(new UserReliabilityVariable)
            for(user <- users){
              user.totalImpactfulEdits = 1
              user.totalEdits = 2
              user.updateValue(null)
            }
            val asrwr = new AuthorSamplerWriterWithReliability(authorCorefModel,initialDB,evidenceBatches,initialDBNameOpt,evidenceBatchNamesOpt,inferenceInitialSteps.value,inferenceStepsPerBatch.value, None){temperature = 0.001}
            asrwr.setUsers(users)
            sampler = asrwr
          } else{
            sampler = new AuthorSamplerWriter(authorCorefModel,initialDB,evidenceBatches,initialDBNameOpt,evidenceBatchNamesOpt,inferenceInitialSteps.value,inferenceStepsPerBatch.value, None){temperature = 0.001}
          }
          //outer loop for edit rounds
          for(i<-0 until opts.heNumEditRounds.value){
            println("Edit round "+i + " of "+opts.heNumEditRounds.value)
            //SNL edits
            var correctSNLEdits = HumanEditExperiments.correctAuthorSplitEdits(initialDB).sortBy(_.score).reverse.take(maxCorrective)
            val maxCorruptive = (pctCorruptive*correctSNLEdits.size.toDouble/(1.0-pctCorruptive)).toInt
            val incorrectSNLEdits = HumanEditExperiments.incorrectAuthorSplitEdits(initialDB,opts.heUseNIncorrectSplits.value).sortBy(_.score).take(maxCorruptive)
            val snlEdits = correctSNLEdits ++ incorrectSNLEdits
            //SL edits
            val allSLEdits = HumanEditExperiments.generateSubEntitySLEdits(initialDB,0.8,4,2)
            val correctSLEdits = allSLEdits.filter(_.isCorrect)
            val incorrectSLEdits =allSLEdits.filter(! _.isCorrect).sortBy(_.score).take(correctSLEdits.size)
            val slEdits = incorrectSLEdits ++ correctSLEdits
            val correctEdits = correctSLEdits ++ correctSNLEdits
            val incorrectEdits = incorrectSLEdits ++ incorrectSNLEdits
            var edits = slEdits ++ snlEdits
            println("Total edits: "+edits.size)
            println("  #sl: "+slEdits.size)
            println("  #snl: "+snlEdits.size)
            println("  #corrective: "+correctEdits.size)
            println("  #corruptive: "+incorrectEdits.size)
            //contradictory edits
            if(heAddPctConflict.value>0.0 && heAddPctConflict.value<=1.0){
              val contradictoryEdits = HumanEditExperiments.generateContradictingEdits(edits.take((edits.size.toDouble*heAddPctConflict.value).toInt))
              println("  #contradictory: "+contradictoryEdits.size+"  (pct: "+heAddPctConflict.value+")")
              edits = edits ++ contradictoryEdits
              println("  #total: "+edits.size)
            }
            evidenceBatches = HumanEditExperiments.edits2evidenceBatches(edits,opts.heNumBatches.value)
            evidenceBatches = random.shuffle(evidenceBatches)
            HumanEditExperiments.baseline1(initialDB,evidenceBatches,pwbl1)
            HumanEditExperiments.baseline2(initialDB,evidenceBatches,pwbl2)
            if(heMoreEvidenceAtEndPct.value>0.0){
              println("Is head of evidence batches an edit (should be true)? " +evidenceBatches.head.head.isEdit)
              println("Is head of additional evidence batches an edit (should be false)? " +additionalEvidenceBatches.head.head.isEdit)
              evidenceBatches = evidenceBatches ++ additionalEvidenceBatches
              println("Is head of evidence batches still an edit (should be true)? " +evidenceBatches.head.head.isEdit)
            }
            if(numUsers>0){
              /* Assign each edit to a user and then calculate reliabilities */
              var correctId=0
              var incorrectId=numUsers/2
              for (edit <- edits) {
                var userIdx = 0
                //if(random.nextDouble<=0.33)userIdx=2
                if(edit.isCorrect){
                  userIdx = correctId
                  correctId += 1;if(correctId>=numUsers/2)correctId=0
                }
                else if(!edit.isCorrect){
                  userIdx=incorrectId
                  incorrectId += 1;if(incorrectId>=numUsers)incorrectId=numUsers/2
                }
                //println("USR ID: "+userIdx)
                edit.setOwner(userIdx)
                edit.mentions.foreach(_.attr += users(userIdx))
                //edit.mention1.attr += users(userIdx)
                //edit.mention2.attr += users(userIdx)
              }
              /* Assign each edit to a user and then calculate reliabilities */
              println("Num Edits: " + edits.length)
              for (idx  <- 0 until numUsers) {
                println("idx: "+idx+" num with idx: "+edits.count(_.owner == idx))
                //users(idx).truth = edits.filter(_.owner == idx).filter(_.isCorrect).length.toDouble / edits.filter(_.owner == idx).length.toDouble
                users(idx).truthCorrect = edits.filter(_.owner == idx).count(_.isCorrect).toDouble
                users(idx).truthTotal = edits.count(_.owner == idx).toDouble
              }
              for (user <- users) println("Users with reliability: " + user.truth)
              println("Num Correct: " + correctEdits.length.toString)
              println("Num Incorrect: " + incorrectEdits.length.toString)
              /* maybe make a new version of edits2evidenceBatches that doesn't cast and instead returns ExperimentalEdits */
              //if(opts.inferenceStepsPerBatch.value==0){
              //  println("About to exit because steps per batch is 0")
              //  System.exit(0)
              //}
              //val sampler = new AuthorSamplerWriterWithReliability (authorCorefModel,initialDB,evidenceBatches,initialDBNameOpt,evidenceBatchNamesOpt,inferenceInitialSteps.value,inferenceStepsPerBatch.value, None){temperature = 0.001}
              //sampler.setUsers(users.toSeq)
              //sampler.processExperiment(pw,evidenceBatches)
            } else{
              //val sampler = new AuthorSamplerWriterWithReliability (authorCorefModel,initialDB,evidenceBatches,initialDBNameOpt,evidenceBatchNamesOpt,inferenceInitialSteps.value,inferenceStepsPerBatch.value, None){temperature = 0.001}
              //sampler.setUsers(users.toSeq)
              //sampler.processExperiment(pw,evidenceBatches)
            }
            sampler.processExperiment(pw,evidenceBatches)
            initialDB = sampler.getEntities
          }
          System.out.println("Finished MIXED experiment (warning, may not have implemented w/o reliability case)... exiting.")
          pw.flush();pw.close()
          pwbl1.flush();pwbl1.close()
          pwbl2.flush();pwbl2.close()
          System.exit(0)
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
    val sampler = new AuthorSamplerWriter(authorCorefModel,initialDB,evidenceBatches,initialDBNameOpt,evidenceBatchNamesOpt,inferenceInitialSteps.value,inferenceStepsPerBatch.value,initInstructionsOpt){temperature = 0.001}
    sampler.processExperiment(new PrintWriter(new File(outputFile.value)))
    ExperimentsEditTracker.endStats()
  }
  def split[T](seq:Seq[T],numFolds:Int,fold:Int):(Seq[T],Seq[T]) = {
    val folds = randomEqualPartitioning(seq, scala.math.ceil(seq.size.toDouble / numFolds.toDouble).toInt)
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
    pw.close()
  }
  def writeOptions(pw:PrintWriter):Unit ={
    //pw.println("Experiment Parameters: "+DateFormat.getDateInstance(DateFormat.SHORT).format(now))
    this.values.foreach(o => pw.println("--"+o.name+"="+o.value))
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
  val rexaData = new CmdOption("rexaData","/Users/akobren/data/rexa/rexaAll/","FILE","Location of the labeled rexa2 directory.")
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
  val bagTopicsPrior = new CmdOption("model-author-bag-topics-prior", 0.25, "N", "Bag of topics prior penalty, formula is bag.size/bag.oneNorm*weight.")
  val bagTopicsEntropyOrdering = new CmdOption("model-author-bag-topics-entropy-ordering", 0.0, "N", "Bag of topics  penalty for when child has higher entropy than parent.")
  val entitySizeExponent = new CmdOption("model-author-size-prior-exponent", 1.2, "N", "Exponent k for rewarding entity size: w*|e|^k")
  val entitySizeWeight = new CmdOption("model-author-size-prior-weight", 0.05, "N", "Weight w for rewarding entity size: w*|e|^k.")
  //author names
  val bagFirstInitialWeight = new CmdOption("model-author-bag-first-initial-weight", 3.0, "N", "Penalty for first initial mismatches.")
  val bagFirstNoNamePenalty = new CmdOption("model-author-bag-first-noname-penalty", 2.0, "N", "Penalty for first initial mismatches.")
  val bagFirstNameWeight = new CmdOption("model-author-bag-first-name-weight", 3.0, "N", "Penalty for first name mismatches")
  val bagFirstSaturation = new CmdOption("model-author-bag-first-saturation", 16.0, "N", "Penalty for first initial mismatches.")
  val bagFirstWeight = new CmdOption("model-author-bag-first-weight", 1.0, "N", "Penalty for first initial mismatches.")
  val bagMiddleInitialWeight = new CmdOption("model-author-bag-middle-initial-weight", 3.0, "N", "Penalty for first initial mismatches.")
  val bagMiddleNameWeight = new CmdOption("model-author-bag-middle-name-weight", 3.0, "N", "Penalty for first name mismatches")
  val bagMiddleSaturation = new CmdOption("model-author-bag-middle-saturation", 26.0, "N", "Penalty for first initial mismatches.")
  val bagMiddleWeight = new CmdOption("model-author-bag-middle-weight", 1.0, "N", "Penalty for first initial mismatches.")
  val bagMiddleNoNamePenalty = new CmdOption("model-author-bag-middle-noname-penalty", 0.25, "N", "Penalty for first initial mismatches.")

  //structural priors
  val entityExistencePenalty = new CmdOption("model-author-entity-penalty", 2.0, "N", "Penalty for a top-level author entity existing")
  val depthPenalty = new CmdOption("model-depth-penalty", 0.0, "N", "Penalty depth in the tree.")
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
