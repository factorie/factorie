package cc.factorie.app.bib

import _root_.cc.factorie.app.bib.HumanEditExperiments.ExpAttributeEdit
import _root_.cc.factorie.app.bib.HumanEditExperiments.ExpSplitEdit
import cc.factorie.util.Attr
import cc.factorie._
import cc.factorie.app.nlp.hcoref._
import experiments.DebugDiffList
import java.io.{PrintWriter, File, BufferedWriter}
import collection.mutable.{HashMap,HashSet, ArrayBuffer}
import Utils.random
import cc.factorie.variable._
import cc.factorie.model.{TupleTemplateWithStatistics3, Model, Factor}
import scala.Some

class HEAuthorCorefModel(val heTemperature:Double=0.5) extends AuthorCorefModel(false){
  def scoreFactor(factor:Factor):Double = {
    val score = factor.currentScore
    var involvesEdit = false
    var reliability = 1.0
    for(variable <- factor.currentAssignment.variables)variable match {
      case v:EntityAttr => {
        if(v.entity.isInstanceOf[AuthorEntity]){
          val e = v.entity.asInstanceOf[AuthorEntity]
          if(!e.editType.eq(HumanEditMention.ET_NONE)){
            val urv = e.attr[UserReliabilityVariable]
            if(urv!=null)reliability = urv.doubleValue
            involvesEdit=true
          }
        }
      }
      case _ => {}
    }
    if(involvesEdit)score*reliability/heTemperature else score
  }
  override def currentScore(variable:Var): Double = { var sum = 0.0; for (f <- factors(variable)) sum += scoreFactor(f); sum }
  override def currentScore(vars:Iterable[Var]): Double = { var sum = 0.0; for (f <- factors(vars)) sum += scoreFactor(f); sum }
  override def currentScore(d:Diff): Double = { var sum = 0.0; for (f <- factors(d)) sum += scoreFactor(f); sum }
  override def currentScore(dl:DiffList): Double = { var sum = 0.0; for (f <- factors(dl)) sum += scoreFactor(f); sum }
}
/* Keep track of one of these per user */
class UserReliabilityVariable extends RealVariable {
  var totalImpactfulEdits = 0.0
  var totalEdits = 0.0
  def truth = truthCorrect/truthTotal
  var truthCorrect = 0.0
  var truthTotal = 0.0
  def updateValue(d:DiffList) = this.set(totalImpactfulEdits/totalEdits)(d)
  def percentImpactful = totalImpactfulEdits/totalEdits
}

object HumanEditMention{
  val ET_SHOULD_LINK = "should-link"
  val ET_SHOULD_NOT_LINK = "should-not-link"
  val ET_ATTRIBUTE = "attribute"
  val ET_NONE="none"
}
trait HumanEditMention extends Attr{
  def entity:HierEntity
  def editSet = attr[EditSetVariable]
  attr += new EditSetVariable(entity)
  //var linkSet
  var linkedMention:Option[HierEntity] = None
  var editType:String = HumanEditMention.ET_NONE
  def isEdit:Boolean = !editType.eq(HumanEditMention.ET_NONE)
  //attr += new UserReliabilityVariable
  var generatedFrom:Option[Entity] = None
  var memberOfEdit:Option[HumanEditExperiments.ExperimentalEdit[_]] = None
}

class EditSetVariable(val entity: Entity) extends SetVariable[HierEntity with HumanEditMention] with EntityAttr

class HumanEditTemplate(val shouldLinkReward:Double=8.0,val shouldNotLinkPenalty:Double=8.0) extends TupleTemplateWithStatistics3[EntityExists, IsEntity, EditSetVariable] {
var debugFlag=false
  def unroll1(eExists: EntityExists) = Factor(eExists,
    eExists.entity.attr[IsEntity], eExists.entity.attr[EditSetVariable])

  def unroll2(isEntity: IsEntity) = Factor(isEntity.entity.attr[EntityExists],
    isEntity, isEntity.entity.attr[EditSetVariable])

  def unroll3(editSetVar: EditSetVariable) = Factor(
    editSetVar.entity.attr[EntityExists], editSetVar.entity.attr[IsEntity],
    editSetVar)

  def score(eExists: EntityExists#Value, isEntity: IsEntity#Value, editSetVar:
  EditSetVariable#Value): Double = {
    def scoreEdit(entity: HierEntity with HumanEditMention) = {
      var result = 0.0
      //we are dividing by 2 because we are looping over both linked mentions (they are in the same edit set).
      if(entity.editType eq HumanEditMention.ET_SHOULD_LINK){
        if(debugFlag)println("  EditTemplate: should-link mention")
        if(entity.entityRoot eq entity.linkedMention.get.entityRoot){
          if(debugFlag)println("    edit template should-link rewarding mention: "+(shouldLinkReward/2.0))
          result += shouldLinkReward/2.0
        } //TODO: else penalty but don't divide by 2?
      }else if(entity.editType eq HumanEditMention.ET_SHOULD_NOT_LINK){
        if(entity.entityRoot eq entity.linkedMention.get.entityRoot)result -= shouldNotLinkPenalty/2.0
      }
      result
    }
    var result = 0.0
    if(eExists.booleanValue && isEntity.booleanValue){
      if(editSetVar.size>0){
        if(debugFlag)println("EditTemplate (debugging). Size="+editSetVar.size)
        for (edit <- editSetVar) {
          result += scoreEdit(edit)
        }
        if(debugFlag)println("  total points: "+result)
      }
    }
    result
  }
}

class HumanEditTemplateWithReliability(shouldLinkReward: Double, shouldNotLinkPenalty:Double, val sigma:Double=2.0) extends HumanEditTemplate(shouldLinkReward,shouldNotLinkPenalty) {
  /* We need a better scoring function that weightsSet the reliability adjustment
   * based on the number of edits that user has made...maybe something like log(totalEdits)
   * we should mix the two together so that if you have made lots of edits, we are more
   * confident about your reliability.
   *
   * Perhaps we should keep track of "experience" which is the number of edits you've made
   * in comparison to the rest of the editors. Then we can scale the score based on this.
   * To do this though, we need to have access other people's experience...*/
  override def score(eExists: EntityExists#Value, isEntity: IsEntity#Value, editSetVar:
  EditSetVariable#Value): Double = {
    def scoreEdit(entity: HierEntity with HumanEditMention) = {
      var result = 0.0
      //we are dividing by 2 because we are looping over both linked mentions (they are in the same edit set).
      if(entity.editType eq HumanEditMention.ET_SHOULD_LINK){
        if(debugFlag)println("  EditTemplate: should-link mention")
        if(entity.entityRoot eq entity.linkedMention.get.entityRoot){
          if(debugFlag)println("    edit template should-link rewarding mention: "+(shouldLinkReward/2.0)+" reliability: "+entity.attr[UserReliabilityVariable].doubleValue+" correct edit? "+entity.memberOfEdit.get.isCorrect)
          // If the person is reliabile <50% of the time, then their opinion counts negatively
          result += (shouldLinkReward/2.0) * scala.math.pow(entity.attr[UserReliabilityVariable].doubleValue,sigma)
        } //TODO: else penalty but don't divide by 2?
      }else if(entity.editType eq HumanEditMention.ET_SHOULD_NOT_LINK){
        if(debugFlag)println("  EditTemplate: should-not-link mention")
        if(entity.entityRoot eq entity.linkedMention.get.entityRoot){
          if(debugFlag)println("    edit template should-not-link penalizing mention: "+(shouldNotLinkPenalty/2.0)+" reliability: "+entity.attr[UserReliabilityVariable].doubleValue+" correct edit? "+entity.memberOfEdit.get.isCorrect)
          result -= shouldNotLinkPenalty/2.0 * scala.math.pow(entity.attr[UserReliabilityVariable].doubleValue,sigma)
        }
      }
      result
    }
    var result = 0.0
    if(eExists.booleanValue && isEntity.booleanValue){
      if(editSetVar.size>0){
        if(debugFlag)println("EditTemplate (debugging). Size="+editSetVar.size)
        for (edit <- editSetVar) {
          result += scoreEdit(edit)
        }
        if(debugFlag)println("  total points: "+result)
      }
    }
    result
  }
}

object HumanEditExperiments{
  trait ExperimentalEdit[T<:HierEntity with HumanEditMention]{
    var userName:String = null
    def score:Double
    def isCorrect:Boolean = score>0
    def mentions:Seq[T]
    var owner:Int = -1    // integer representing the id of the owner who created this edit
    def setOwner(id: Int): Unit = {
      //for(m <- mentions){
      //  if(m.attr[UserReliabilityVariable]==null){
      //    attr += new UserReliabilityVariable
      //  } else throw new Exception("Error, this edit already has been assigned an owner.")
      //}
      this.owner = id
    }
  }
  class ExpMergeEdit[T<:HierEntity with HumanEditMention](val mention1:T,val mention2:T,val score:Double) extends ExperimentalEdit[T]{
    val mentions:Seq[T]=Seq(mention1,mention2)
    mention1.linkedMention=Some(mention2)
    mention1.editType=HumanEditMention.ET_SHOULD_LINK
    mention1.memberOfEdit = Some(this)
    mention2.linkedMention=Some(mention1)
    mention2.editType=HumanEditMention.ET_SHOULD_LINK
    mention2.memberOfEdit = Some(this)
    def print(): Unit ={
      if(mention1.isInstanceOf[AuthorEntity] && mention2.isInstanceOf[AuthorEntity]){
        println("\n=======PRINTING EDIT=======")
        println("SCORE: "+score)
        println("--EditMention1--")
        EntityUtils.prettyPrintAuthor(mention1.asInstanceOf[AuthorEntity])
        println("--EditMention2--")
        EntityUtils.prettyPrintAuthor(mention2.asInstanceOf[AuthorEntity])
        println("SCORE: "+score)
        println("Mention 1 generated from:")
        EntityUtils.prettyPrintAuthor(mention1.generatedFrom.get.asInstanceOf[AuthorEntity])
        println("Mention 2 generated from:")
        EntityUtils.prettyPrintAuthor(mention2.generatedFrom.get.asInstanceOf[AuthorEntity])
        println("SCORE: "+score)
      }
    }
  }
  class ExpSplitEdit[T<:HierEntity with HumanEditMention](val mention1:T,val mention2:T,val score:Double) extends ExperimentalEdit[T]{
    val mentions:Seq[T]=Seq(mention1,mention2)
    mention1.linkedMention=Some(mention2)
    mention1.editType=HumanEditMention.ET_SHOULD_NOT_LINK
    mention1.memberOfEdit = Some(this)
    mention2.linkedMention=Some(mention1)
    mention2.editType=HumanEditMention.ET_SHOULD_NOT_LINK
    mention2.memberOfEdit = Some(this)
  }
  class ExpAttributeEdit[T<:HierEntity with HumanEditMention](val contextMention:T,val payloadMention:T,val score:Double) extends ExperimentalEdit[T]{
    val mentions:Seq[T]=Seq(contextMention,payloadMention)
    contextMention.linkedMention=Some(payloadMention)
    contextMention.editType=HumanEditMention.ET_ATTRIBUTE
    contextMention.memberOfEdit = Some(this)
    payloadMention.linkedMention=Some(contextMention)
    payloadMention.editType=HumanEditMention.ET_ATTRIBUTE
    payloadMention.memberOfEdit = Some(this)
  }
  def edits2evidenceBatches(edits:Seq[ExperimentalEdit[AuthorEntity]],numBatches:Int):Seq[Seq[AuthorEntity]] ={
    val evidenceBatches = new ArrayBuffer[Seq[AuthorEntity]]
    if(numBatches == -1){
      for(edit <- edits)evidenceBatches.asInstanceOf[ArrayBuffer[Seq[AuthorEntity]]] += edit.mentions
    }else if(numBatches == 1){
      val allEdits = edits.flatMap(_.mentions)
      evidenceBatches.asInstanceOf[ArrayBuffer[Seq[AuthorEntity]]] += allEdits
    }else if(numBatches==0){
      val e2e = new HashMap[AuthorEntity,ArrayBuffer[AuthorEntity]]
      for(edit <- edits){
        for(mention <- edit.mentions){
          e2e.getOrElseUpdate(mention.generatedFrom.get.entityRoot.asInstanceOf[AuthorEntity],new ArrayBuffer[AuthorEntity]) += mention
        }
      }
      evidenceBatches.asInstanceOf[ArrayBuffer[Seq[AuthorEntity]]] ++= e2e.map(_._2)
    } else throw new Exception("Num, evidence batches not implemented for arbitrary values (only -1,1)")
    evidenceBatches
  }
  def scoreMergeEdit(edit:ExpMergeEdit[AuthorEntity]):(Double,Double,Double) ={
    val dl = new DiffList
    val mentions = (edit.mention1.descendantsOfClass[HierEntity]++edit.mention2.descendantsOfClass[HierEntity]) .filter(_.isMention.booleanValue)
    val beforeScores = Evaluator.pairF1(mentions)
    val root = new AuthorEntity("","","",false)
    EntityUtils.linkChildToParent(edit.mention1.generatedFrom.get,root)(dl)
    EntityUtils.linkChildToParent(edit.mention2.generatedFrom.get,root)(dl)
    val afterScores = Evaluator.pairF1(mentions)
    dl.undo()
    val f1 = afterScores(0) - beforeScores(0)
    val p = afterScores(1) - beforeScores(1)
    val r = afterScores(2) - beforeScores(2)
    (f1,p,r)
  }
  def scoreSplitEdit(edit:ExpMergeEdit[AuthorEntity]):(Double,Double,Double) ={
    val dl = new DiffList
    val mentions = edit.mention1.generatedFrom.get.entityRoot.descendantsOfClass[HierEntity].filter(_.isMention.booleanValue)
    val beforeScores = Evaluator.pairF1(mentions)
    val depth1 = edit.mention1.generatedFrom.get.depth
    val depth2 = edit.mention2.generatedFrom.get.depth
    if(depth1 < depth2)EntityUtils.linkChildToParent(edit.mention2.generatedFrom.get,null)(dl)
    else if(depth2<depth1)EntityUtils.linkChildToParent(edit.mention1.generatedFrom.get,null)(dl)
    else{
      EntityUtils.linkChildToParent(edit.mention1.generatedFrom.get,null)(dl)
      EntityUtils.linkChildToParent(edit.mention2.generatedFrom.get,null)(dl)
    }
    val afterScores = Evaluator.pairF1(mentions)
    dl.undo()
    val f1 = afterScores(0) - beforeScores(0)
    val p = afterScores(1) - beforeScores(1)
    val r = afterScores(2) - beforeScores(2)
    (f1,p,r)
  }
  def generateAttributeVandalism(authors:Seq[AuthorEntity],vandalizedFirstName:String,vandalizedMiddleName:String,portionVandalized:Double):Seq[ExpAttributeEdit[AuthorEntity]] ={
    implicit val difflist:DiffList = null
    val edits = new ArrayBuffer[ExpAttributeEdit[AuthorEntity]]
    val authorEntities = random.shuffle(filterAuthorsForAttributeEditing(authors))
    val numVandalized = (authorEntities.size.toDouble*portionVandalized).toInt
    println("Num authors meeting vandalism crieteria: "+authorEntities.size+" target num vandalized: "+numVandalized)
    for(author <- authorEntities.take(numVandalized)){
      val contextMention = copyForAttributeEditMention(author)
      val payloadMention = copyForAttributeEditMention(author)
      //contextMention.attr[FullName].setFirst(vandalizedFirstName)
      //contextMention.attr[FullName].setMiddle(vandalizedMiddleName)
      payloadMention.attr[FullName].setFirst(vandalizedFirstName)
      payloadMention.attr[FullName].setMiddle(vandalizedMiddleName)
      edits += new ExpAttributeEdit[AuthorEntity](contextMention,payloadMention,-2)
    }
    println("  vandalizing edits"+edits.size)
    edits
  }
  def generateAttributeEdits(authors:Seq[AuthorEntity],includeCorruptiveEdits:Boolean=false):Seq[ExpAttributeEdit[AuthorEntity]] ={
    val edits = new ArrayBuffer[ExpAttributeEdit[AuthorEntity]]
    val l2t = label2truth(authors)
    var numCorrective=0
    var numCorruptive=0
    for(author <- filterAuthorsForAttributeEditing(authors)){
      val truthAlignment:String = author.attr[BagOfTruths].value.iterator.maxBy(_._2)._1
      val trueLabel = l2t(truthAlignment)
      val contextMention = copyForAttributeEditMention(author)
      val payloadMention = copyForAttributeEditMention(author)
      contextMention.attr[FullName].setFullName(trueLabel)(null) //todo remove this
      payloadMention.attr[FullName].setFullName(trueLabel)(null)
      edits += new ExpAttributeEdit[AuthorEntity](contextMention,payloadMention,1.0)
      numCorrective+=1
      if(includeCorruptiveEdits){
        val mentions = author.descendantsOfClass[AuthorEntity].filter(_.isMention.booleanValue)
        val randomCanonical = mentions(random.nextInt(mentions.size))
        if(!randomCanonical.fullName.eq(trueLabel)){
          val contextMention2 = copyForAttributeEditMention(author)
          val payloadMention2 = copyForAttributeEditMention(author)
          contextMention.attr[FullName].setFullName(randomCanonical.attr[FullName])(null)
          payloadMention.attr[FullName].setFullName(randomCanonical.attr[FullName])(null)
          edits += new ExpAttributeEdit(contextMention2,payloadMention2,-1)
          numCorruptive+=1
        }
      }
    }
    println("Generated "+edits.size+" attribute edits  ("+numCorrective+" corrective, and "+numCorruptive+" corruptive) for "+authors.size+" entities.")
    edits
  }
  protected def copyForAttributeEditMention(original:AuthorEntity) = {
    val cp = new AuthorEntity(original.fullName.firstName, original.fullName.middleName, original.fullName.lastName, true)
    for(bv <- original.attr.all[BagOfWordsVariable]){
      cp.attr(bv.getClass).add(bv.value)(null)
    }
    cp.attr[FullName].setFullName(original.attr[FullName])(null)
    cp.generatedFrom = Some(original)
    cp.editSet += cp
    cp
  }

  def evaluateCanonicalAttributeAcc(author:AuthorEntity,truth:FullName):Double = {
    val predicted = pickCanonicalMention(author).attr[FullName]
    evaluateAttributeAcc(predicted,truth)
  }
  def evaluateAttributeAcc(predicted:FullName,truth:FullName):Double = {
    var score = 0.0
    //val name = pickCanonicalMention(entity).attr[FullName]
    //println("Evaluating attribute")
    //println("   predicted: "+predicted.toString)
    //println("   truth    : "+truth.toString)
    if(predicted.firstName == truth.firstName)score += 1.0
    if(predicted.middleName == truth.middleName)score += 1.0
    if(predicted.lastName == truth.lastName)score += 1.0
    score /= 3.0
    //println("  score: "+score)
    score
  }
  protected def label2truth(authors:Seq[AuthorEntity]):HashMap[String,FullName] ={
    val authorTruthCopy = authors.map((a:AuthorEntity) => copyForAttributeEditMention(a)).filter((a:AuthorEntity) => a.isEntity.booleanValue)
    val label2truth = new HashMap[String,FullName]
    for(author <- authorTruthCopy)label2truth += author.attr[BagOfTruths].map(_._1).head -> pickCanonicalMention(author).attr[FullName]
    label2truth
  }
  protected def filterAuthorsForAttributeEditing(authors:Seq[AuthorEntity]) = authors.filter((a:AuthorEntity) => {a.isEntity.booleanValue && !a.isMention.booleanValue && a.numLeaves>3 && a.attr[BagOfTruths].value.l1Norm>3 && EntityUtils.purity(a)>=0.8})

  def evaluateCompleteTrustInEditsAccuracy(authors:Seq[AuthorEntity],edits:Seq[ExpAttributeEdit[AuthorEntity]],vandalizedFirstName:String,vandalizedMiddleName:String):(Double,Double) = {
    implicit val difflist = new DiffList
    for(edit <- edits)edit.contextMention.generatedFrom.get.attr[FullName].setFullName(edit.payloadMention.attr[FullName])
    val evaluationAuthors = filterAuthorsForAttributeEditing(authors) //these will correspond with edited authors
    val accuracy = evaluateAttributeAccuracy(authors,label2truth(authors))
    val percentVandalized = percentOfAuthorsVandalized(evaluationAuthors,vandalizedFirstName,vandalizedMiddleName)
    println("done evaluating, dl size: "+difflist.size)
    difflist.undo()
    (accuracy,percentVandalized)
  }
  def evaluateInferredAccuracy(authors:Seq[AuthorEntity],vandalizedFirstName:String,vandalizedMiddleName:String,useReliabilities:Boolean=false):(Double,Double) = {
    implicit val difflist = new DiffList
    val evaluationAuthors = filterAuthorsForAttributeEditing(authors)
    for(author <- evaluationAuthors)author.attr[FullName].setFullName(pickCanonicalMention(author,useReliabilities).attr[FullName])
    val accuracy = evaluateAttributeAccuracy(evaluationAuthors,label2truth(authors))
    val percentVandalized = percentOfAuthorsVandalized(evaluationAuthors,vandalizedFirstName,vandalizedMiddleName)
    println("done evaluating, dl size: "+difflist.size)
    difflist.undo()
    (accuracy,percentVandalized)
  }
  def computeUserReliabilityForAttributeEdits(aParticularUsersEdits:Seq[ExpAttributeEdit[AuthorEntity]]):Int= {
    implicit val difflist = new DiffList
    var correct = 0
    for(edit <- aParticularUsersEdits){
      val target = edit.payloadMention.generatedFrom.get.asInstanceOf[AuthorEntity]
      if(target.attr[EntityExists].booleanValue){
        val targetName = target.attr[FullName]
        targetName.setFullName(pickCanonicalMention(target).attr[FullName])
        val name = edit.payloadMention.attr[FullName]
        val score = evaluateAttributeAcc(name,targetName)
        if(score>=0.9)correct += 1
      }
    }
    difflist.undo()
    //correct.toDouble/aParticularUsersEdits.size.toDouble
    correct
  }
  def evaluateAttributeAccuracy(authors:Seq[AuthorEntity],l2t:HashMap[String,FullName]):Double ={
    var z = 0.0
    var result = 0.0
    for(author <- authors){
      val predicted = author.attr[FullName]
      val truth = l2t(author.attr[BagOfTruths].value.iterator.maxBy(_._2)._1)
      result += evaluateAttributeAcc(predicted,truth)
      z += 1.0
    }
    result /= z
    result
  }
  def percentOfAuthorsVandalized(authors:Seq[AuthorEntity],vandalizedFirstName:String,vandalizedMiddleName:String):Double ={
    var numVandalized = 0.0
    for(name <- authors.map(_.attr[FullName])){
      if(name.firstName == vandalizedFirstName && name.middleName == vandalizedMiddleName)numVandalized += 1.0
    }
    numVandalized/authors.size.toDouble
  }


  protected def normalizedEditSimilarity(memberOfMentionsToCompare:AuthorEntity,mentions:Seq[AuthorEntity]):Double = {
    var result = 0.0
    var z=0.0
    val target = memberOfMentionsToCompare.attr[FullName].toString()
    for(m <- mentions){
      if(!m.eq(memberOfMentionsToCompare)){
        val other = m.attr[FullName].toString()
        if(other.length>0 || target.length>0)result += target.editDistance(other).toDouble/ math.max(target.size, other.size).toDouble
        if(other.length>0 && target.length>0){
          if(other.charAt(0) != target.charAt(0))result += 1.0
          z += 1.0
        }
        z += 1.0
      }
    }
    result /= z
    result = 1.0-result
    if(z==0.0)0.0 else result
  }
  def canonicalNameFactorSimulator(author:AuthorEntity,mentions:Seq[AuthorEntity],useReliability:Boolean=false):Double = {
    val name = author.attr[FullName]
    val initScore = normalizedEditSimilarity(author,mentions)
    var result = initScore //1.0
    val first = name.firstName
    val middle = name.middleName
    if(first.matches("[A-Z]\\.?"))result += 0.25
    if(middle.matches("[A-Z]\\.?"))result += 0.25
    if(first.matches("[A-Z][a-z]+"))result *= 2.0
    if(middle.matches("[A-Z][a-z]+"))result *= 2.0
    if(author.editType eq HumanEditMention.ET_ATTRIBUTE){
      result *= 2.0
      if(useReliability && author.attr[UserReliabilityVariable]!=null)result *= author.attr[UserReliabilityVariable].doubleValue
    }
    if(author.editType eq HumanEditMention.ET_ATTRIBUTE)print("  *") else print("  -")
    println("  canscore total: "+result+" nes: "+initScore+" num-mentions:"+mentions.size+" rep: "+name.toString)
    result
  }
  def pickCanonicalMention(entity:AuthorEntity,useReliability:Boolean=false):AuthorEntity ={
    val mentions = entity.descendantsOfClass[AuthorEntity].filter(_.isMention.booleanValue)
    println("Finding max of "+mentions.size+" mentions.")
    val max = mentions.maxBy((a:AuthorEntity) => canonicalNameFactorSimulator(a,mentions,useReliability))
    println("  found max: "+max.attr[FullName])
    max
  }
  def testCanonical(authors:Seq[AuthorEntity]):Unit = {
    for(a <- authors)testCanonicalization(a)
  }
  def testCanonicalization(entity:AuthorEntity):Unit ={
    val canonical = pickCanonicalMention(entity)
    val result = new StringBuffer
    val fn = entity.attr[FullName]
    result.append("\nEntityRoot["+fn.toString+"]\n")
    val mentions = entity.descendantsOfClass[AuthorEntity].filter(_.isMention.booleanValue)
    for(m <- mentions){
      if(m eq canonical)result.append("***") else result.append("  -")
      result.append("-mention:")
      result.append(m.attr[FullName].toString+"\n")
    }
    println(result.toString)
  }

  def generateContradictingEdits(edits:Seq[ExperimentalEdit[AuthorEntity]]):Seq[ExperimentalEdit[AuthorEntity]] ={
    def copy(original:AuthorEntity) = {
      val cp = new AuthorEntity(original.fullName.firstName, original.fullName.middleName, original.fullName.lastName, true)
      for(bv <- original.attr.all[BagOfWordsVariable]){
        if(!bv.isInstanceOf[BagOfTruths])
          cp.attr(bv.getClass).add(bv.value)(null)
      }
      cp.generatedFrom = original.generatedFrom
      cp.editSet += cp
      cp
    }
    val result = new ArrayBuffer[ExperimentalEdit[AuthorEntity]]
    for(edit <- edits){
      edit match{
        case e:ExpMergeEdit[AuthorEntity] => {
          val m1 = copy(e.mention1)
          val m2 = copy(e.mention2)
          result += new ExpSplitEdit[AuthorEntity](m1,m2,-e.score)
        }
        case e:ExpSplitEdit[AuthorEntity] => {
          val m1 = copy(e.mention1)
          val m2 = copy(e.mention2)
          result += new ExpMergeEdit[AuthorEntity](m1,m2,-e.score)
        }
        case _ => println("GenerateContradictingEdit not implemented for this edit type")
      }
    }
    result
  }
  def getUserNamesFromProminentAuthors(entities:Seq[AuthorEntity],mentionReq:Int):Seq[(String,Int)] ={
    val result = new ArrayBuffer[ExperimentalEdit[AuthorEntity]]
    val users = new ArrayBuffer[UserReliabilityVariable]
    val mentions = entities.filter(_.groundTruth != None)
    val truth = new HashMap[String,Int]
    for(m <- mentions)truth(m.groundTruth.get) = truth.getOrElse(m.groundTruth.get,0)+1
    val ordered = truth.toSeq.sortBy(_._2).reverse.filter(_._2>=mentionReq)
    println("Generated "+ordered.size+" users from authors (with ten or more papers).")
    ordered
  }
  def assignUnlabeledToUsers(edits:Seq[ExperimentalEdit[AuthorEntity]],userPool:Seq[String]):Unit ={
    var userCount = 0
    for(edit <- edits){
      if(edit.userName == null){
        edit.userName = userPool(userCount)
        userCount += 1;if(userCount>=userPool.size)userCount=0
      }
    }
  }
  def createUsersFromAuthorEdits(edits:Seq[ExperimentalEdit[AuthorEntity]],userNames:Seq[String]):Seq[UserReliabilityVariable] ={
    val name2user = new HashMap[String,UserReliabilityVariable]
    var numNull = 0
    for(edit <- edits){
      val name = edit.userName
      if(name!=null && !name2user.contains(name)){
        name2user(name) = new UserReliabilityVariable
      }
      if(name==null)numNull+=1
    }
    var userCount=0
    for((name,user) <- name2user){
      for(edit <- edits){
        if(edit.userName == name){
          edit.setOwner(userCount)
          edit.mentions.foreach(_.attr += user)
        }
      }
      user.truthCorrect += edits.filter(_.owner == userCount).count(_.isCorrect).toDouble
      user.truthTotal += edits.count(_.owner == userCount).toDouble
      println("User: "+name)
      println("   num edits: "+user.truthTotal+"  correct: "+user.truthCorrect)
      userCount += 1
    }
    println("Generated "+name2user.size + " from "+ edits.size + " ("+numNull+" not assigned).")
    name2user.map(_._2).toSeq
  }
  def simulateBenevolentSelfInterestedUser(entities:Seq[AuthorEntity],authorKey:String):Seq[ExperimentalEdit[AuthorEntity]] ={
    generateEditsForAuthor(entities,authorKey).filter(_.isCorrect)
  }

/*
  protected def getEditRecordsForMergeLeft(left:AuthorEntity,right:AuthorEntity):Seq[ExperimentalEdit] ={

  }
  def getSLEditRecordForMergeLeft(left:AuthorEntity,right:AuthorEntity,f:(AuthorEntity,AuthorEntity)=>Double):ExpMergeEdit[AuthorEntity] ={
    val dl = new DiffList
    val beforeScore:Double = f(left,right)
    val root = new AuthorEntity("","","",false)
    EntityUtils.linkChildToParent(yourPapers,root)(dl)
    EntityUtils.linkChildToParent(me,root)(dl)
    val score = f(left,right)-beforeScore
    dl.undo
    new ExpMergeEdit(lightcopy(yourPapers),lightcopy(me),score)
  }
  def getSNLEditRecordForSplitRight(node:AuthorEntity,f:(AuthorEntity,AuthorEntity)=>Double):ExpSplitEdit[AuthorEntity] ={
    val parent = node.parentEntity.asInstanceOf[AuthorEntity]
    val dl = new DiffList
    EntityUtils.linkChildToParent(node,null)(dl)
    val mention1 = lightcopy(parent)
    val mention2 = lightcopy(node)
    val score = f(left,right) - initialScore
    dl.undo
    new ExpSplitEdit(mention1,mention2,score)
  }
*/
  
  def simulateGreedyUser(entities:Seq[AuthorEntity],greedyUserName:String,allUserNames:Seq[String],numTargets:Int):Seq[ExperimentalEdit[AuthorEntity]] ={
    val namesSet = allUserNames.toSet
    var result = new ArrayBuffer[ExperimentalEdit[AuthorEntity]]
    val mine = allAuthorNodesWithKey(entities,greedyUserName)//.filter(_.isEntity.booleanValue)
    val me = mine.sortBy((e:AuthorEntity) => e.attr[BagOfTruths].value(greedyUserName).toDouble).reverse.head.entityRoot.asInstanceOf[AuthorEntity]
    val yours = random.shuffle(entities.filter((e:AuthorEntity) =>  e.groundTruth != None && namesSet.contains(e.groundTruth.get)))
    val mentionsOfMP = me.entityRoot.descendantsOfClass[HierEntity].filter(_.groundTruth != None)
    val targets = new HashSet[String]
    var count = 0
    while(targets.size<numTargets && count<yours.size){
      val yourPaper = yours(count)
      val you = yourPaper.entityRoot
      if(!targets.contains(you.id.toString) && you.numLeaves>=3){
        targets += you.id.toString
        val stealPapers:Boolean = true
        if(stealPapers){
          val yourPapers = sampleLineage(yourPaper)
          val dl = new DiffList
          val mentions = mentionsOfMP ++ yourPapers.entityRoot.descendantsOfClass[HierEntity].filter(_.groundTruth != None)
          val beforeScore = Evaluator.pairF1(mentions)(0)
          val root = new AuthorEntity("","","",false)
          EntityUtils.linkChildToParent(yourPapers,root)(dl)
          EntityUtils.linkChildToParent(me,root)(dl)
          val afterScore = Evaluator.pairF1(mentions)(0)
          val score = afterScore-beforeScore
          val isCorrect = score>0.0 && EntityUtils.purity(root)>=0.8 && root.attr[BagOfTruths].value.size<=2
          if(score != 0.0){
            if(isCorrect)result += new ExpMergeEdit(lightcopy(yourPapers),lightcopy(me),score)
            else if(!isCorrect && score<0)result += new ExpMergeEdit(lightcopy(yourPapers),lightcopy(me),score)
          }
          dl.undo()
        }
      }
      count += 1
    }
    result.foreach(_.userName=greedyUserName)
    result
  }
  /*
  def simulateGreedyUser(entities:Seq[AuthorEntity],authorTruthKey:String,targetKeys:Seq[String],papersToTakePerTarget:Int):Seq[ExperimentalEdit[AuthorEntity]] ={
    var result = new ArrayBuffer[ExperimentalEdit[AuthorEntity]]
    val mine = allAuthorNodesWithKey(entities,authorTruthKey)//.filter(_.isEntity.booleanValue)
    val yours = targetKeys.flatMap(allAuthorNodesWithKey(entities, _).take(papersToTakePerTarget)).filter(_.isEntity.booleanValue)
    val mostProminent = mine.sortBy((e:AuthorEntity) => e.attr[BagOfTruths].value(authorTruthKey)).reverse.head.entityRoot.asInstanceOf[AuthorEntity]
    val mentionsOfMP = mostProminent.entityRoot.descendantsOfClass[HierEntity].filter(_.groundTruth != None)
    for(node <- (yours ++ mine).filter((e:AuthorEntity) => !e.entityRoot.eq(mostProminent) && e.numLeaves>=3 && EntityUtils.purity(e)>=0.8)){
      val dl = new DiffList
      val mentions = mentionsOfMP ++ node.entityRoot.descendantsOfClass[HierEntity].filter(_.groundTruth != None)
      val beforeScore = Evaluator.pairF1(mentions)(0)
      val root = new AuthorEntity("","","",false)
      EntityUtils.linkChildToParent(node,root)(dl)
      EntityUtils.linkChildToParent(mostProminent,root)(dl)
      val afterScore = Evaluator.pairF1(mentions)(0)
      val score = afterScore-beforeScore
      val isCorrect = score>0.0 && EntityUtils.purity(root)>=0.8 //&& root.attr[BagOfTruths].value.size<=2
      if(score != 0.0){
        if(isCorrect)result += new ExpMergeEdit(lightcopy(node),lightcopy(mostProminent),score)
        else if(!isCorrect && score<0)result += new ExpMergeEdit(lightcopy(node),lightcopy(mostProminent),score)
      }
      dl.undo
    }
    result.foreach(_.userName=authorTruthKey)
    result
  }
  */
  protected def lightcopy(original:AuthorEntity) = {
      val cp = new AuthorEntity(original.fullName.firstName, original.fullName.middleName, original.fullName.lastName, true)
      for(bv <- original.attr.all[BagOfWordsVariable]){
        if(!bv.isInstanceOf[BagOfTruths]){
          val b = cp.attr(bv.getClass)
          for((word,weight) <- bv.value.iterator)b.add(word,weight/1000.0)(null)
        }
      }
      cp.generatedFrom = Some(original)
      cp.editSet += cp
      cp
    }
  def generateEditsForAuthor(entities:Seq[AuthorEntity],authorTruthKey:String):Seq[ExperimentalEdit[AuthorEntity]] ={
    var result = new ArrayBuffer[ExperimentalEdit[AuthorEntity]]
    val mine = random.shuffle(allAuthorNodesWithKey(entities,authorTruthKey))
    val me = mine.sortBy((e:AuthorEntity) => e.attr[BagOfTruths].value(authorTruthKey)).reverse.head.entityRoot.asInstanceOf[AuthorEntity]
    val mentionsOfMP = me.entityRoot.descendantsOfClass[HierEntity].filter(_.groundTruth != None)
    //should-link edits
    if(EntityUtils.purity(me)>=0.8){
      for(node <- mine.filter((e:AuthorEntity) => !e.entityRoot.eq(me) && EntityUtils.purity(e)>=0.8)){
        val dl = new DiffList
        val mentions = mentionsOfMP ++ node.entityRoot.descendantsOfClass[HierEntity].filter(_.groundTruth != None)
        val beforeScore = Evaluator.pairF1(mentions)(0)
        val root = new AuthorEntity("","","",false)
        EntityUtils.linkChildToParent(node,root)(dl)
        EntityUtils.linkChildToParent(me,root)(dl)
        val afterScore = Evaluator.pairF1(mentions)(0)
        val score = afterScore-beforeScore
        val isCorrect = score>0.0 && EntityUtils.purity(root)>=0.8 && root.attr[BagOfTruths].value.size<=2
        if(score != 0.0){
          if(isCorrect)result += new ExpMergeEdit(lightcopy(node),lightcopy(me),score)
          else if(!isCorrect && score<0)result += new ExpMergeEdit(lightcopy(node),lightcopy(me),score)
        }
        dl.undo()
      }
    }
    //should-not-link
    val initialScore = Evaluator.pairF1(mentionsOfMP)(0)
    for(node <- me.descendantsOfClass[AuthorEntity]){
      val parent = node.parentEntity.asInstanceOf[AuthorEntity]
      if(!node.eq(me) && parent != null){
        val dl = new DiffList
        EntityUtils.linkChildToParent(node,null)(dl)
        val mention1 = lightcopy(parent)
        val mention2 = lightcopy(node)
        val score = Evaluator.pairF1(mentionsOfMP)(0) - initialScore
        if(score!=0.0)result += new ExpSplitEdit(mention1,mention2,score)
        dl.undo()
      }
    }
    result.filter(_.isCorrect).foreach(_.userName=authorTruthKey)
    result
  }
  protected def sampleLineage(e:AuthorEntity):AuthorEntity = e.getAncestor(random.nextInt(e.depth+1)).asInstanceOf[AuthorEntity]
  def allAuthorNodesWithKey(entities:Seq[AuthorEntity],authorTruthKey:String):Seq[AuthorEntity] = entities.filter((e:AuthorEntity) => e.attr[BagOfTruths].value(authorTruthKey)>=1).toSeq
  def generateSubEntitySLEdits(entities:Seq[AuthorEntity],purityRequirement:Double, sizeRequirement:Int, truthSizeRequirement:Int):Seq[ExpMergeEdit[AuthorEntity]] ={
    val result = new ArrayBuffer[ExpMergeEdit[AuthorEntity]]
    val subentitySets = filteredSubEntities(entities)
    var i=0; var j=0
    println("Generating sub-entity-edits")
    var numCorrect = 0
    def copy(original:AuthorEntity) = {
      val cp = new AuthorEntity(original.fullName.firstName, original.fullName.middleName, original.fullName.lastName, true)
      for(bv <- original.attr.all[BagOfWordsVariable]){
        if(!bv.isInstanceOf[BagOfTruths]){
          val b = cp.attr(bv.getClass)
          for((word,weight) <- bv.value.iterator)b.add(word,weight/1000.0)(null)
        }
      }
      cp.generatedFrom = Some(original)
      cp.editSet += cp
      cp
    }
    val initialScore = Evaluator.pairF1(entities)
    println("  real score: "+initialScore.mkString(" "))
    while(i<subentitySets.size){
      val iSESet = subentitySets(i)
      j = i+1
      while(j<subentitySets.size){
        val jSESet = subentitySets(j)
        for(isub <- iSESet){
          for(jsub <- jSESet){
            val mentions = (isub.entityRoot.descendantsOfClass[HierEntity]++jsub.entityRoot.descendantsOfClass[HierEntity]).filter(_.groundTruth != None)
            if(mentions.size>=sizeRequirement){
              val dl = new DiffList
              val beforeScores = Evaluator.pairF1(mentions)
              val beforeScore = beforeScores(0)
              val root = new AuthorEntity("","","",false)
              EntityUtils.linkChildToParent(isub,root)(dl)
              EntityUtils.linkChildToParent(jsub,root)(dl)
              val afterScores = Evaluator.pairF1(mentions)
              val afterScore = afterScores(0)
              val score = afterScore-beforeScore
              val isCorrect = score>0.0 && EntityUtils.purity(root)>=0.8 && root.attr[BagOfTruths].value.size<=2
              if(score != 0.0){
                if(isCorrect){
                  numCorrect += 1
                  result += new ExpMergeEdit(copy(isub),copy(jsub),score)
                }
                else if(!isCorrect && score<0)result += new ExpMergeEdit(copy(isub),copy(jsub),score)
                //else ignore
              }
              dl.undo()
            }
          }
        }
        j += 1
      }
      i+=1
    }
    println("Found "+result.size + " total edits and "+numCorrect+" correct edits.")
    result
  }

  def generateSubEntitySLEditsOLD(entities:Seq[AuthorEntity],purityRequirement:Double, sizeRequirement:Int, truthSizeRequirement:Int):Seq[ExpMergeEdit[AuthorEntity]] ={
    val result = new ArrayBuffer[ExpMergeEdit[AuthorEntity]]
    val subentitySets = filteredSubEntities(entities)
    var i=0; var j=0
    println("Generating sub-entity-edits")
    var numCorrect = 0
    def copy(original:AuthorEntity) = {
      val cp = new AuthorEntity(original.fullName.firstName, original.fullName.middleName, original.fullName.lastName, true)
      for(bv <- original.attr.all[BagOfWordsVariable]){
        if(!bv.isInstanceOf[BagOfTruths])
          cp.attr(bv.getClass).add(bv.value)(null)
      }
      cp.generatedFrom = Some(original)
      cp.editSet += cp
      cp
    }
    val initialScore = Evaluator.pairF1(entities)
    println("  real score: "+initialScore.mkString(" "))
    while(i<subentitySets.size){
      val iSESet = subentitySets(i)
      j = i+1
      while(j<subentitySets.size){
        val jSESet = subentitySets(j)
        for(isub <- iSESet){
          for(jsub <- jSESet){
            val mentions = (isub.entityRoot.descendantsOfClass[HierEntity]++jsub.entityRoot.descendantsOfClass[HierEntity]).filter(_.isMention.booleanValue)
            if(mentions.size>=sizeRequirement){
              val dl = new DiffList
              val beforeScores = Evaluator.pairF1(mentions)
              val beforeScore = beforeScores(0)
              val root = new AuthorEntity("","","",false)
              EntityUtils.linkChildToParent(isub,root)(dl)
              EntityUtils.linkChildToParent(jsub,root)(dl)
              val afterScores = Evaluator.pairF1(mentions)
              val afterScore = afterScores(0)
              val score = afterScore-beforeScore
              if(EntityUtils.purity(root)>=purityRequirement && root.attr[BagOfTruths].value.size<=truthSizeRequirement){
                var correct = if(afterScore>beforeScore) "correct" else "incorrect"
                if(afterScore>beforeScore)numCorrect += 1
                /*
                if(afterScore>beforeScore){
                  println("before score: "+beforeScore+" after score: "+afterScore)
                  //val newf1score = Evaluator.pairF1(entities)
                  //println("  real score: "+newf1score.mkString(" "))
                  //println("  f1 improvement: "+(newf1score(0)-initialScore(0)))
                }
                println("Found "+correct +"("+(afterScore-beforeScore)+")"+" edit of size "+mentions.size+" with purity "+EntityUtils.purity(root)+" and truths: "+root.attr[BagOfTruths])
                */
                if(afterScore-beforeScore != 0.0)result += new ExpMergeEdit(copy(isub),copy(jsub),afterScore-beforeScore)
              }
              dl.undo()
            }
          }
        }
        j += 1
      }
      i+=1
    }
    println("Found "+result.size + " total edits and "+numCorrect+" correct edits.")
    result
  }
  def filteredSubEntities(entities:Seq[AuthorEntity]):ArrayBuffer[Seq[AuthorEntity]] = {
    val result = new ArrayBuffer[Seq[AuthorEntity]]
    for(e<-entities.filter(_.isEntity.booleanValue))result += filteredSubEntities(e)
    println("Found "+result.size + " matching sub entities for "+entities.size + " entities.")
    for(subs<-result){
      if(subs.size>0){
        println("  Found "+subs.size + " matching sub entities.")
        for(s <- subs)println("  "+s.attr[BagOfTruths].value.iterator.map(_._2).toSeq.mkString(" "))
      }
    }
    result
  }
  def filteredSubEntities(e:AuthorEntity):Seq[AuthorEntity] = {
    val result = e.descendantsOfClass[AuthorEntity].filter((s:AuthorEntity) => {
      (s.numLeaves>=2 && EntityUtils.purity(s)>=0.8) || (s.numLeaves>=3 && s.attr[BagOfTruths].value.size<=2)
    })
    result.toSeq
  }

  def getAuthorEdits(initialDB:Seq[AuthorEntity],minimumEntitySize:Int,minPurity:Double=0.0):Seq[ExpMergeEdit[AuthorEntity]] ={
    val edits = allMergeEdits[AuthorEntity](
      initialDB,
      _ => new AuthorEntity,
      (entities:Seq[HierEntity]) => {Evaluator.pairF1(entities).head},
      (original:AuthorEntity) => {
        val cp = new AuthorEntity(original.fullName.firstName, original.fullName.middleName, original.fullName.lastName, true)
        for(bv <- original.attr.all[BagOfWordsVariable]){
          if(!bv.isInstanceOf[BagOfTruths])
            cp.attr(bv.getClass).add(bv.value)(null)
        }
        cp.generatedFrom = Some(original)
        cp.editSet += cp
        cp
      },
      minimumEntitySize,
      minPurity
    )
    edits
  }
  def applySplitEdits(evidenceBatches:Seq[Seq[AuthorEntity]])(implicit d:DiffList):Unit ={
    for(batch <- evidenceBatches){
      val visited = new HashSet[AuthorEntity]
      for(edit <- batch){
        val linked = edit.linkedMention.get.asInstanceOf[AuthorEntity]
        if(!visited.contains(edit)){
          visited += edit
          visited += linked
          applySplitEdit(edit,true)(d)
        }
      }
    }
  }
  private def structurePreservationForEntityThatLostChild(e:Entity)(implicit d:DiffList):Unit ={
    if(e!=null && e.childEntitiesSize<=1){
      for(childEntity <- e.childEntities)
        childEntity.setParentEntity(e.parentEntity)
      e.setParentEntity(null)(d)
    }
  }
  def applySplitEdit(edit:AuthorEntity,preserveStructure:Boolean=false)(implicit d:DiffList){
    val linked = edit.linkedMention.get.asInstanceOf[AuthorEntity]
    val editSource = edit.generatedFrom.get.asInstanceOf[AuthorEntity]
    val linkedSource = linked.generatedFrom.get.asInstanceOf[AuthorEntity]
    if(editSource.parentEntity eq linkedSource){
      EntityUtils.linkChildToParent(editSource,null)(d)
      if(preserveStructure)structurePreservationForEntityThatLostChild(linkedSource)(d) //linkedSource is the parent
    }
    else if(linkedSource.parentEntity eq editSource){
      EntityUtils.linkChildToParent(linkedSource,null)(d)
      if(preserveStructure)structurePreservationForEntityThatLostChild(editSource)(d) //editSource is the parent
    }
    else println("Warning: edit not a valid split point.")
  }
  def getExperimentPrintWriter(file:File):PrintWriter = {
    val pw = new PrintWriter(file)
    pw.println("time samples accepted f1 p r batch-count mentions entities batch-name score maxscore")
    pw
  }
  def splitBaseline1(initialDB:Seq[AuthorEntity],evidenceBatches:Seq[Seq[AuthorEntity]],file:File):Unit ={
    val pwbl1 = new PrintWriter(file)
    val d = new DiffList
    var batchName = 0
    val toScore = new ArrayBuffer[AuthorEntity]
    toScore ++= initialDB
    var entityCount = toScore.count(_.isEntity.booleanValue)
    val mentionCount = toScore.size
    pwbl1.println("time samples accepted f1 p r batch-count mentions entities batch-name score maxscore")
    pwbl1.println("-1 -1 -1 "+Evaluator.pairF1(toScore).mkString(" ")+" "+0+" "+mentionCount+" -1 "+batchName+" -1 -1")
    for(batch <- evidenceBatches){
      batchName += 1
      val visited = new HashSet[AuthorEntity]
      for(edit <- batch){
        if(!visited.contains(edit)){
          val linked = edit.linkedMention.get.asInstanceOf[AuthorEntity]
          /*
          val editSource = edit.generatedFrom.get.asInstanceOf[AuthorEntity]
          val linkedSource = linked.generatedFrom.get.asInstanceOf[AuthorEntity]
          if(editSource.parentEntity eq linkedSource)EntityUtils.linkChildToParent(editSource,null)(d)
          else if(linkedSource.parentEntity eq editSource) EntityUtils.linkChildToParent(linkedSource,null)(d)
          else println("Warning: edit not a valid split point.")
          */
          visited += edit
          visited += linked
          entityCount += 1
          applySplitEdit(edit)(d)
        }
      }
      val scores = Evaluator.pairF1(toScore)
      pwbl1.println("-1 -1 -1 "+scores.mkString(" ")+" "+batchName+" "+mentionCount+" "+entityCount+" "+batchName+" -1 -1")
      pwbl1.flush()
    }
    d.undo();d.clear();pwbl1.close()
  }
  protected def splitBaseline1(edit:AuthorEntity, d:DiffList):Unit ={
    val linked = edit.linkedMention.get.asInstanceOf[AuthorEntity]
    val editSource = edit.generatedFrom.get.asInstanceOf[AuthorEntity]
    val linkedSource = linked.generatedFrom.get.asInstanceOf[AuthorEntity]
    if(editSource.parentEntity eq linkedSource)EntityUtils.linkChildToParent(editSource,null)(d)
    else if(linkedSource.parentEntity eq editSource) EntityUtils.linkChildToParent(linkedSource,null)(d)
    //else println("Warning: edit not a valid split point.")
  }

  def baseline1(initialDB:Seq[AuthorEntity],evidenceBatches:Seq[Seq[AuthorEntity]],pwbl:PrintWriter):Unit ={
    println("Baseline 1: "+evidenceBatches.size+" evidence batches.")
    //val pwbl = new PrintWriter(file)
    val d = new DiffList
    var batchName = 0
    val toScore = new ArrayBuffer[AuthorEntity]
    toScore ++= initialDB
    var entityCount = toScore.count(_.isEntity.booleanValue)
    val mentionCount = toScore.size
    //pwbl.println("time samples accepted f1 p r batch-count mentions entities batch-name score maxscore")
    pwbl.println("-1 -1 -1 "+Evaluator.pairF1(toScore).mkString(" ")+" "+0+" "+mentionCount+" -1 "+batchName+" -1 -1")
    for(batch <- evidenceBatches){
      batchName += 1
      val visited = new HashSet[AuthorEntity]
      //println("  batch size: "+batch.size)
      for(edit <- batch){
        //println("    Processing edit that should improve sore by "+edit.memberOfEdit.get.score+" of type: "+edit.editType)
        if(!visited.contains(edit)){
          visited += edit
          visited += edit.linkedMention.get.asInstanceOf[AuthorEntity]
          entityCount -= 1
          edit.editType match{
            case HumanEditMention.ET_SHOULD_LINK => mergeBaseline1(edit,d)
            case HumanEditMention.ET_SHOULD_NOT_LINK => splitBaseline1(edit,d)
            case _ => {}//{throw new Exception("Not implemented for this edit type")}
          }
        }
      }
      val scores = Evaluator.pairF1(toScore)
      pwbl.println("-1 -1 -1 "+scores.mkString(" ")+" "+batchName+" "+mentionCount+" "+entityCount+" "+batchName+" -1 -1")
      //println("scores: "+Evaluator.pairF1(toScore).mkString(" "))
      pwbl.flush()
    }
    d.undo();d.clear()//;pwbl.close
  }
  def baseline2(initialDB:Seq[AuthorEntity],evidenceBatches:Seq[Seq[AuthorEntity]],pwbl:PrintWriter):Unit ={
    //val pwbl = new PrintWriter(file)
    val d = new DiffList
    var batchName = 0
    val toScore = new ArrayBuffer[AuthorEntity]
    toScore ++= initialDB
    var entityCount = toScore.count(_.isEntity.booleanValue)
    val mentionCount = toScore.size
    //pwbl.println("time samples accepted f1 p r batch-count mentions entities batch-name score maxscore")
    pwbl.println("-1 -1 -1 "+Evaluator.pairF1(toScore).mkString(" ")+" "+0+" "+mentionCount+" -1 "+batchName+" -1 -1")
    for(batch <- evidenceBatches){
      batchName += 1
      val visited = new HashSet[AuthorEntity]
      for(edit <- batch){
        //println("Processing edit that should improve sore by "+edit.memberOfEdit.get.score)
        if(!visited.contains(edit)){
          visited += edit
          visited += edit.linkedMention.get.asInstanceOf[AuthorEntity]
          entityCount -= 1
          edit.editType match{
            case HumanEditMention.ET_SHOULD_LINK => mergeBaseline2(edit,d)
            case HumanEditMention.ET_SHOULD_NOT_LINK => splitBaseline1(edit,d)
            case _ => {}//{throw new Exception("Not implemented for this edit type")}
          }
        }
      }
      val scores = Evaluator.pairF1(toScore)
      pwbl.println("-1 -1 -1 "+scores.mkString(" ")+" "+batchName+" "+mentionCount+" "+entityCount+" "+batchName+" -1 -1")
      println("scores: "+Evaluator.pairF1(toScore).mkString(" "))
      pwbl.flush()
    }
    d.undo();d.clear()//;pwbl.close
  }
  def mergeBaseline1(initialDB:Seq[AuthorEntity],evidenceBatches:Seq[Seq[AuthorEntity]],file:File):Unit ={
    val pwbl1 = new PrintWriter(file)
    val d = new DiffList
    var batchName = 0
    val toScore = new ArrayBuffer[AuthorEntity]
    toScore ++= initialDB
    var entityCount = toScore.count(_.isEntity.booleanValue)
    val mentionCount = toScore.size
    pwbl1.println("time samples accepted f1 p r batch-count mentions entities batch-name score maxscore")
    pwbl1.println("-1 -1 -1 "+Evaluator.pairF1(toScore).mkString(" ")+" "+0+" "+mentionCount+" -1 "+batchName+" -1 -1")
    for(batch <- evidenceBatches){
      batchName += 1
      val visited = new HashSet[AuthorEntity]
      for(edit <- batch){
        println("Processing edit that should improve sore by "+edit.memberOfEdit.get.score)
        if(!visited.contains(edit)){
          visited += edit
          visited += edit.linkedMention.get.asInstanceOf[AuthorEntity]
          entityCount -= 1
          mergeBaseline1(edit,d)
          /*
          val parent = new AuthorEntity
          EntityUtils.linkChildToParent(edit.generatedFrom.get,parent)(d)
          EntityUtils.linkChildToParent(edit.linkedMention.get.asInstanceOf[AuthorEntity].generatedFrom.get,parent)(d)
          toScore += parent
          */
        }
      }
      val scores = Evaluator.pairF1(toScore)
      pwbl1.println("-1 -1 -1 "+scores.mkString(" ")+" "+batchName+" "+mentionCount+" "+entityCount+" "+batchName+" -1 -1")
      println("scores: "+Evaluator.pairF1(toScore).mkString(" "))
      pwbl1.flush()
    }
    d.undo();d.clear();pwbl1.close()
  }
  protected def mergeBaseline1(edit:AuthorEntity, d:DiffList):Unit ={
    val parent = new AuthorEntity
    EntityUtils.linkChildToParent(edit.generatedFrom.get,parent)(d)
    EntityUtils.linkChildToParent(edit.linkedMention.get.asInstanceOf[AuthorEntity].generatedFrom.get,parent)(d)
  }
  def mergeBaseline2(initialDB:Seq[AuthorEntity],evidenceBatches:Seq[Seq[AuthorEntity]],file:File):Unit ={
    val d2 = new DiffList
    val pwbl2 = new PrintWriter(file)
    val toScore = new ArrayBuffer[AuthorEntity]
    val mentionCount = toScore.size
    var batchName = 0
    toScore ++= initialDB
    var entityCount = toScore.count(_.isEntity.booleanValue)
    pwbl2.println("time samples accepted f1 p r batch-count mentions entities batch-name score maxscore")
    pwbl2.println("-1 -1 -1 "+Evaluator.pairF1(toScore).mkString(" ")+" "+0+" "+mentionCount+" -1 "+batchName+" -1 -1")
    for(batch <- evidenceBatches){
      batchName += 1
      val visited = new HashSet[AuthorEntity]
      for(edit <- batch){
        if(!visited.contains(edit)){// && !edit.generatedFrom.get.entityRoot.eq(edit.linkedMention.get.asInstanceOf[AuthorEntity].generatedFrom.get.entityRoot)){
          visited += edit
          visited += edit.linkedMention.get.asInstanceOf[AuthorEntity]
          entityCount -= 1
          mergeBaseline2(edit,d2)
          /*
          val parent = new AuthorEntity
          val epar1 = edit.generatedFrom.get.parentEntity
          val epar2 = edit.linkedMention.get.asInstanceOf[AuthorEntity].generatedFrom.get.parentEntity
          if(epar1==null && epar2==null){
            EntityUtils.linkChildToParent(edit.generatedFrom.get,parent)(d2)
            EntityUtils.linkChildToParent(edit.linkedMention.get.asInstanceOf[AuthorEntity].generatedFrom.get,parent)(d2)
            toScore += parent
          } else if(epar1 != null && epar2 == null){
            EntityUtils.linkChildToParent(edit.linkedMention.get.asInstanceOf[AuthorEntity].generatedFrom.get,epar1)(d2)
          } else if(epar2 != null && epar1 == null){
            EntityUtils.linkChildToParent(edit.generatedFrom.get,epar2)(d2)
          } else if(!edit.generatedFrom.get.entityRoot.eq(edit.linkedMention.get.asInstanceOf[AuthorEntity].generatedFrom.get.entityRoot)){
            EntityUtils.linkChildToParent(edit.generatedFrom.get.entityRoot.asInstanceOf[AuthorEntity],parent)(d2)
            EntityUtils.linkChildToParent(edit.linkedMention.get.asInstanceOf[AuthorEntity].generatedFrom.get.entityRoot.asInstanceOf[AuthorEntity],parent)(d2)
          }
          */
        }
      }
      //entityCount = toScore.filter(_.isEntity.booleanValue).size
      //time samples accepted f1 p r batch-count mentions entities batch-name score maxscore
      val scores = Evaluator.pairF1(toScore)
      pwbl2.println("-1 -1 -1 "+scores.mkString(" ")+" "+batchName+" "+mentionCount+" "+entityCount+" "+batchName+" -1 -1")
      pwbl2.flush()
    }
    d2.undo();d2.clear();pwbl2.close()
  }
  protected def mergeBaseline2(edit:AuthorEntity, d2:DiffList):Unit ={
    if(!edit.generatedFrom.get.entityRoot.eq(edit.linkedMention.get.asInstanceOf[AuthorEntity].generatedFrom.get.entityRoot)){
      //visited += edit
      val parent = new AuthorEntity
      val epar1 = edit.generatedFrom.get.parentEntity
      val epar2 = edit.linkedMention.get.asInstanceOf[AuthorEntity].generatedFrom.get.parentEntity
      if(epar1==null && epar2==null){
        EntityUtils.linkChildToParent(edit.generatedFrom.get,parent)(d2)
        EntityUtils.linkChildToParent(edit.linkedMention.get.asInstanceOf[AuthorEntity].generatedFrom.get,parent)(d2)
        //toScore += parent
      } else if(epar1 != null && epar2 == null){
        EntityUtils.linkChildToParent(edit.linkedMention.get.asInstanceOf[AuthorEntity].generatedFrom.get,epar1)(d2)
      } else if(epar2 != null && epar1 == null){
        EntityUtils.linkChildToParent(edit.generatedFrom.get,epar2)(d2)
      } else if(!edit.generatedFrom.get.entityRoot.eq(edit.linkedMention.get.asInstanceOf[AuthorEntity].generatedFrom.get.entityRoot)){
        EntityUtils.linkChildToParent(edit.generatedFrom.get.entityRoot.asInstanceOf[AuthorEntity],parent)(d2)
        EntityUtils.linkChildToParent(edit.linkedMention.get.asInstanceOf[AuthorEntity].generatedFrom.get.entityRoot.asInstanceOf[AuthorEntity],parent)(d2)
      }
    }
  }

  def correctAuthorSplitEdits(initialDB:Seq[AuthorEntity]):Seq[ExpSplitEdit[AuthorEntity]] ={
    val edits = correctSplitEdits[AuthorEntity](
      initialDB,
      _ => new AuthorEntity,
      (entities:Seq[HierEntity]) => {Evaluator.pairF1(entities).head},
      (original:AuthorEntity) => {
        val cp = new AuthorEntity(original.fullName.firstName, original.fullName.middleName, original.fullName.lastName, true)
        for(bv <- original.attr.all[BagOfWordsVariable]){
          //if(!bv.isInstanceOf[BagOfTruths])
            cp.attr(bv.getClass).add(bv.value)(null)
        }
        cp.generatedFrom = Some(original)
        cp.editSet += cp
        cp
      }
    )
    edits
  }
  def incorrectAuthorSplitEdits(initialDB:Seq[AuthorEntity],editsPerEntity:Int):Seq[ExpSplitEdit[AuthorEntity]] ={
    val edits = incorrectSplitEdits[AuthorEntity](
      initialDB,
      _ => new AuthorEntity,
      (entities:Seq[HierEntity]) => {Evaluator.pairF1(entities).head},
      (original:AuthorEntity) => {
        val cp = new AuthorEntity(original.fullName.firstName, original.fullName.middleName, original.fullName.lastName, true)
        for(bv <- original.attr.all[BagOfWordsVariable]){
          //if(!bv.isInstanceOf[BagOfTruths])
            cp.attr(bv.getClass).add(bv.value)(null)
        }
        cp.generatedFrom = Some(original)
        cp.editSet += cp
        cp
      },
      editsPerEntity
    )
    edits
  }
  def incorrectSplitEdits[T<:HierEntity with HumanEditMention](allEntities:Seq[T],newEntity:Unit=>T,scoreFunction:Seq[HierEntity]=>Double,createEditMentionFrom:T=>T, editsPerEntity:Int):Seq[ExpSplitEdit[T]] ={
    val result = new ArrayBuffer[ExpSplitEdit[T]]
    val entities = allEntities.filter((e:T) => e.isEntity.booleanValue)
    var i = 0
    while(i<entities.size){
      //val (splitPoint,score) = findIncorrectSplit(entities(i),scoreFunction)
      val splits = new ArrayBuffer[(T,Double)]
      //if(onePerEntity)splits ++= findRandomIncorrectSplit(entities(i),scoreFunction,editsPerEntity)
      //else splits ++= findIncorrectSplits(entities(i),scoreFunction)
      if(editsPerEntity>0)splits ++= findRandomIncorrectSplit(entities(i),scoreFunction,editsPerEntity)
      else if(editsPerEntity== -1)splits += findMostIncorrectSplit(entities(i),scoreFunction)
      for((splitPoint,score) <- splits){
        if(splitPoint!=null){
          println("Found split point with score"+score)
          if(splitPoint.isInstanceOf[AuthorEntity])EntityUtils.prettyPrintAuthor(splitPoint.asInstanceOf[AuthorEntity])
          println("split-point ids:")
          //println("found split point")
          val d = new DiffList
          val parent = splitPoint.parentEntity.asInstanceOf[T]
          //splitPoint.setParentEntity(null)(d)
          EntityUtils.linkChildToParent(splitPoint,null)(d)
          val mention1 = createEditMentionFrom(parent)
          val mention2 = createEditMentionFrom(splitPoint)
          println("   **split-point parent "+parent.id+" generated "+mention1.id)
          println("   **split-point child  "+splitPoint.id+" generated "+mention2.id)
          result += new ExpSplitEdit(mention1,mention2,score)
          d.undo()
        }
      }
      i += 1
      println("processed "+i + " of "+entities.size)
    }
    result
  }
  /*
  def correctSplitEdits[T<:HierEntity with HumanEditMention](allEntities:Seq[T],newEntity:Unit=>T,scoreFunction:Seq[HierEntity]=>Double,createEditMentionFrom:T=>T):Seq[ExpSplitEdit[T]] ={
    val result = new ArrayBuffer[ExpSplitEdit[T]]
    val entities = allEntities.filter((e:T) => e.isEntity.booleanValue)
    var i = 0
    while(i<entities.size){
      val (splitPoint,score) = findBestSplit(entities(i),scoreFunction)
      if(splitPoint!=null){
        println("Found split point with score"+score)
        if(splitPoint.isInstanceOf[AuthorEntity])EntityUtils.prettyPrintAuthor(splitPoint.asInstanceOf[AuthorEntity])
        println("split-point ids:")
        //println("found split point")
        val d = new DiffList
        val parent = splitPoint.parentEntity.asInstanceOf[T]
        //splitPoint.setParentEntity(null)(d)
        EntityUtils.linkChildToParent(splitPoint,null)(d)
        val mention1 = createEditMentionFrom(parent)
        val mention2 = createEditMentionFrom(splitPoint)
        println("   **split-point parent "+parent.id+" generated "+mention1.id)
        println("   **split-point child  "+splitPoint.id+" generated "+mention2.id)
        result += new ExpSplitEdit(mention1,mention2,score)
        d.undo
      }
      i += 1
      println("processed "+i + " of "+entities.size)
    }
    result
  }
  */
  def correctSplitEdits[T<:HierEntity with HumanEditMention](allEntities:Seq[T],newEntity:Unit=>T,scoreFunction:Seq[HierEntity]=>Double,createEditMentionFrom:T=>T):Seq[ExpSplitEdit[T]] ={
    val result = new ArrayBuffer[ExpSplitEdit[T]]
    val entities = allEntities.filter((e:T) => e.isEntity.booleanValue)
    var i = 0
    while(i<entities.size){
      //val (splitPoint,score) = findBestSplit(entities(i),scoreFunction)
      val splitPoints = findBestSplits(entities(i),scoreFunction)
      val d = new DiffList
      val parents = new ArrayBuffer[T]
      for((splitPoint,score) <- splitPoints){
        parents += splitPoint.parentEntity.asInstanceOf[T]
        EntityUtils.linkChildToParent(splitPoint,null)(d)
      }
      var spCount = 0
      for((splitPoint,score) <- splitPoints){
        val parent = parents(spCount)
        val mention1 = createEditMentionFrom(parent)
        val mention2 = createEditMentionFrom(splitPoint)
        result += new ExpSplitEdit(mention1,mention2,score)
        spCount += 1
      }
      i += 1
      println("processed "+i + " of "+entities.size)
      d.undo()
    }
    result
  }
  def findMostIncorrectSplit[T<:HierEntity with HumanEditMention](e:T,scoreFunction:Seq[HierEntity]=>Double):(T,Double) ={
    val incorrectSplits = findIncorrectSplits(e,scoreFunction)
    var worstScore = 0.0
    var worstSplit:T = null.asInstanceOf[T]
    for((split,score) <- incorrectSplits){
      if(score<worstScore){
        worstScore=score
        worstSplit=split
      }
    }
    (worstSplit,worstScore)
  }
  def findRandomIncorrectSplit[T<:HierEntity with HumanEditMention](e:T,scoreFunction:Seq[HierEntity]=>Double,editsPerEntity:Int):Seq[(T,Double)] ={
    val incorrectSplits = findIncorrectSplits(e,scoreFunction)
    random.shuffle(incorrectSplits).take(editsPerEntity)
  }
  def findIncorrectSplits[T<:HierEntity with HumanEditMention](e:T,scoreFunction:Seq[HierEntity]=>Double):Seq[(T,Double)] ={
    val root = e.entityRoot.asInstanceOf[T]
    var entity = e
    val scoringMentions = root.descendantsOfClass[HierEntity].filter(_.isMention.booleanValue)
    var candidates = new ArrayBuffer[T]
    var scores = new ArrayBuffer[Double]
    val initScore = scoreFunction(scoringMentions)
    val added = new HashSet[T]
    for(desc <- root.descendantsOfClass[HierEntity].filter(_.isMention.booleanValue)){
      entity = desc.asInstanceOf[T]
      while(entity != null){
        val d = new DiffList
        EntityUtils.linkChildToParent(entity,null)(d)
        val score = scoreFunction(scoringMentions)
        if(score<initScore && !added.contains(entity)){
          added += entity
          candidates += entity
          scores += (score - initScore)
        }
        d.undo()
        entity = entity.parentEntity.asInstanceOf[T]
      }
    }
    if(candidates.size==0) Seq[(T,Double)]()//(null.asInstanceOf[T],0.0)
    else {
      //val rid = random.nextInt(candidates.size)
      //(candidates(rid),scores(rid))
      println("Found incorrect split edits for entity")
      println("  num mentions: "+scoringMentions.size)
      println("  num nodes: "+root.descendantsOfClass[HierEntity].size)
      println("  num splits: "+candidates.size)
      candidates zip scores
    }
  }
  def findBestSplits[T<:HierEntity with HumanEditMention](e:T,scoreFunction:Seq[HierEntity]=>Double):Seq[(T,Double)] ={
    val root = e.entityRoot.asInstanceOf[T]
    var entity = e
    val scoringMentions = root.descendantsOfClass[HierEntity].filter(_.isMention.booleanValue)
    var initScore = scoreFunction(scoringMentions)//EntityUtils.purity(root)
    val result = new HashMap[T,Double]
    //println("Finding split point for entity with score: "+bestScore)
    //var result:T = null.asInstanceOf[T]
    for(desc <- root.descendantsOfClass[HierEntity].filter(_.isMention.booleanValue)){
      entity = desc.asInstanceOf[T]
      while(entity != null){
        val d = new DiffList
        EntityUtils.linkChildToParent(entity,null)(d)
        val score = scoreFunction(scoringMentions)//EntityUtils.purity(root)
        //println("  (score bestscore): "+score+" "+bestScore)
        if(score>initScore){
          result += entity->score
          //bestScore = score
          //result = entity
        }
        d.undo()
        entity = entity.parentEntity.asInstanceOf[T]
      }
    }
    result.toSeq
  }

  def allMergeEdits[T<:HierEntity with HumanEditMention](allEntities:Seq[T],newEntity:Unit=>T,scoreFunction:Seq[HierEntity]=>Double,createEditMentionFrom:T=>T,minESize:Int,minPurity:Double=0.0):Seq[ExpMergeEdit[T]] ={
    val result = new ArrayBuffer[ExpMergeEdit[T]]
    val entities = allEntities.filter((e:T) =>{e.isEntity.booleanValue && e.numLeaves>=minESize && EntityUtils.purity(e)>=minPurity})
    println("About to create edits from "+ entities.size + " entities.")
    var i = 0;var j = 0
    while(i<entities.size){
      j = i+1
      val ei = entities(i)
      val eiMentions = ei.descendantsOfClass[HierEntity]
      while(j<entities.size){
        val ej = entities(j)
        val ejMentions = ej.descendantsOfClass[HierEntity]
        val originalScore = scoreFunction(eiMentions ++ ejMentions)
        val tmpRoot = newEntity()
        val d = new DiffList
        //see what happens if we were to merge these two entities together
        EntityUtils.linkChildToParent(ei,tmpRoot)(d)
        EntityUtils.linkChildToParent(ej,tmpRoot)(d)
        val editScore = scoreFunction(eiMentions ++ ejMentions ++ Seq(tmpRoot.asInstanceOf[HierEntity]))
        //undo the merge
        d.undo()
        assert(ei.parentEntity == null)
        assert(ej.parentEntity == null)
        //package the finding
        result += new ExpMergeEdit(createEditMentionFrom(ei),createEditMentionFrom(ej),editScore-originalScore)
        j+=1
      }
      i+=1
      print(".")
      if(i % 25 == 0) println()
    }
    val numGood = result.count(_.isCorrect)
    val numBad = result.size - numGood
    println("Generated "+ result.size + " edits ("+ numGood + " correct edits. " + numBad + " incorrectEdits).")
    result
  }
}

object ExperimentsEditTracker{
  import HumanEditExperiments._
  def numEdits=numSNLEdits+numSLEdits
  var numSLEdits=0
  var numSNLEdits=0
  def numSNLSatisfied=correctSNLSatisfied+incorrectSNLSatisfied
  var correctSNLSatisfied=0
  var incorrectSNLSatisfied=0
  var correctSNLIgnored=0
  var incorrectSNLIgnored=0
  def numSLSatisfied=correctSLSatisfied+incorrectSLSatisfied
  var correctSLSatisfied=0
  var incorrectSLSatisfied=0
  var correctSLIgnored=0
  var incorrectSLIgnored=0

  protected var _slpw:PrintWriter=null
  protected var _snlpw:PrintWriter=null
  protected var _allpw:PrintWriter=null

  def reset(): Unit = {
    numSLEdits=0
    numSNLEdits=0
    correctSNLSatisfied=0
    incorrectSNLSatisfied=0
    correctSNLIgnored=0
    incorrectSNLIgnored=0
    correctSLSatisfied=0
    incorrectSLSatisfied=0
    correctSLIgnored=0
    incorrectSLIgnored=0
  }
  def beginStats(fileName:String):Unit ={
    println("EDIT BASE FILE NAME: "+fileName)
    val schema = "total correct incorrect satisfied ignored pct-correct pct-incorrect pct-satisfied pct-ignored pct-correct-satisfied pct-incorrect-satisfied pct-correct-ignored pct-incorrect-ignored"
    println("EDIT STATS SCHEMA: "+schema)
    _slpw = new PrintWriter(fileName+".sl")
    _snlpw = new PrintWriter(fileName+".snl")
    _allpw = new PrintWriter(fileName+".all")
    _slpw.println(schema);_slpw.flush()
    _snlpw.println(schema);_snlpw.flush()
    _allpw.println(schema);_allpw.flush()
  }
  def endStats(): Unit = {
    _slpw.close()
    _snlpw.close()
    _allpw.close()
  }
  def computeAndPrintStats[T<:HierEntity with HumanEditMention](stats:Iterable[ExperimentalEdit[T]]):Unit ={
    computeStats(stats)
    printStats(_slpw,"SLEDITS",correctSLSatisfied,incorrectSLSatisfied,correctSLIgnored,incorrectSLIgnored,numSLEdits)
    printStats(_snlpw,"SNLEDITS",correctSNLSatisfied,incorrectSNLSatisfied,correctSNLIgnored,incorrectSNLIgnored,numSNLEdits)
    printStats(_allpw,"ALLEDITS",correctSLSatisfied+correctSNLSatisfied,incorrectSLSatisfied+incorrectSNLSatisfied,correctSLIgnored+correctSNLIgnored,incorrectSLIgnored+incorrectSNLIgnored, numSLEdits + numSNLEdits)
  }
  def computeStats[T<:HierEntity with HumanEditMention](edits:Iterable[ExperimentalEdit[T]]):Unit ={
    reset()
    println("COMPUTE STATS: num edits = "+edits.size)
    for(edit <- edits)updateStatistics(edit)
  }
  def printStats(pw:PrintWriter,prefix:String,correctSatisfied:Int,incorrectSatisfied:Int,correctIgnored:Int,incorrectIgnored:Int,total:Int):Unit ={
    println("EDITS STATS")
    val correctTotal = correctSatisfied + correctIgnored
    val incorrectTotal = incorrectSatisfied + incorrectIgnored
    val satisfiedTotal = correctSatisfied + incorrectSatisfied
    val ignoredTotal = correctIgnored + incorrectIgnored
    val total = correctTotal + incorrectTotal
    val pctCorrectSatisfied = correctSatisfied.toDouble/correctTotal.toDouble
    val pctIncorrectSatisfied = incorrectSatisfied.toDouble/incorrectTotal.toDouble
    val pctCorrectIgnored = correctIgnored.toDouble/correctTotal.toDouble
    val pctIncorrectIgnored = incorrectIgnored.toDouble/incorrectTotal.toDouble
    val pctCorrect = correctTotal.toDouble/total.toDouble
    val pctIncorrect = incorrectTotal.toDouble/total.toDouble
    val pctSatisfied = satisfiedTotal.toDouble/total.toDouble
    val pctIgnored = ignoredTotal.toDouble/total.toDouble
    println("total, correct, incorrect, satisfied, ignored, pct-correct, pct-incorrect, pct-satisfied, pct-ignored, pct-correct-satisfied, pct-incorrect-satisfied, pct-correct-ignored, pct-incorrect-ignored")
    val statLine = total+" "+correctTotal+" "+incorrectTotal+" "+satisfiedTotal+" "+ignoredTotal+" "+pctCorrect+" "+pctIncorrect+" "+pctSatisfied+" "+pctIgnored+" "  +pctCorrectSatisfied+" "+pctIncorrectSatisfied+" "+pctCorrectIgnored+" "+pctIncorrectIgnored
    println(prefix+" "+statLine)
    pw.println(statLine)
    pw.flush()

    println("  EDITS SATISFIED TOTAL: "+satisfiedTotal)
    println("  EDITS pct-correct/incorrect: "+pctCorrect+" "+pctIncorrect)
    println("  EDITS pct-satisfied/ignored: "+pctSatisfied+" "+pctIgnored)

    println("  EDITS SATISFIED CORRECT: "+correctSatisfied)
    println("  EDITS SATISFIED INCORRECT: "+incorrectSatisfied)
    println("  EDITS IGNORED TOTAL: "+ignoredTotal)
    println("  EDITS SATISFIED CORRECT: "+correctIgnored)
    println("  EDITS SATISFIED INCORRECT: "+incorrectIgnored)
    println("  EDITS TOTAL: "+total)
  }
  def updateStatistics[T<:HierEntity with HumanEditMention](edit:ExperimentalEdit[T]):Unit={
    edit match{
      case e:ExpMergeEdit[T] => updateSLStats(e)
      case e:ExpSplitEdit[T] => updateSNLStats(e)
      case _ => {throw new Error("type of exp-edit not recognized")}
    }
  }
  def updateSLStats[T<:HierEntity with HumanEditMention](edit:ExpMergeEdit[T]):Unit ={
    numSLEdits += 1
    val editMention = edit.mention1
    val linked = editMention.linkedMention.get.asInstanceOf[T]
    val sourceOfEdit = editMention.generatedFrom.get.asInstanceOf[T]
    val sourceOfLinked = linked.generatedFrom.get.asInstanceOf[T]
    if(sourceOfEdit.entityRoot eq sourceOfLinked.entityRoot){
      if(edit.isCorrect)correctSLSatisfied += 1 else incorrectSLSatisfied += 1
    }else{
      if(edit.isCorrect)correctSLIgnored += 1 else incorrectSLIgnored += 1
    }
  }
  def updateSNLStats[T<:HierEntity with HumanEditMention](edit:ExpSplitEdit[T]):Unit ={
    numSNLEdits += 1
    val editMention = edit.mention1
    val linked = editMention.linkedMention.get.asInstanceOf[T]
    val sourceOfEdit = editMention.generatedFrom.get.asInstanceOf[T]
    val sourceOfLinked = linked.generatedFrom.get.asInstanceOf[T]
    if(sourceOfEdit.entityRoot eq sourceOfLinked.entityRoot){
      if(edit.isCorrect)correctSNLIgnored += 1 else incorrectSNLIgnored += 1
    }else{
      if(edit.isCorrect)correctSNLSatisfied += 1 else incorrectSNLSatisfied += 1
    }
  }
}

trait HumanEditDebugUtils{
  //split debug stats
  var snlSatisfied=0
  var snlViolatedDueToModelAtGenerator=0
  var snlViolatedDueToInferenceAtGenerator=0
  var snlViolatedDueToModelAtLink=0
  var snlViolatedDueToInferenceAtLink=0
  def snlViolatedDueToInference = snlViolatedDueToInferenceAtGenerator + snlViolatedDueToInferenceAtLink
  def snlViolatedDueToModel = snlViolatedDueToModelAtGenerator + snlViolatedDueToModelAtLink
  def snlViolated=snlViolatedDueToModel + snlViolatedDueToInference
  def snlTotal = snlSatisfied+snlViolated
  def snlViolatedDueToGenerator = snlViolatedDueToInferenceAtGenerator+snlViolatedDueToModelAtGenerator
  //debug tools
  def splitRight(left:AuthorEntity,right:AuthorEntity)(implicit d:DiffList):Unit
  def mergeLeft(left:AuthorEntity,right:AuthorEntity)(implicit d:DiffList):Unit
  def mergeUp(left:AuthorEntity,right:AuthorEntity)(implicit d:DiffList):AuthorEntity
  def model:Model
  //
  def reset(): Unit = {
    snlSatisfied=0
    snlViolatedDueToModelAtGenerator=0
    snlViolatedDueToInferenceAtGenerator=0
    snlViolatedDueToModelAtLink=0
    snlViolatedDueToInferenceAtLink=0
  }
  def printSNLStatistics(): Unit = {
    println("\n-----should-not-link (SNL) statistics-----")
    println("#total constraints: "+snlTotal)
    println("#satisfied: "+snlSatisfied+" pct:"+(snlSatisfied.toDouble/snlTotal.toDouble))
    println("#violated: "+snlViolated+" pct:"+(snlViolated.toDouble/snlTotal.toDouble))
    println("  #due-to-inference: "+snlViolatedDueToInference+" pct: "+(snlViolatedDueToInference.toDouble/snlViolated))
    println("    #due to generator: "+snlViolatedDueToInferenceAtGenerator)
    println("    #due to snl:       "+(snlViolatedDueToInference-snlViolatedDueToInferenceAtGenerator))
    println("  #due-to-model: "+snlViolatedDueToModel+" pct: "+ (snlViolatedDueToModel.toDouble/snlViolated.toDouble))
    println("    #due to generator: "+snlViolatedDueToModelAtGenerator)
    println("    #due to snl:       "+(snlViolatedDueToModel-snlViolatedDueToModelAtGenerator))
    println("  #due to generators: "+snlViolatedDueToGenerator)
    println("-----------------------------------------")
  }

  def debugSNLConstraint(editMention:AuthorEntity):Unit ={
    println("debugSNLConstraint(e)")
    if(editMention.editType eq HumanEditMention.ET_SHOULD_NOT_LINK){
      println("  should-not-link")
      val linkedMention = editMention.linkedMention.get.asInstanceOf[AuthorEntity]
      val e1 = editMention.generatedFrom.get.asInstanceOf[AuthorEntity]
      val e2 = linkedMention.generatedFrom.get.asInstanceOf[AuthorEntity]
      var splitPointParent:AuthorEntity=null
      var splitPointChild:AuthorEntity=null
      if(e1.parentEntity eq e2){
        splitPointParent=e2
        splitPointChild=e1
      }else if(e2.parentEntity eq e1){
        splitPointParent=e1
        splitPointChild=e2
      }else if(!e1.entityRoot.eq(e2.entityRoot)){
        snlSatisfied += 1
      }
      if(splitPointParent!=null){ //SNL-constraint not satisfied, let's debug further
        println("\n=====Debugging SNL constraint=====")
        val d = new DebugDiffList
        println("---Debugging edit-mention affinity to generator---")
        val emAffinity = debugEditAffinityToGenerator(editMention)
        println("---Debugging linked-mention affinity to generator---")
        val lmAffinity = debugEditAffinityToGenerator(linkedMention)
        println("---Debugging split point---")
        if(emAffinity==1 || lmAffinity==1){snlViolatedDueToInferenceAtGenerator += 1;println("  snl-test failed: inference failed to assign mention to generator")}
        if(emAffinity == -1 || lmAffinity == -1){snlViolatedDueToModelAtGenerator += 1;println("  snl-test failed: model failed to assign mention to generator")}
        if(emAffinity==0 && lmAffinity==0){ //passed test so far
          println("  link-to-gen-test passed: continuing tests.")
          if(e1.entityRoot eq e2.entityRoot){ //mentions failed to split entity
            splitRight(splitPointParent,splitPointChild)(d)
            val score = d.scoreAndUndo(model)
            if(score<0.0){snlViolatedDueToModelAtLink += 1;println("snl-test failed: model")} else {snlViolatedDueToInferenceAtLink += 1;println("snl-test-failed:inference")}
          } else{
            println("snl-test passed")
            snlSatisfied += 1
          }
        }
        println("^^^^^Done testing SNL constraint^^^^^")
        println("-------------------------------------")
      }
      printSNLStatistics()
    }
  }
  def mergeEditToGenerator(e:AuthorEntity,d:DiffList):Unit ={
    if(!e.generatedFrom.get.entityRoot.isLeaf)this.mergeLeft(e.generatedFrom.get.entityRoot.asInstanceOf[AuthorEntity],e)(d)
    else this.mergeUp(e.generatedFrom.get.asInstanceOf[AuthorEntity],e.entityRoot.asInstanceOf[AuthorEntity])(d)
  }
  def debugEditAffinityToGenerator(e:AuthorEntity):Int ={
    if(!(e.entityRoot eq e.generatedFrom.get.entityRoot)){
      val d = new DebugDiffList
      mergeEditToGenerator(e,d)
      //if(!e.generatedFrom.get.entityRoot.isLeaf)this.mergeLeft(e.generatedFrom.get.entityRoot.asInstanceOf[AuthorEntity],e)(d)
      //else this.mergeUp(e.generatedFrom.get.asInstanceOf[AuthorEntity],e.entityRoot.asInstanceOf[AuthorEntity])(d)
      val score = d.scoreAndUndo(model)
      //println("  Failed test 1. Affinity diff-score to generator: "+score)
      if(score<=0.0) -1 else  1
      //if(score<=0.0)println("    test1 failure: model") else println("    test1 failure: inference")
    } else 0 //else println("  Passed test 1: edit in generated entity.")
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
}
