package cc.factorie.app.bib
import cc.factorie.util.Attr
import cc.factorie._
import cc.factorie.app.nlp.coref._
import experiments.DebugDiffList
import java.io.{PrintWriter, File, BufferedWriter}
import collection.mutable.{HashMap,HashSet, ArrayBuffer}

/* Keep track of one of these per user */
class UserReliabilityVariable extends RealVariable {
  var totalImpactfulEdits = 0.0
  var totalEdits = 0.0
  var truth = 0.0
  def updateValue(d:DiffList) = this.set(totalImpactfulEdits/totalEdits)(d)
}

object HumanEditMention{
  val ET_SHOULD_LINK = "should-link"
  val ET_SHOULD_NOT_LINK = "should-not-link"
}
trait HumanEditMention extends Attr{
  def entity:HierEntity
  def editSet = attr[EditSetVariable]
  attr += new EditSetVariable(entity)
  //var linkSet
  var linkedMention:Option[HierEntity] = None
  var editType:String = "none"
  attr += new UserReliabilityVariable
  var generatedFrom:Option[Entity] = None
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

class HumanEditTemplateWithReliability(override val shouldLinkReward: Double, override val shouldNotLinkPenalty:Double) extends HumanEditTemplate {

  /* We need a better scoring function that weights the reliability adjustment
   * based on the number of edits that user has made...maybe something like log(totalEdits)
   * we should mix the two together so that if you have made lots of edits, we are more
   * confident about your reliability.
   *
   * Perhaps we should keep track of "experience" which is the number of edits you've made
   * in comparison to the rest of the editors. Then we can scale the score based on this.
   * To do this though, we need to have access other people's experience...*/
  override  def score(eExists: EntityExists#Value, isEntity: IsEntity#Value, editSetVar:
  EditSetVariable#Value): Double = {
    def scoreEdit(entity: HierEntity with HumanEditMention) = {
      var result = 0.0
      //we are dividing by 2 because we are looping over both linked mentions (they are in the same edit set).
      if(entity.editType eq HumanEditMention.ET_SHOULD_LINK){
        if(debugFlag)println("  EditTemplate: should-link mention")
        if(entity.entityRoot eq entity.linkedMention.get.entityRoot){
          if(debugFlag)println("    edit template should-link rewarding mention: "+(shouldLinkReward/2.0))
	  // If the person is reliabile <50% of the time, then their opinion counts negatively
          result += (shouldLinkReward/2.0) * (entity.attr[UserReliabilityVariable].doubleValue - .5)
        } //TODO: else penalty but don't divide by 2?
      }else if(entity.editType eq HumanEditMention.ET_SHOULD_NOT_LINK){
        if(entity.entityRoot eq entity.linkedMention.get.entityRoot)result -=
	    shouldNotLinkPenalty/2.0 * (entity.attr[UserReliabilityVariable].doubleValue - .5)
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
    def score:Double
    def isCorrect:Boolean = score>0
    def mentions:Seq[T]

    var owner:Int = -1    // integer representing the id of the owner who created this edit
    def setOwner(id: Int): Unit = this.owner = id
  }

  class ExpMergeEdit[T<:HierEntity with HumanEditMention](val mention1:T,val mention2:T,val score:Double) extends ExperimentalEdit[T]{
    val mentions:Seq[T]=Seq(mention1,mention2)
    mention1.linkedMention=Some(mention2)
    mention1.editType=HumanEditMention.ET_SHOULD_LINK
    mention2.linkedMention=Some(mention1)
    mention2.editType=HumanEditMention.ET_SHOULD_LINK
    def print:Unit ={
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
    mention2.linkedMention=Some(mention1)
    mention2.editType=HumanEditMention.ET_SHOULD_NOT_LINK
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
  def getAuthorEdits(initialDB:Seq[AuthorEntity],minimumEntitySize:Int):Seq[ExpMergeEdit[AuthorEntity]] ={
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
      minimumEntitySize
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
  def splitBaseline1(initialDB:Seq[AuthorEntity],evidenceBatches:Seq[Seq[AuthorEntity]],file:File):Unit ={
    val pwbl1 = new PrintWriter(file)
    val d = new DiffList
    var batchName = 0
    val toScore = new ArrayBuffer[AuthorEntity]
    toScore ++= initialDB
    var entityCount = toScore.filter(_.isEntity.booleanValue).size
    val mentionCount = toScore.size
    pwbl1.println("time samples accepted f1 p r batch-count mentions entities batch-name score maxscore")
    pwbl1.println("-1 -1 -1 "+Evaluator.pairF1(toScore).mkString(" ")+" "+mentionCount+" -1 "+batchName+" -1 -1")
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
          applySplitEdit(edit)(d)
          entityCount += 1
        }
      }
      val scores = Evaluator.pairF1(toScore)
      pwbl1.println("-1 -1 -1 "+scores.mkString(" ")+" "+mentionCount+" "+entityCount+" "+batchName+" -1 -1")
      pwbl1.flush()
    }
    d.undo;d.clear;pwbl1.close
  }
  def mergeBaseline1(initialDB:Seq[AuthorEntity],evidenceBatches:Seq[Seq[AuthorEntity]],file:File):Unit ={
    val pwbl1 = new PrintWriter(file)
    val d = new DiffList
    var batchName = 0
    val toScore = new ArrayBuffer[AuthorEntity]
    toScore ++= initialDB
    var entityCount = toScore.filter(_.isEntity.booleanValue).size
    val mentionCount = toScore.size
    pwbl1.println("time samples accepted f1 p r batch-count mentions entities batch-name score maxscore")
    pwbl1.println("-1 -1 -1 "+Evaluator.pairF1(toScore).mkString(" ")+" "+mentionCount+" -1 "+batchName+" -1 -1")
    for(batch <- evidenceBatches){
      batchName += 1
      val visited = new HashSet[AuthorEntity]
      for(edit <- batch){
        if(!visited.contains(edit)){
          visited += edit
          val parent = new AuthorEntity
          EntityUtils.linkChildToParent(edit.generatedFrom.get,parent)(d)
          EntityUtils.linkChildToParent(edit.linkedMention.get.asInstanceOf[AuthorEntity].generatedFrom.get,parent)(d)
          toScore += parent
          entityCount -= 1
        }
      }
      val scores = Evaluator.pairF1(toScore)
      pwbl1.println("-1 -1 -1 "+scores.mkString(" ")+" "+mentionCount+" "+entityCount+" "+batchName+" -1 -1")
      pwbl1.flush()
    }
    d.undo;d.clear;pwbl1.close
  }
  def mergeBaseline2(initialDB:Seq[AuthorEntity],evidenceBatches:Seq[Seq[AuthorEntity]],file:File):Unit ={
    val d2 = new DiffList
    val pwbl2 = new PrintWriter(file)
    val toScore = new ArrayBuffer[AuthorEntity]
    val mentionCount = toScore.size
    var batchName = 0
    toScore ++= initialDB
    var entityCount = toScore.filter(_.isEntity.booleanValue).size
    pwbl2.println("time samples accepted f1 p r batch-count mentions entities batch-name score maxscore")
    pwbl2.println("-1 -1 -1 "+Evaluator.pairF1(toScore).mkString(" ")+" "+mentionCount+" -1 "+batchName+" -1 -1")
    for(batch <- evidenceBatches){
      batchName += 1
      val visited = new HashSet[AuthorEntity]
      for(edit <- batch){
        if(!visited.contains(edit)){
          visited += edit
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
          entityCount -= 1
        }
      }
      //time samples accepted f1 p r batch-count mentions entities batch-name score maxscore
      val scores = Evaluator.pairF1(toScore)
      pwbl2.println("-1 -1 -1 "+scores.mkString(" ")+" "+mentionCount+" "+entityCount+" "+batchName+" -1 -1")
      pwbl2.flush()
    }
    d2.undo;d2.clear;pwbl2.close
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
  def incorrectAuthorSplitEdits(initialDB:Seq[AuthorEntity]):Seq[ExpSplitEdit[AuthorEntity]] ={
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
      }
    )
    edits
  }
  def incorrectSplitEdits[T<:HierEntity with HumanEditMention](allEntities:Seq[T],newEntity:Unit=>T,scoreFunction:Seq[HierEntity]=>Double,createEditMentionFrom:T=>T):Seq[ExpSplitEdit[T]] ={
    val result = new ArrayBuffer[ExpSplitEdit[T]]
    val entities = allEntities.filter((e:T) => e.isEntity.booleanValue)
    var i = 0
    while(i<entities.size){
      val (splitPoint,score) = findIncorrectSplit(entities(i),scoreFunction)
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
      d.undo
    }
    result
  }
  def findIncorrectSplit[T<:HierEntity with HumanEditMention](e:T,scoreFunction:Seq[HierEntity]=>Double):(T,Double) ={
    val root = e.entityRoot.asInstanceOf[T]
    var entity = e
    val scoringMentions = root.descendantsOfClass[HierEntity]
    var candidates = new ArrayBuffer[T]
    var scores = new ArrayBuffer[Double]
    var initScore = scoreFunction(scoringMentions)
    for(desc <- root.descendantsOfClass[HierEntity].filter(_.isMention.booleanValue)){
      entity = desc.asInstanceOf[T]
      while(entity != null){
        val d = new DiffList
        EntityUtils.linkChildToParent(entity,null)(d)
        val score = scoreFunction(scoringMentions)
        if(score<initScore){
          candidates += entity
          scores += score
        }
        d.undo
        entity = entity.parentEntity.asInstanceOf[T]
      }
    }
    if(candidates.size==0) (null.asInstanceOf[T],0.0)
    else {
      val rid = random.nextInt(candidates.size)
      (candidates(rid),scores(rid))
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
        d.undo
        entity = entity.parentEntity.asInstanceOf[T]
      }
    }
    result.toSeq
  }

  def allMergeEdits[T<:HierEntity with HumanEditMention](allEntities:Seq[T],newEntity:Unit=>T,scoreFunction:Seq[HierEntity]=>Double,createEditMentionFrom:T=>T,minESize:Int):Seq[ExpMergeEdit[T]] ={
    val result = new ArrayBuffer[ExpMergeEdit[T]]
    val entities = allEntities.filter((e:T) =>{e.isEntity.booleanValue && e.numLeaves>=minESize})
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
        d.undo
        assert(ei.parentEntity == null)
        assert(ej.parentEntity == null)
        //package the finding
        result += new ExpMergeEdit(createEditMentionFrom(ei),createEditMentionFrom(ej),editScore-originalScore)
        j+=1
      }
      i+=1
      print(".")
      if(i % 25 == 0)println
    }
    val numGood = result.filter(_.isCorrect).size
    val numBad = result.size - numGood
    println("Generated "+ result.size + " edits ("+ numGood + " correct edits. " + numBad + " incorrectEdits).")
    result
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
  def reset:Unit ={
    snlSatisfied=0
    snlViolatedDueToModelAtGenerator=0
    snlViolatedDueToInferenceAtGenerator=0
    snlViolatedDueToModelAtLink=0
    snlViolatedDueToInferenceAtLink=0
  }
  def printSNLStatistics:Unit ={
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
      printSNLStatistics
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
