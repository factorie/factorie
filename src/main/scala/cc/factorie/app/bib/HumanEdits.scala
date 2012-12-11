package cc.factorie.app.bib
import cc.factorie.util.Attr
import cc.factorie._
import cc.factorie.app.nlp.coref._
import collection.mutable.ArrayBuffer
import java.io.BufferedWriter

/* Keep track of one of these per user */
class UserReliabilityVariable extends RealVariable {
  var totalImpactfulEdits = 0.0
  var totalEdits = 0.0
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
}

class EditSetVariable(val entity: Entity) extends SetVariable[HierEntity with HumanEditMention] with EntityAttr

class HumanEditTemplate(val shouldLinkReward:Double=8.0,val shouldNotLinkPenalty:Double=8.0) extends TupleTemplateWithStatistics3[EntityExists, IsEntity, EditSetVariable] {

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
        if(entity.entityRoot eq entity.linkedMention.get.entityRoot)result += shouldLinkReward/2.0 //TODO: else penalty but don't divide by 2?
      }else if(entity.editType eq HumanEditMention.ET_SHOULD_NOT_LINK){
        if(entity.entityRoot eq entity.linkedMention.get.entityRoot)result -= shouldNotLinkPenalty/2.0
      }
      result
    }
    var result = 0.0
    for (edit <- editSetVar) {
      result += scoreEdit(edit)
    }
    result
  }
}



object GenerateExperiments{
  def main(args:Array[String]) = {


  }


  trait ExperimentalEdit[T<:HierEntity with HumanEditMention]{
    def score:Double
    def isCorrect:Boolean = score>0
    def mentions:Seq[T]
  }
  class ExpMergeEdit[T<:HierEntity with HumanEditMention](val mention1:T,val mention2:T,val score:Double) extends ExperimentalEdit[T]{val mentions:Seq[T]=Seq(mention1,mention2)}
  class ExpSplitEdit[T<:HierEntity with HumanEditMention](val mention1:T,val mention2:T,val score:Double) extends ExperimentalEdit[T]{val mentions:Seq[T]=Seq(mention1,mention2)}

  /*
  def runExperiment[T<:HierEntity with HumanEditMention](entities:Seq[T],streamOfEdits:Seq[ExperimentalEdit[T]], output:BufferedWriter):Unit ={
    outputResults(corefer.getEntities,corefer.numSamples,corefer.numAccepted,editCount)
  }
  def outputResults[T<:HierEntity with HumanEditMention](writer:BufferedWriter,entities:Seq[T],editScore:Double,numSamples:Int,numAccepted:Int,editCount:Int,mentionCount:Int):Unit ={

  }
  */
//      val line = ((System.currentTimeMillis - time)/1000L + " "+numSamples+" "+numAccepted+" "+scores.mkString(" ")+" "+batchCount+" "+mentionCount+" "+gtEntityCount+" "+currentBatchName+" "+curScore+" "+maxScore)

  def allMergeEdits[T<:HierEntity with HumanEditMention ](allEntities:Seq[T],newEntity:Unit=>T,scoreFunction:Seq[T]=>Double):Seq[ExpMergeEdit[T]] ={
    def createEditMentionFrom(original:T):T = {
      val cp = newEntity()
      cp.flagAsMention
      original.attr.all[AnyRef].foreach(cp.attr += _)
      cp
    }
    val result = new ArrayBuffer[ExpMergeEdit[T]]
    val entities = allEntities.filter(_.isEntity.booleanValue)
    val entitiesToScore = new ArrayBuffer[T]
    val originalScore = scoreFunction(entitiesToScore)
    entitiesToScore ++= entities
    var i = 0;var j = 0
    while(i<entities.size){
      j = i+1
      val ei = entities(i)
      while(j<entities.size){
        val ej = entities(j)
        val tmpRoot = newEntity()
        val d = new DiffList
        //see what happens if we were to merge these two entities together
        EntityUtils.linkChildToParent(ei,tmpRoot)(d)
        EntityUtils.linkChildToParent(ej,tmpRoot)(d)
        entitiesToScore += tmpRoot
        val editScore = scoreFunction(entitiesToScore)
        //undo the merge
        d.undo
        entitiesToScore.remove(entitiesToScore.size-1)
        //package the finding
        result += new ExpMergeEdit(createEditMentionFrom(ei),createEditMentionFrom(ej),editScore-originalScore)
        j+=1
      }
      i+=1
    }
    result
  }
}