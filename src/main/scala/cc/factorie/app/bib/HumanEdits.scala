package cc.factorie.app.bib
import cc.factorie.util.Attr
import cc.factorie._
import cc.factorie.app.nlp.coref._
import collection.mutable.ArrayBuffer

/* Keep track of one of these per user */
class UserReliabilityVariable extends RealVariable {
  var totalImpactfulEdits = 0.0
  var totalEdits = 0.0
  def updateValue(d:DiffList) = this.set(totalImpactfulEdits/totalEdits)(d)
}

trait HumanEditMention extends Attr{
  def editSet = attr[EditSetVariable]
  //var linkSet
  var editType:String = "none"
//  attr += new EditSetVariable(entity)
  attr += new UserReliabilityVariable
}

class EditSetVariable(val entity: Entity) extends SetVariable[HierEntity with HumanEditMention] with EntityAttr

class HumanEditTemplate extends TupleTemplateWithStatistics3[EntityExists, IsEntity, EditSetVariable] {

  def unroll1(eExists: EntityExists) = Factor(eExists,
    eExists.entity.attr[IsEntity], eExists.entity.attr[EditSetVariable])

  def unroll2(isEntity: IsEntity) = Factor(isEntity.entity.attr[EntityExists],
    isEntity, isEntity.entity.attr[EditSetVariable])

  def unroll3(editSetVar: EditSetVariable) = Factor(
    editSetVar.entity.attr[EntityExists], editSetVar.entity.attr[IsEntity],
    editSetVar)

  def score(eExists: EntityExists#Value, isEntity: IsEntity#Value, editSetVar:
  EditSetVariable#Value): Double = {

    def scoreEdit(entity: HierEntity) = 1

    var result = 0.0
    for (edit <- editSetVar) {
      result += scoreEdit(edit)
    }

    //if(edit.entityRoot eq edit.linkedEdit.entityRoot)
    //if(e1.entityRoot eq e2.entityRoot) then they are in the same entity
    result
  }
}



object GenerateExperiments{

  trait ExperimentalEdit[T<:HierEntity]{
    def score:Double
    def isCorrect:Boolean = score>0
    def mentions:Seq[T]
  }
  class ExpMergeEdit[T<:HierEntity](val mention1:T,val mention2:T,val score:Double) extends ExperimentalEdit[T]{val mentions:Seq[T]=Seq(mention1,mention2)}
  class ExpSplitEdit[T<:HierEntity](val mention1:T,val mention2:T,val score:Double) extends ExperimentalEdit[T]{val mentions:Seq[T]=Seq(mention1,mention2)}

  def runExperiment[T<:HierEntity](entities:Seq[T],streamOfEdits:Seq[ExperimentalEdit[T]], output:BufferedWriter):Unit ={

  }

  def allMergeEdits[T<:HierEntity](entities:Seq[T],newEntity:Unit=>T,scoreFunction:Seq[T]=>Double):Seq[ExpMergeEdit[T]] ={
    def createEditMentionFrom(original:T):T = {
      val cp = newEntity()
      original.attr.all[AnyRef].foreach(cp.attr += _)
      cp
    }
    val result = new ArrayBuffer[ExpMergeEdit[T]]
    val entitiesToScore = new ArrayBuffer[T]
    val originalScore = scoreFunction(entitiesToScore)
    entitiesToScore ++= entities
    var i = 0
    var j = 0
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