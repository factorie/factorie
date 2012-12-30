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

object GenerateExperiments{
  trait ExperimentalEdit[T<:HierEntity with HumanEditMention]{
    def score:Double
    def isCorrect:Boolean = score>0
    def mentions:Seq[T]
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
  def allMergeEdits[T<:HierEntity with HumanEditMention ](allEntities:Seq[T],newEntity:Unit=>T,scoreFunction:Seq[HierEntity]=>Double,createEditMentionFrom:T=>T,minESize:Int):Seq[ExpMergeEdit[T]] ={
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