package cc.factorie.app.nlp.xcoref

import cc.factorie.model.TupleTemplateWithStatistics3
import cc.factorie._
import cc.factorie.app.nlp.hcoref.DebugableTemplate
import cc.factorie.variable.BooleanValue

/**
 * @author John Sullivan
 */
abstract class CorefModel[Vars <: NodeVariables[Vars]] extends TemplateModel with Parameters { // This is to ensure that the model's features' NodeVariables match the type of the model's NodeVariables
implicit val params:Parameters = this

  this += new StructuralPrior[Vars]() {this.debugOff()}
}

class StructuralPrior[Vars <: NodeVariables[Vars]](entityvalue:Double = -0.5,
                                                   subEntityValue:Double = 0.25)
  extends TupleTemplateWithStatistics3[Node[Vars]#IsRoot, Node[Vars]#IsMention, Node[Vars]#Exists] with DebugableTemplate{

  def name = "StructuralPrior"
  def unroll1(isRoot: Node[Vars]#IsRoot) = Factor(isRoot, isRoot.node.isMentionVar, isRoot.node.existsVar)

  def unroll2(isMention: Node[Vars]#IsMention) = throw new Exception(
    "is Mention changed for %s. This should never happen" format isMention.node)

  def unroll3(exists: Node[Vars]#Exists) = Factor(exists.node.isRootVar, exists.node.isMentionVar, exists)

  def score(isRoot: BooleanValue, isMention: BooleanValue, exists: BooleanValue): Double = {
    if(isRoot.booleanValue && exists.booleanValue) {
      if(_debug) println("Entity benefit: " + -entityvalue)
      -entityvalue
    } else if(!isRoot.booleanValue && !isMention.booleanValue && exists.booleanValue) {
      if(_debug) println("Subentity penalty: " + -subEntityValue)
      -subEntityValue
    } else {
      if(_debug) println("Non-Root Mention " + 0)
      0.0
    }
  }
}