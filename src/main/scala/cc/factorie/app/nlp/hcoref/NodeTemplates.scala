/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */
package cc.factorie.app.nlp.hcoref

import cc.factorie._
import cc.factorie.model._
import cc.factorie.variable._
import scala.reflect.ClassTag
import cc.factorie.la.{Tensor, Tensor1}
import cc.factorie.Parameters

/**
 * @author John Sullivan
 */
trait DebuggableTemplate {
  protected var _debug: Boolean = false
  def debugOn() = _debug = true
  def debugOff() = _debug = false
  def name: String

  def report(score:Double, weight:Double) {
    if(_debug) {
    println("\t%.4f = %.4f * %.4f (score * weight)  [%s]".format(score * weight, score, weight, name))
    }
  }
}

class EntitySizePrior[Vars <: NodeVariables[Vars]](val weight:Double=0.1, val exponent:Double=1.2, val saturation:Double=128)
  extends TupleTemplateWithStatistics3[Node[Vars]#Exists,Node[Vars]#IsRoot,Node[Vars]#MentionCount]{

  def unroll1(exists: Node[Vars]#Exists) = Factor(exists, exists.node.isRootVar, exists.node.mentionCountVar)
  def unroll2(isRoot: Node[Vars]#IsRoot) = Factor(isRoot.node.existsVar, isRoot, isRoot.node.mentionCountVar)
  def unroll3(mentionCount: Node[Vars]#MentionCount) = Factor(mentionCount.node.existsVar, mentionCount.node.isRootVar, mentionCount)

  def score(exists: Node[Vars]#Exists#Value, isRoot: Node[Vars]#IsRoot#Value, mentionCount: Node[Vars]#MentionCount#Value) = if(exists.booleanValue && isRoot.booleanValue) {
    math.min(saturation, math.pow(mentionCount, exponent)) * weight
  } else {
    0.0
  }
}

class SingleBagTemplate[Vars <: NodeVariables[Vars]](initialWeight:Double, getBag:(Vars => BagOfWordsVariable), getScore:(BagOfWordsVariable => Double), val name:String)(implicit ct:ClassTag[Vars], params:Parameters)
extends Template2[Node[Vars]#Exists, Vars]
with DotFamily2[Node[Vars]#Exists, Vars]
with DebuggableTemplate {

  def unroll1(v: Node[Vars]#Exists) = Factor(v, v.node.variables)
  def unroll2(v: Vars) = Factor(v.node.existsVar, v)
  
  override def statistics(exists: Node[Vars]#Exists#Value, vars:Vars) = if(exists.booleanValue) {
    val score = getScore(getBag(vars))
    report(score, t(0))
    Tensor1(score)
  } else {
    report(0.0, t(0))
    Tensor1(0.0)
  }

  private val t = Tensor1(initialWeight)
  val _weights = params.Weights(t)
  
  def weights: Weights = _weights


}

class BagOfWordsEntropy[Vars <: NodeVariables[Vars]](initialWeight:Double, getBag:(Vars => BagOfWordsVariable))(implicit ct:ClassTag[Vars], params:Parameters)
  extends SingleBagTemplate[Vars](initialWeight, getBag, {b =>
    val bag = b.value
    var entropy = 0.0
    var n = 0.0
    val l1Norm = bag.l1Norm
    bag.asHashMap.foreach{ case(k,v) =>
      entropy -= (v/l1Norm)*math.log(v/l1Norm)
      n+=1.0
    }
    if(n>1)entropy /= scala.math.log(n) //normalized entropy in [0,1]
    -entropy
  }, "BagOfWordsEntropy")

class RootNodeBagTemplate[Vars <: NodeVariables[Vars]](initialWeight:Double, getBag:(Vars => BagOfWordsVariable), getScore:(BagOfWordsVariable => Double), val name:String)(implicit ct:ClassTag[Vars], params:Parameters)
  extends Template3[Node[Vars]#Exists,Node[Vars]#IsRoot,Vars]
  with DotFamily3[Node[Vars]#Exists,Node[Vars]#IsRoot,Vars]
  with DebuggableTemplate {

  def unroll1(exists: Node[Vars]#Exists) = Factor(exists, exists.node.isRootVar, exists.node.variables)
  def unroll2(isRoot: Node[Vars]#IsRoot) = Factor(isRoot.node.existsVar, isRoot, isRoot.node.variables)
  def unroll3(vars: Vars) = Factor(vars.node.existsVar, vars.node.isRootVar, vars)


  override def statistics(exists: Node[Vars]#Exists#Value, isRoot: Node[Vars]#IsRoot#Value, vars: Vars) = if(exists.booleanValue && isRoot.booleanValue) {
    val score = getScore(getBag(vars))
    report(score, t(0))
    Tensor1(score)
  } else {
    report(0.0, t(0))
    Tensor1(0.0)
  }

  private val t = Tensor1(initialWeight)
  val _weights = params.Weights(t)

  def weights: Weights = _weights

}

class BagOfWordsSizePrior[Vars <: NodeVariables[Vars]](initialWeight:Double, getBag:(Vars => BagOfWordsVariable))(implicit ct:ClassTag[Vars], params:Parameters)
  extends RootNodeBagTemplate[Vars](initialWeight, getBag, {bag => if (bag.size > 0) - bag.size.toDouble / bag.value.l1Norm else 0.0}, "BagOfWordsSizePrior")

class EmptyBagPenalty[Vars <: NodeVariables[Vars]](initialWeight:Double, getBag:(Vars => BagOfWordsVariable))(implicit ct:ClassTag[Vars], params:Parameters)
  extends RootNodeBagTemplate[Vars](initialWeight, getBag, {bag => if (bag.size == 0) -1.0 else 0.0}, "EmptyBagPenalty")

/*

class EntityNameTemplate[B<:BagOfWordsVariable with EntityAttr](val firstLetterWeight:Double=4.0, val fullNameWeight:Double=4.0,val weight:Double=64,val saturation:Double=128.0, val penaltyOnNoName:Double=2.0)(implicit m:Manifest[B]) extends TupleTemplateWithStatistics3[EntityExists,IsEntity,B] with DebuggableTemplate{
  val name = "EntityNameTemplate(flWeight="+firstLetterWeight+", fnWeight="+fullNameWeight+", weight="+weight+" sat="+saturation+")"
  println("EntityNameTemplate("+weight+")")
  def unroll1(exists:EntityExists) = Factor(exists,exists.entity.attr[IsEntity],exists.entity.attr[B])
  def unroll2(isEntity:IsEntity) = Factor(isEntity.entity.attr[EntityExists],isEntity,isEntity.entity.attr[B])
  def unroll3(bag:B) = Factor(bag.entity.attr[EntityExists],bag.entity.attr[IsEntity],bag)//throw new Exception("An entitie's status as a mention should never change.")
  def score(exists:EntityExists#Value, isEntity:IsEntity#Value, bag:B#Value): Double ={
    var result = 0.0
    if(exists.booleanValue){
      val bagSeq = bag.iterator.filter(_._1.length>0).toSeq
      var firstLetterMismatches = 0
      var nameMismatches = 0
      var i=0;var j=0
      while(i<bagSeq.size){
        val (wordi,weighti) = bagSeq(i)
        j=i+1
        while(j<bagSeq.size){
          val (wordj,weightj) = bagSeq(j)
          if(wordi.charAt(0) != wordj.charAt(0))firstLetterMismatches += 1 //weighti*weightj
          else if(FeatureUtils.isInitial(wordi) && FeatureUtils.isInitial(wordj) && wordi != wordj)firstLetterMismatches += 1
          if(wordi.length>1 && wordj.length>1 && !FeatureUtils.isInitial(wordi) && !FeatureUtils.isInitial(wordj))nameMismatches += wordi.editDistance(wordj)
          j += 1
        }
        i += 1
      }
      result -= scala.math.min(saturation,firstLetterMismatches*firstLetterWeight)
      result -= scala.math.min(saturation,nameMismatches*fullNameWeight)
      result = result*weight
      //if(isEntity.booleanValue){
      if(bag.size==0){
        if(isEntity.booleanValue)result -= penaltyOnNoName
      }
      //}
      if(_debug)println("  "+debug(result))
    }
    result
  }
}
*/

abstract class ChildParentTemplate[Vars <: NodeVariables[Vars]](val initWeights:Tensor1)(implicit v1:ClassTag[Vars], params:Parameters)
  extends Template3[ArrowVariable[Node[Vars], Node[Vars]], Vars, Vars]
  with DotFamily3[ArrowVariable[Node[Vars], Node[Vars]], Vars, Vars]{
  override def unroll1(v: ArrowVariable[Node[Vars], Node[Vars]]) = Option(v.dst) match { // If the parent-child relationship exists, we generate factors for it
    case Some(dest) => Factor(v, v.src.variables, dest.variables)
    case None => Nil
  }
  def unroll2(v: Vars) = Nil
  def unroll3(v: Vars) = Nil

  val _weights = params.Weights(initWeights)

  def weights: Weights = _weights
}

class ChildParentCosineDistance[Vars <: NodeVariables[Vars]](weight:Double, shift: Double, getBag:(Vars => BagOfWordsVariable))(implicit c:ClassTag[Vars], p:Parameters) extends ChildParentTemplate[Vars](Tensor1(weight)) with DebuggableTemplate {
  val name: String = "ChildParentCosineDistance: %s".format(getBag)

  override def statistics(v1: (Node[Vars], Node[Vars]), child: Vars, parent: Vars): Tensor = {
    val childBag = getBag(child)
    val parentBag = getBag(parent)
    val v = childBag.value.cosineSimilarity(parentBag.value, childBag.value) + shift

    report(v, initWeights(0))
    Tensor1(v)
  }
}

/**
 * This feature serves to ensure that certain merges are forbidden. Specifically no two nodes that share the same value
 * in the [[BagOfWordsVariable]] should be permitted to merge. Together with [[IdentityFactor]] it can create uniquely
 * identifying features.
 */
class ExclusiveConstraintFactor[Vars <: NodeVariables[Vars]](getBag:(Vars => BagOfWordsVariable))(implicit ct:ClassTag[Vars])
  extends TupleTemplateWithStatistics3[Node[Vars]#Exists, Node[Vars]#IsRoot, Vars]
  with DebuggableTemplate {
  val name = "ExclusiveConstraintFactor"

  def unroll1(exists:Node[Vars]#Exists) = Factor(exists,exists.node.isRootVar,exists.node.variables)
  def unroll2(isEntity:Node[Vars]#IsRoot) = Factor(isEntity.node.existsVar,isEntity,isEntity.node.variables)
  def unroll3(bag:Vars) = Factor(bag.node.existsVar,bag.node.isRootVar,bag) // this should really never happen

  def score(exists: Node[Vars]#Exists#Value, isRoot: Node[Vars]#IsRoot#Value, vars: Vars) = {
    val bag = getBag(vars)
    var result = 0.0
    if(exists.booleanValue && bag.value.size >= 2) {
      result = -999999.0
    } else {
      result = 0.0
    }
    report(result, 1.0)
    result
  }
}

/**
 * This feature serves to account for special information that may uniquely identify an entity. If a merge is proposed
 * between two nodes that share a value in getBag they will be merged. This feature does not ensure that the value in
 * getBag is unique, [[ExclusiveConstraintFactor]] manages that separately.
 */
class IdentityFactor[Vars <: NodeVariables[Vars]](getBag:(Vars => BagOfWordsVariable))(implicit ct:ClassTag[Vars])
  extends TupleTemplateWithStatistics3[ArrowVariable[Node[Vars], Node[Vars]], Vars, Vars]
  with DebuggableTemplate {
  val name = "IdentityFactor"

  override def unroll1(v: ArrowVariable[Node[Vars], Node[Vars]]) = Option(v.dst) match { // If the parent-child relationship exists, we generate factors for it
    case Some(dest) => Factor(v, v.src.variables, dest.variables)
    case None => Nil
  }
  def unroll2(v: Vars) = Nil
  def unroll3(v: Vars) = Nil

  def score(v1: (Node[Vars], Node[Vars]), child: Vars, parent: Vars) = {
    val childBag = getBag(child)
    val parentBag = getBag(parent)
    var result = 0.0
    if(childBag.value.asHashMap.exists{case (id, _) => parentBag.value.asHashMap.contains(id)}) {
      result = 999999.0
    } else {
      result = 0.0
    }
    report(result, 1.0)
    result
  }
}