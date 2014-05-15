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
package cc.factorie.app.nlp.xcoref

import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.variable.{TensorVar, ArrowVariable}
import cc.factorie.model._
import cc.factorie.variable._
import scala.reflect.ClassTag
import cc.factorie.la.{Tensor, Tensor1}
import scala.Some
import cc.factorie.Parameters

/**
 * @author John Sullivan
 */
abstract class ChildParentTemplate[Vars <: NodeVariables[Vars]](initWeights:Tensor1)(implicit v1:ClassTag[Vars], params:Parameters)
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

class BagOfWordsTensorEntropy[Vars <:NodeVariables[Vars], T <: TensorVar](initialWeight:Double, getBag:(Vars => T))(implicit ct:ClassTag[Vars], params:Parameters)
  extends Template2[Node[Vars]#Exists, Vars]
  with DotFamily2[Node[Vars]#Exists, Vars]
  with DebugableTemplate {


  def name: String = "BagOfWordsEntropy"

  val _weights = params.Weights(Tensor1(initialWeight))
  def weights: Weights = _weights

  def unroll1(v: Node[Vars]#Exists) = Factor(v, v.node.variables)

  def unroll2(v: Vars) = Factor(v.node.existsVar, v)

  override def statistics(exists: Node[Vars]#Exists#Value, vars: Vars#Value) = {
    val bag = getBag(vars).value
    var entropy = 0.0
    var n = 0.0
    if(exists.booleanValue /*&& isEntity.booleanValue*/){
      val l1Norm = bag.oneNorm
      bag.foreachActiveElement{ case(k,v) =>
        entropy -= (v/l1Norm)*math.log(v/l1Norm)
        n+=1.0
      }
    }
    if(n>1)entropy /= scala.math.log(n) //normalized entropy in [0,1]
    if(entropy.isNaN) {
      println("Warning entropy is NaN!")
      println("n=:" + n)
      println("active size: %d\tone norm: %.4f\t".format(bag.activeDomainSize, bag.oneNorm))
      bag.foreachActiveElement{ case (index, value) =>
        println("index:%d\tvalue:%.4f\t(value/oneNorm):%.4f\tlog(value/oneNorm:%.4f".format(index, value, value/bag.oneNorm, math.log(value/bag.oneNorm)))
      }
    }
    entropy = -entropy
    if(_debug)println("  "+debug(entropy*initialWeight))
    Tensor1(entropy)
  }
}

class BagOfWordsEntropy[Vars <:NodeVariables[Vars]](initialWeight:Double, getBag:(Vars => BagOfWordsVariable))(implicit ct:ClassTag[Vars], params:Parameters)
  extends Template2[Node[Vars]#Exists, Vars]
  with DotFamily2[Node[Vars]#Exists, Vars]
  with DebugableTemplate {


  def name: String = "BagOfWordsEntropy"

  val _weights = params.Weights(Tensor1(initialWeight))
  def weights: Weights = _weights

  def unroll1(v: Node[Vars]#Exists) = Factor(v, v.node.variables)

  def unroll2(v: Vars) = Factor(v.node.existsVar, v)

  override def statistics(exists: Node[Vars]#Exists#Value, vars: Vars#Value) = {
    val bag = getBag(vars).value
    var entropy = 0.0
    var n = 0.0
    if(exists.booleanValue /*&& isEntity.booleanValue*/){
      val l1Norm = bag.l1Norm
      bag.asHashMap.foreach{ case(k,v) =>
        entropy -= (v/l1Norm)*math.log(v/l1Norm)
        n+=1.0
      }
    }
    if(n>1)entropy /= scala.math.log(n) //normalized entropy in [0,1]
    if(entropy.isNaN) {
      println("Warning entropy is NaN!")
      println("n=:" + n)
      println("active size: %d\tone norm: %.4f\t".format(bag.size, bag.l1Norm))
      bag.asHashMap.foreach{ case (index, value) =>
        println("index:%d\tvalue:%.4f\t(value/oneNorm):%.4f\tlog(value/oneNorm:%.4f".format(index, value, value/bag.l1Norm, math.log(value/bag.l1Norm)))
      }
    }
    entropy = -entropy
    if(_debug)println("  "+debug(entropy))
    Tensor1(entropy)
  }
}

abstract class BagOfWordsTemplate[Vars <: NodeVariables[Vars]](initialWeights: Tensor1)(implicit v1: ClassTag[Vars], params: Parameters)
  extends Template1[Vars]
  with DotFamily1[Vars] {
  override def unroll1(v: Vars) = Option(v) match {
    case Some(v) => Factor(v)
    case None => Nil
  }

  val _weights: Weights = params.Weights(initialWeights)

  def weights: Weights = _weights
}

abstract class StructuralTemplate[A <: Var](initialWeights: Tensor1)(implicit v1: ClassTag[A], params: Parameters)
  extends Template1[A]
  with DotFamily1[A] {

  val _weights: Weights = params.Weights(initialWeights)

  def weights: Weights = _weights
}

class ChildParentCosineDistance[Vars <: NodeVariables[Vars]](weight:Double, shift: Double, getBag:(Vars => BagOfWordsVariable))(implicit c:ClassTag[Vars], p:Parameters) extends ChildParentTemplate[Vars](Tensor1(weight)) with DebugableTemplate {
  override def name: String = "ChildParentCosineDistance: %s".format(getBag)

  override def statistics(v1: (Node[Vars], Node[Vars]), child: Vars, parent: Vars): Tensor = {
    val childBag = getBag(child)
    val parentBag = getBag(parent)
    val v = childBag.value.cosineSimilarity(parentBag.value, childBag.value) + shift

    if(_debug) {
      println(debug(v*weight))
    }
    Tensor1(v)
  }
}