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
package cc.factorie.app.nlp.parse

import cc.factorie.app.nlp._
import cc.factorie._
import cc.factorie.app.nlp.lemma.TokenLemma
import cc.factorie.app.nlp.pos.PosTag
import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, Set}
import java.io._
import cc.factorie.util._
import cc.factorie.optimize._
import scala.concurrent.Await
import cc.factorie.variable._
import cc.factorie.app.classify.backend._
import cc.factorie.la._

class LightweightParseToken(t: Token){
  lazy val string = t.string
  lazy val posTag = t.attr[PosTag]
  lazy val lemma = if(posTag ne null) t.lemmaString else string
  lazy val lemmaLower = if(posTag ne null) lemma.toLowerCase else string
  lazy val posTagString = if(posTag ne null) posTag.categoryValue else string
}

class LightweightParseSentence(s: Sentence){
  val length: Int = s.length + 1
  val _tokens: Array[LightweightParseToken] = new Array[LightweightParseToken](length-1)
  var i = 0; while(i < length-1) { _tokens(i) = new LightweightParseToken(s(i)); i += 1 }
  val parse = s.attr[ParseTree]
  val goldHeads = Seq(-1) ++ parse._targetParents.map(_ + 1)
  val goldLabels = Seq("<ROOT-ROOT>") ++ parse._labels.map(_.target.categoryValue)

  // we are working with the original sentence, with an additional
  // ROOT token that comes at index 0, moving all other indices up by 1:
  // idx < 0 -> NULL_TOKEN
  // idx = 0 -> ROOT_TOKEN
  // 0 < idx < sentence.length+1 -> sentence(idx-1)
  // idx > sentence.length -> NULL_TOKEN
  def apply(idx: Int) = idx match {
    case 0 => RootToken
    case i if (i > 0 && i < length) => _tokens(i-1)
    case _ => NullToken
  }
}

object RootToken extends LightweightParseToken(null.asInstanceOf[Token]){
  override lazy val string = ParserConstants.ROOT_STRING
  override lazy val lemmaLower = ParserConstants.ROOT_STRING
  override lazy val posTagString = ParserConstants.ROOT_STRING
}
object NullToken extends LightweightParseToken(null.asInstanceOf[Token]){
  override lazy val string = ParserConstants.NULL_STRING
  override lazy val lemmaLower = ParserConstants.NULL_STRING
  override lazy val posTagString = ParserConstants.NULL_STRING
}

object ParserConstants {
  val NOTHING = -1

  val ROOT_ID = 0

  val SHIFT  = 1
  val REDUCE = 2
  val PASS   = 3

  val LEFT  = 4
  val RIGHT = 5
  val NO    = 6

  val TRAINING   = 7
  val PREDICTING = 8
  val BOOSTING   = 9
  val PREDICTING_FAST = 10

  val NULL_STRING = "<NULL>"
  val ROOT_STRING = "<ROOT>"
  val SEP = "|"

  // for debugging purposes
  def apply(i: Int): String = i match {
    case NOTHING => "nothing"

    case SHIFT => "shift"
    case REDUCE => "reduce"
    case PASS => "pass"

    case LEFT => "left"
    case RIGHT => "right"
    case NO => "no"

    case TRAINING => "training"
    case PREDICTING => "predicting"
    case BOOSTING => "boosting"

    case ROOT_ID => "root id"

    case _ => throw new Error(s"Integer value $i is not defined in ParserConstants")
  }
}

case class ParseDecision(action: String) {
  val Array(lrnS, srpS, label) = action.split(" ")
  val leftOrRightOrNo = lrnS.toInt 		// leftarc-rightarc-noarc
  val shiftOrReduceOrPass = srpS.toInt	// shift-reduce-pass
  override def toString = action
  def readableString = s"${ParserConstants(leftOrRightOrNo)} ${ParserConstants(shiftOrReduceOrPass)} $label"
}

class ParseState(var stack: Int, var input: Int, val reducedIds: Set[Int], val sentence: LightweightParseSentence) {
  val parseSentenceLength = sentence.length

  val headIndices = Array.fill[Int](parseSentenceLength)(-1)
  val arcLabels = Array.fill[String](parseSentenceLength)("")

  val leftmostDeps = Array.fill[Int](parseSentenceLength)(-1)
  val rightmostDeps = Array.fill[Int](parseSentenceLength)(-1)

  def goldHeads = sentence.goldHeads
  def goldLabels = sentence.goldLabels

  def setHead(tokenIndex: Int, headIndex: Int, label: String) {
    // set head
    headIndices(tokenIndex) = headIndex
    arcLabels(tokenIndex) = label

    // update left and rightmost dependents
    if(headIndex != -1){
      if (tokenIndex < headIndex)
        leftmostDeps(headIndex) = tokenIndex
      else
        rightmostDeps(headIndex) = tokenIndex
    }
  }

  @tailrec final def isDescendantOf(firstIndex: Int, secondIndex: Int): Boolean = {
    val firstHeadIndex = headIndices(firstIndex)
    if (firstHeadIndex == -1) false // firstIndex has no head, so it can't be a descendant
    else if (headIndices(firstHeadIndex) == secondIndex) true
    else isDescendantOf(firstHeadIndex, secondIndex)
  }

  def leftmostDependent(tokenIndex: Int): Int = {
    if (tokenIndex == -1) -1
    else leftmostDeps(tokenIndex)
  }

  def rightmostDependent(tokenIndex: Int): Int = {
    if (tokenIndex == -1) -1
    else rightmostDeps(tokenIndex)
  }

  def leftmostDependent2(tokenIndex: Int): Int = {
    if (tokenIndex == -1) -1
    else{
      val i = leftmostDeps(tokenIndex)
      if (i == -1) -1
      else leftmostDeps(i)
    }
  }

  def rightmostDependent2(tokenIndex: Int): Int = {
    if (tokenIndex == -1) -1
    else {
      val i = rightmostDeps(tokenIndex)
      if (i == -1) -1
      else rightmostDeps(i)
    }
  }

  def leftNearestSibling(tokenIndex: Int): Int = {
    val tokenHeadIndex = headIndices(tokenIndex)
    if(tokenHeadIndex != -1){
      var i = tokenIndex - 1
      while(i >= 0){
        if (headIndices(i) != -1 && headIndices(i) == tokenHeadIndex)
          return i
        i -= 1
      }
    }
    -1
  }

  def rightNearestSibling(tokenIndex: Int): Int = {
    val tokenHeadIndex = headIndices(tokenIndex)
    if(tokenHeadIndex != -1){
      var i = tokenIndex + 1
      while(i < parseSentenceLength){
        if(headIndices(i) != -1 && headIndices(i) == tokenHeadIndex)
          return i
        i += 1
      }
    }
    -1
  }

  def inputToken(offset: Int): Int = {
    val i = input + offset
    if (i < 0 || parseSentenceLength - 1 < i) -1
    else i
  }

  def lambdaToken(offset: Int): Int = {
    val i = stack + offset
    if (i < 0 || parseSentenceLength - 1 < i) -1
    else i
  }

  def stackToken(offset: Int): Int = {
    if (offset == 0)
      return stack

    var off = math.abs(offset)
    var dir = if (offset < 0) -1 else 1
    var i = stack + dir
    while (0 < i && i < input) {
      if (!reducedIds.contains(i)) {
        off -= 1
        if (off == 0)
          return i
      }
      i += dir
    }
    -1
  }
}

/** Default transition-based dependency parser. */
class TransitionBasedParser extends DocumentAnnotator {
  private val logger = Logger.getLogger(this.getClass.getName)

  def this(stream:InputStream) = { this(); deserialize(stream) }
  def this(file: File) = this(new FileInputStream(file))
  def this(url:java.net.URL) = {
    this()
    val stream = url.openConnection.getInputStream
    if (stream.available <= 0) throw new Error("Could not open "+url)
    logger.debug("TransitionBasedParser loading from "+url)
    deserialize(stream)
  }

  object FeaturesDomain extends CategoricalVectorDomain[String]
  class NonProjDependencyParserFeatures(val decisionVariable: ParseDecisionVariable) extends BinaryFeatureVectorVariable[String] {
    def domain = FeaturesDomain
    override def skipNonCategories = domain.dimensionDomain.frozen
  }

  def addFeatureString(featureVariable: NonProjDependencyParserFeatures, feat: String) = featureVariable += feat // for things that don't need to be checked for <NULL>
  def addFeatureNoNulls(featureVariable: NonProjDependencyParserFeatures, feat: String) = if(!feat.endsWith(ParserConstants.NULL_STRING)) featureVariable += feat
  def addConjunctiveFeatureNoNulls(featureVariable: NonProjDependencyParserFeatures, feats: Array[String]) = {
    val len = feats.length
    var i = 0
    var addFeats = false
    while(i < len && !addFeats) {
      if (!feats(i).endsWith(ParserConstants.NULL_STRING))
        addFeats = true
      i += 1
    }
    if(addFeats){
      val sb = new StringBuilder()
      i = 0
      while(i < len-1) {
        sb.append(feats(i))
        sb.append(ParserConstants.SEP)
        i += 1
      }
      sb.append(feats(i))
      featureVariable += sb.toString
    }
  }
  def addConjunctiveFeatureWithNulls(featureVariable: NonProjDependencyParserFeatures, feats: Array[String]) = {
    val len = feats.length
    var i = 0
    val sb = new StringBuilder()
    while(i < len-1) {
      sb.append(feats(i))
      sb.append(ParserConstants.SEP)
      i += 1
    }
    sb.append(feats(i))
    featureVariable += sb.toString
  }

  val predictingNPSR = new NonProjectiveShiftReduce(classify)

  object ParseDecisionDomain extends CategoricalDomain[String]{
    import ParserConstants._
    val defaultLabel = ParseTreeLabelDomain.defaultCategory
    val defaultCategory = NOTHING + " " + NOTHING + " " + defaultLabel
    this += defaultCategory
  }

  class ParseDecisionVariable(val state: ParseState) extends CategoricalVariable[String] {
    def domain = ParseDecisionDomain
    val features = new NonProjDependencyParserFeatures(this)
    var target = -1
  }
  
  class ParseDecisionExample(decisionVariable: ParseDecisionVariable, m: LinearMulticlassClassifier, objective: MultivariateOptimizableObjective[Int]) extends Example {
    def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator): Unit = {
      val (obj, objGradient) = objective.valueAndGradient(m.predict(decisionVariable.features.value), decisionVariable.target)
      value.accumulate(obj)
      gradient.accumulate(m.weights, decisionVariable.features.value outer objGradient)
    }
  }

  def computeFeatures(state: ParseState, featureVariable: NonProjDependencyParserFeatures, addFeature: (NonProjDependencyParserFeatures, String) => Unit, addConjunctiveFeature: (NonProjDependencyParserFeatures, Array[String]) => Unit) = {

      // don't use growable tensor at test time -- we know the size of the domain
    if(FeaturesDomain.dimensionDomain.frozen)
      featureVariable.set(new SparseBinaryTensor1(FeaturesDomain.dimensionDomain.size))(null)
    else
      featureVariable.set(new GrowableSparseBinaryTensor1(FeaturesDomain.dimensionDomain))(null)

    // basic features
    val lambdaIndex = state.lambdaToken(0)
    val betaIndex = state.inputToken(0)

    val lambdaToken = state.sentence(lambdaIndex)
    val betaToken = state.sentence(betaIndex)

    val lambdaForm = "l:f:" + lambdaToken.string
    val lambdaLemma = "l:m:" + lambdaToken.lemmaLower
    val lambdaPos = "l:p:" + lambdaToken.posTagString

    val betaForm =  "b:f:" + betaToken.string
    val betaLemma = "b:m:" + betaToken.lemmaLower
    val betaPos = "b:p:" + betaToken.posTagString

    addFeature(featureVariable, lambdaForm)
    addFeature(featureVariable, lambdaLemma)
    addFeature(featureVariable, lambdaPos)

    addFeature(featureVariable, betaForm)
    addFeature(featureVariable, betaLemma)
    addFeature(featureVariable, betaPos)

    addConjunctiveFeature(featureVariable, Array(lambdaPos, lambdaLemma))
    addConjunctiveFeature(featureVariable, Array(betaPos, betaLemma))
    addConjunctiveFeature(featureVariable, Array(lambdaPos, betaPos))
    addConjunctiveFeature(featureVariable, Array(lambdaPos, betaLemma))

    addConjunctiveFeature(featureVariable, Array(lambdaLemma, betaPos)) // not in new
    addConjunctiveFeature(featureVariable, Array(lambdaLemma, betaLemma))

    // 1-gram features
    val stack_1Index = state.stackToken(-1)
    val stack_2Index = state.stackToken(-2)
    val lambda_2Index = state.lambdaToken(-2)
    val lambda_1Index = state.lambdaToken(-1)
    val lambda1Index = state.lambdaToken(1)
    val lambda2Index = state.lambdaToken(2)
    val beta_2Index = state.inputToken(-2)
    val beta_1Index = state.inputToken(-1)
    val beta1Index = state.inputToken(1)
    val beta2Index = state.inputToken(2)
    val beta3Index = state.inputToken(3)

    val stack_1 = state.sentence(stack_1Index)
    val stack_2 = state.sentence(stack_2Index)
    val lambda_2 = state.sentence(lambda_2Index)
    val lambda_1 = state.sentence(lambda_1Index)
    val lambda1 = state.sentence(lambda1Index)
    val lambda2 = state.sentence(lambda2Index)
    val beta_2 = state.sentence(beta_2Index)
    val beta_1 = state.sentence(beta_1Index)
    val beta1 = state.sentence(beta1Index)
    val beta2 = state.sentence(beta2Index)
    val beta3 = state.sentence(beta3Index)

    val stackLemma_1 = "s-1:m:"+stack_1.lemmaLower
    val lambdaLemma_1 = "l-1:m:"+lambda_1.lemmaLower
    val lambdaLemma1 = "l1:m:"+lambda1.lemmaLower
    val betaLemma_2 = "b-2:m:"+beta_2.lemmaLower
    val betaLemma_1 = "b-1:m:"+beta_1.lemmaLower
    val betaLemma1 = "b1:m:"+beta1.lemmaLower
    val betaLemma2 = "b2:m:"+beta2.lemmaLower

    val lambdaPos_2 = "l-2:p:"+lambda_2.posTagString
    val lambdaPos_1 = "l-1:p:"+lambda_1.posTagString
    val lambdaPos1 = "l1:p:"+lambda1.posTagString
    val lambdaPos2 = "l2:p:"+lambda2.posTagString
    val betaPos_1 = "b-1:p:"+beta_1.posTagString
    val betaPos1 = "b1:p:"+beta1.posTagString

    val stackPos_2 = "s-2:p:"+stack_2.posTagString
    val stackPos_1 = "s-1:p:"+stack_1.posTagString
    val betaPos_2 = "b-2:p:"+beta_2.posTagString
    val betaPos2 = "b2:p:"+beta2.posTagString
    val betaPos3 = "b3:p:"+beta3.posTagString

    addFeature(featureVariable, stackLemma_1)
    addFeature(featureVariable, lambdaLemma_1)
    addFeature(featureVariable, lambdaLemma1)
    addFeature(featureVariable, betaLemma_2)
    addFeature(featureVariable, betaLemma_1)
    addFeature(featureVariable, betaLemma1)
    addFeature(featureVariable, betaLemma2)

    //    addFeature(featureVariable, stackPos_1)
    addFeature(featureVariable, lambdaPos_2)
    addFeature(featureVariable, lambdaPos_1)
    addFeature(featureVariable, lambdaPos1)
    addFeature(featureVariable, lambdaPos2)
    addFeature(featureVariable, betaPos_1)
    addFeature(featureVariable, betaPos1)

    // 2-gram features
    addConjunctiveFeature(featureVariable, Array(betaPos1, lambdaPos))
    addConjunctiveFeature(featureVariable, Array(stackPos_1, lambdaPos))
    addConjunctiveFeature(featureVariable, Array(stackPos_1, betaPos))

    addConjunctiveFeature(featureVariable, Array(betaLemma_1, lambdaLemma))
    addConjunctiveFeature(featureVariable, Array(betaPos1, lambdaLemma))
    addConjunctiveFeature(featureVariable, Array(betaPos1, betaLemma))

    addConjunctiveFeature(featureVariable, Array(betaLemma1, lambdaPos))
    addConjunctiveFeature(featureVariable, Array(betaLemma1, betaPos))

    addConjunctiveFeature(featureVariable, Array(lambdaLemma1, lambdaLemma))
    addConjunctiveFeature(featureVariable, Array(lambdaLemma1, betaLemma))

    // 3-gram features
    addConjunctiveFeature(featureVariable, Array(lambdaPos_2, lambdaPos_1, lambdaPos))
    addConjunctiveFeature(featureVariable, Array(lambdaPos_1, lambdaPos1, lambdaPos))
    addConjunctiveFeature(featureVariable, Array(lambdaPos1, lambdaPos2, lambdaPos))
    addConjunctiveFeature(featureVariable, Array(betaPos_1, betaPos1, betaPos))
    addConjunctiveFeature(featureVariable, Array(betaPos1, betaPos2, betaPos))

    addConjunctiveFeature(featureVariable, Array(stackPos_2, lambdaPos, betaPos))
    addConjunctiveFeature(featureVariable, Array(lambdaPos_1, lambdaPos, betaPos))

    addConjunctiveFeature(featureVariable, Array(lambdaPos1, lambdaPos, betaPos)) // not in new
    addConjunctiveFeature(featureVariable, Array(betaPos_2, lambdaPos, betaPos))
    addConjunctiveFeature(featureVariable, Array(betaPos_1, lambdaPos, betaPos))
    addConjunctiveFeature(featureVariable, Array(betaPos1, lambdaPos, betaPos))
    addConjunctiveFeature(featureVariable, Array(betaPos2, lambdaPos, betaPos))
    addConjunctiveFeature(featureVariable, Array(betaPos3, lambdaPos, betaPos))

    // 2nd order features
    val lambdaHeadIndex = if(lambdaIndex > -1) state.headIndices(lambdaIndex) else -1
    val lambdaArcLabel = if(lambdaIndex > -1) state.arcLabels(lambdaIndex) else ParserConstants.NULL_STRING
    val lambdaHeadToken = state.sentence(lambdaHeadIndex)

    val lambdaLeftmostDepIndex = if(lambdaIndex > -1) state.leftmostDependent(lambdaIndex) else -1
    val lambdaRightmostDepIndex = if(lambdaIndex > -1) state.rightmostDependent(lambdaIndex) else -1
    val betaLeftmostDepIndex = if(betaIndex > -1) state.leftmostDependent(betaIndex) else -1
    val lambdaLeftNearestSibIndex = if(lambdaIndex > -1) state.leftNearestSibling(lambdaIndex) else -1

    val lambdaLeftmostDep = state.sentence(lambdaLeftmostDepIndex)
    val lambdaRightmostDep = state.sentence(lambdaRightmostDepIndex)
    val betaLeftmostDep = state.sentence(betaLeftmostDepIndex)
//    val lambdaLeftNearestSib = state.sentence(lambdaLeftNearestSibIndex)

    val lambdaHeadLemma = "l_h:m:" + lambdaHeadToken.lemmaLower
    val lambdaHeadPos = "l_h:p:" + lambdaHeadToken.posTagString
    val lambdaHeadLabel = "l:d:" + lambdaArcLabel
    val lambdaLeftmostDepLemma = "l_lmd:m:"+lambdaLeftmostDep.lemmaLower
    val lambdaRightmostDepLemma = "l_rmd:m:"+lambdaRightmostDep.lemmaLower
    val betaLeftmostDepLemma = "b_lmd:m:"+betaLeftmostDep.lemmaLower
    val lambdaLeftmostDepPos = "l_lmd:p:"+lambdaLeftmostDep.posTagString
    val lambdaRightmostDepPos = "l_rmd:p:"+lambdaRightmostDep.posTagString
    val betaLeftmostDepPos = "b_lmd:p:"+betaLeftmostDep.posTagString

    val lambdaLeftmostDepHeadLabel = "l_lmd:d:" + (if(lambdaLeftmostDepIndex > -1 && state.headIndices(lambdaLeftmostDepIndex) > -1) state.arcLabels(lambdaLeftmostDepIndex) else ParserConstants.NULL_STRING)
    val lambdaRightmostDepHeadLabel = "l_rmd:d:" + (if(lambdaRightmostDepIndex > -1 && state.headIndices(lambdaRightmostDepIndex) > -1) state.arcLabels(lambdaRightmostDepIndex) else ParserConstants.NULL_STRING)
    val lambdaLeftNearestSibHeadLabel = "l_lns:d:" + (if(lambdaLeftNearestSibIndex > -1 && state.headIndices(lambdaLeftNearestSibIndex) > -1) state.arcLabels(lambdaLeftNearestSibIndex) else ParserConstants.NULL_STRING)

    addFeature(featureVariable, lambdaHeadLemma)
    addFeature(featureVariable, lambdaLeftmostDepLemma)
    addFeature(featureVariable, lambdaRightmostDepLemma)
    addFeature(featureVariable, betaLeftmostDepLemma)

    addFeature(featureVariable, lambdaHeadPos)
    addFeature(featureVariable, lambdaLeftmostDepPos)
    addFeature(featureVariable, lambdaRightmostDepPos)
    addFeature(featureVariable, betaLeftmostDepPos)

    addFeature(featureVariable, lambdaHeadLabel)
    addFeature(featureVariable, lambdaLeftmostDepHeadLabel)
    addFeature(featureVariable, lambdaRightmostDepHeadLabel)

    addConjunctiveFeature(featureVariable, Array(lambdaHeadLabel, betaLemma))

    addConjunctiveFeature(featureVariable, Array(lambdaLeftmostDepPos, lambdaPos, betaPos))
    addConjunctiveFeature(featureVariable, Array(lambdaRightmostDepPos, lambdaPos, betaPos))
    addConjunctiveFeature(featureVariable, Array(betaLeftmostDepPos, lambdaPos, betaPos))
    addConjunctiveFeature(featureVariable, Array(lambdaLeftNearestSibHeadLabel, lambdaPos, betaPos))

    // 3rd order features
//    val (lambdaGrandHead, lambdaGrandHeadTok) = if(lambdaTok.hasGrandHead) (Some(lambdaTok.grandHead), Some(lambdaTok.grandHead.depToken)) else (NULL, NULL)
    val lambdaGrandHeadIdx = if(lambdaHeadIndex > -1) state.headIndices(lambdaHeadIndex) else -1
    val lambdaGrandHeadToken = state.sentence(lambdaGrandHeadIdx)

    val lambdaLeftmostDep2Index = if(lambdaIndex > -1) state.leftmostDependent2(lambdaIndex) else -1
    val lambdaRightmostDep2Index = if(lambdaIndex > -1) state.rightmostDependent2(lambdaIndex) else -1
    val betaLeftmostDep2Index = if(betaIndex > -1) state.leftmostDependent2(betaIndex) else -1

    val lambdaLeftmostDep2 = state.sentence(lambdaLeftmostDep2Index)
    val lambdaRightmostDep2 = state.sentence(lambdaRightmostDep2Index)
    val betaLeftmostDep2 = state.sentence(betaLeftmostDep2Index)

    val lambdaGrandHeadLemma = "l_h2:m:" + lambdaGrandHeadToken.lemmaLower
    val lambdaGrandHeadPos =  "l_h2:p:" + lambdaGrandHeadToken.posTagString

    val lambdaLeftmostDep2Lemma = "l_lmd2:m:" + lambdaLeftmostDep2.lemmaLower
    val lambdaLeftmostDep2Pos = "l_lmd2:p:" + lambdaLeftmostDep2.posTagString
    val lambdaRightmostDep2Lemma = "l_rmd2:m:" + lambdaRightmostDep2.lemmaLower
    val lambdaRightmostDep2Pos = "l_rmd2:p:" + lambdaRightmostDep2.posTagString
    val betaLeftmostDep2Lemma = "b_lmd2:m:" + betaLeftmostDep2.lemmaLower
    val betaLeftmostDep2Pos = "b_lmd2:p:" + betaLeftmostDep2.posTagString

    val lambdaGrandHeadLabel = "l_h:d:" + (if(lambdaGrandHeadIdx > -1)  state.arcLabels(lambdaGrandHeadIdx) else ParserConstants.NULL_STRING)

    val lambdaLeftmostDep2Label = "l_lmd2:d" + (if(lambdaLeftmostDep2Index > -1 && state.headIndices(lambdaLeftmostDep2Index) > -1) state.arcLabels(lambdaLeftmostDep2Index) else ParserConstants.NULL_STRING)
    val lambdaRightmostDep2Label = "l_rmd2:d" + (if(lambdaRightmostDep2Index > -1 && state.headIndices(lambdaRightmostDep2Index) > -1) state.arcLabels(lambdaRightmostDep2Index) else ParserConstants.NULL_STRING)
    val betaLeftmostDep2Label = "b_lmd2:d" + (if(betaLeftmostDep2Index > -1 && state.headIndices(betaLeftmostDep2Index) > -1) state.arcLabels(betaLeftmostDep2Index) else ParserConstants.NULL_STRING)

    addFeature(featureVariable, lambdaGrandHeadLemma)
    addFeature(featureVariable, lambdaLeftmostDep2Lemma)
    addFeature(featureVariable, lambdaRightmostDep2Lemma)
    addFeature(featureVariable, betaLeftmostDep2Lemma)

    addFeature(featureVariable, lambdaGrandHeadPos)
    addFeature(featureVariable, lambdaLeftmostDep2Pos)
    addFeature(featureVariable, lambdaRightmostDep2Pos)
    addFeature(featureVariable, betaLeftmostDep2Pos)

    addFeature(featureVariable, lambdaGrandHeadLabel)
    addFeature(featureVariable, lambdaLeftmostDep2Label)
    addFeature(featureVariable, lambdaRightmostDep2Label)
    addFeature(featureVariable, betaLeftmostDep2Label)

    addConjunctiveFeature(featureVariable, Array(lambdaLeftmostDep2Pos, lambdaLeftmostDepPos, lambdaPos))
    addConjunctiveFeature(featureVariable, Array(betaLeftmostDep2Pos, betaLeftmostDepPos, betaPos))
    addConjunctiveFeature(featureVariable, Array(lambdaGrandHeadPos, lambdaHeadPos, lambdaPos))

    addConjunctiveFeature(featureVariable, Array(lambdaLeftmostDep2Pos, lambdaPos, betaPos))
    addConjunctiveFeature(featureVariable, Array(betaLeftmostDep2Pos, lambdaPos, betaPos))
    addConjunctiveFeature(featureVariable, Array(lambdaLeftmostDep2Label, lambdaPos, betaPos))

    // binary features
    val lambdaIsLeftmostTok = lambdaIndex == 0
    val betaIsRightmostTok = betaIndex == state.parseSentenceLength - 1
    val lambdaBetaAdjacent = state.input - state.stack == 1

    // make into single
    addConjunctiveFeature(featureVariable, Array(lambdaIsLeftmostTok.toString, betaIsRightmostTok.toString, lambdaBetaAdjacent.toString))
  }

  // Serialization
  def serialize(file: File): Unit = {
    if (file.getParentFile ne null) file.getParentFile.mkdirs()
    serialize(new java.io.FileOutputStream(file))
  }
  def deserialize(file: File): Unit = {
    require(file.exists(), "Trying to load non-existent file: '" +file)
    deserialize(new java.io.FileInputStream(file))
  }
  def serialize(stream: java.io.OutputStream): Unit = {
    import cc.factorie.util.CubbieConversions._
    // Sparsify the evidence weights
    import scala.language.reflectiveCalls
    val sparseEvidenceWeights = new la.DenseLayeredTensor2(FeaturesDomain.dimensionDomain.size, ParseDecisionDomain.size, new la.SparseIndexedTensor1(_))
    model.weights.value.foreachElement((i, v) => if (v != 0.0) sparseEvidenceWeights += (i, v))
    model.weights.set(sparseEvidenceWeights)
    val dstream = new java.io.DataOutputStream(new BufferedOutputStream(stream))
    BinarySerializer.serialize(FeaturesDomain.dimensionDomain, dstream)
    BinarySerializer.serialize(ParseDecisionDomain, dstream)
    BinarySerializer.serialize(model, dstream)
    dstream.close()  // TODO Are we really supposed to close here, or is that the responsibility of the caller?
  }
  def deserialize(stream: java.io.InputStream): Unit = {
    import cc.factorie.util.CubbieConversions._
    // Get ready to read sparse evidence weights
    val dstream = new java.io.DataInputStream(new BufferedInputStream(stream))
    BinarySerializer.deserialize(FeaturesDomain.dimensionDomain, dstream)
    BinarySerializer.deserialize(ParseDecisionDomain, dstream)
    import scala.language.reflectiveCalls
    model.weights.set(new la.DenseLayeredTensor2(FeaturesDomain.dimensionDomain.size, ParseDecisionDomain.size, new la.SparseIndexedTensor1(_)))
    BinarySerializer.deserialize(model, dstream)
    logger.debug("TransitionBasedParser model parameters oneNorm "+model.parameters.oneNorm)
    dstream.close()  // TODO Are we really supposed to close here, or is that the responsibility of the caller?
  }

  def setParse(parseTree: ParseTree, heads: Array[Int], labels: Array[String]) = {
    for(i <- 1 until heads.length){
      val headIndex = heads(i)
      parseTree.setParent(i-1, headIndex-1)
      parseTree.label(i-1).set(ParseTreeLabelDomain.index(labels(i)))(null)
    }
  }

  val parseDecisionCache = JavaHashMap[String,ParseDecision]()
  def getParseDecision(s: String): ParseDecision = parseDecisionCache.getOrElseUpdate(s, new ParseDecision(s))
  def classify(v: ParseDecisionVariable) = {
    computeFeatures(v.state, v.features, addFeatureString, addConjunctiveFeatureWithNulls)
    getParseDecision(ParseDecisionDomain.category(model.predict(v.features.value).maxIndex))
  }
  lazy val model = new LinearMulticlassClassifier(ParseDecisionDomain.size, FeaturesDomain.dimensionSize)

  def testString(testSentences:Seq[Sentence], extraText: String = ""): String = {
    val(las, uas, tokSpeed, sentSpeed) = test(testSentences)
    s"$extraText LAS=$las UAS=$uas ${tokSpeed} tokens/sec ${sentSpeed} sentences/sec"
  }

  def test(testSentences:Seq[Sentence]): (Double, Double, Double, Double) = {
    var i = 0
    val numSentences = testSentences.size
    var t0: Long = 0
    var totalTime: Long = 0
    while(i < numSentences){
      t0 = System.currentTimeMillis()
      process(testSentences(i))
      totalTime += System.currentTimeMillis() - t0
      i += 1
    }
    val totalTokens = testSentences.map(_.length).sum
    val pred = testSentences.map(_.attr[ParseTree])
    (ParserEval.calcLas(pred), ParserEval.calcUas(pred), totalTokens*1000.0/totalTime, numSentences*1000.0/totalTime)
  }

  def train(trainSentences: Seq[Sentence], testSentences: Seq[Sentence], lrate: Double = 1.0, delta: Double = 0.1,
            cutoff: Int = 2, numBoostingIterations: Int = 0, useHingeLoss: Boolean = true, useSVM: Boolean = false,
            nThreads: Int = 1, numIterations: Int = 7, l1Factor: Double = 0.01, l2Factor: Double = 0.0001, debug: Boolean = false)
           (implicit random: scala.util.Random): Double = {

    logger.debug(s"Initializing trainer (${nThreads} threads)")

    val objective = if(useHingeLoss) OptimizableObjectives.hingeMulticlass else OptimizableObjectives.sparseLogMulticlass

    def evaluate() {
      println(model.weights.value.toSeq.count(x => x == 0).toFloat/model.weights.value.length +" sparsity")
      println(testString(trainSentences, "Train "))
      println(testString(testSentences,  "Test "))
    }

    FeaturesDomain.dimensionDomain.gatherCounts = true
    println("Generating decisions...")
    var trainingVs = generateDecisions(trainSentences, ParserConstants.TRAINING, nThreads)
    println(s"Feature count before count cutoff=$cutoff: ${FeaturesDomain.dimensionDomain.size}")
    FeaturesDomain.dimensionDomain.trimBelowCount(cutoff)
    FeaturesDomain.freeze()
    FeaturesDomain.dimensionDomain.gatherCounts = false
    println(s"Feature count after count cutoff=$cutoff: ${FeaturesDomain.dimensionDomain.size}")

    if(cutoff > 1) {
      println("Re-generating decisions after feature count cutoff...")
      trainingVs = null // gc
      trainingVs = generateDecisions(trainSentences, ParserConstants.TRAINING, nThreads)
    }
    ParseDecisionDomain.freeze()
    println(s"Label (decision) domain size: ${ParseDecisionDomain.size}")
    if(debug) ParseDecisionDomain.dimensionDomain.categories.map(c => ParseDecision(c).readableString).foreach(c => println(c))

    /* Print out features */
    if(debug) {
      println(s"Sentence: ${trainSentences.head.tokens.map(_.string).mkString(" ")}")
      trainingVs.head.foreach(tv => {
        println(s"Training decision: ${ParseDecision(ParseDecisionDomain.category(tv.target)).readableString}; features: ${
          tv.features.domain.dimensionDomain.categories.zip(tv.features.value.toSeq).filter(_._2 == 1.0).map(_._1).mkString(" ")
        }")
      })
    }

    val examples = trainingVs.flatten.map(v => new ParseDecisionExample(v, model, objective)).toSeq

    println("Training...")
    val optimizer = new AdaGradRDA(delta=delta, rate=lrate, l1=l1Factor, l2=l2Factor, numExamples=examples.length)
    Trainer.onlineTrain(model.parameters, examples, maxIterations=numIterations, optimizer=optimizer, evaluate=evaluate, useParallelTrainer = if(nThreads > 1) true else false, nThreads=nThreads)
    println("Done training")

    println(testString(testSentences,  "Test "))

    val(las, uas, tokPerSec, sentPerSec) = test(testSentences)
    las
  }

  def generateDecisions(sentences: Iterable[Sentence], mode: Int, nThreads: Int): Array[Array[ParseDecisionVariable]] = {
    // parallelizing this will lead to non-repeatable results
    // since the order of examples will be different every time
    val oracle = new NonProjectiveShiftReduce(if (mode == ParserConstants.TRAINING) NonprojectiveGoldOracle.predict else new NonprojectiveBoostingOracle(classify).predict)
    def genDecisions(s: Sentence) = oracle.getParseDecisions(new LightweightParseSentence(s)).toArray
    (if(nThreads > 1) cc.factorie.util.Threading.parMap(sentences, nThreads)(genDecisions)
    else sentences.map(genDecisions)).toArray
  }

  // For DocumentAnnotator trait
  def process(doc: Document) = { doc.sentences.foreach(process); doc }
  def prereqAttrs = Seq(classOf[Sentence], classOf[PosTag], classOf[TokenLemma]) // Sentence also includes Token
  def postAttrs = Seq(classOf[ParseTree])
  override def tokenAnnotationString(token:Token): String = {
    val sentence = token.sentence
    val pt = if (sentence ne null) sentence.attr[ParseTree] else null
    if (pt eq null) "_\t_"
    else (pt.parentIndex(token.positionInSentence)+1).toString+"\t"+pt.label(token.positionInSentence).categoryValue
  }

  def process(s: Sentence): Sentence = {
    val parseTree = s.attr.getOrElseUpdate(new ParseTree(s))
    val (heads, labels) = predictingNPSR.parse(new LightweightParseSentence(s))
    setParse(parseTree, heads, labels)
    s
  }

  /* Takes features and turns them into a parse decision using predict(ParseDecisionVariable => ParseDecision) */
  class NonProjectiveShiftReduce(val predict: ParseDecisionVariable => ParseDecision) {
    import ParserConstants._

    def getParseDecisions(s: LightweightParseSentence): ArrayBuffer[ParseDecisionVariable] = {
      val state = new ParseState(0, 1, JavaHashSet[Int](), s)
      val decisions = new ArrayBuffer[ParseDecisionVariable] { override val initialSize = 100 }
      while(state.input < state.parseSentenceLength) {
        if (state.stack < 0)
          noShift(state)
        else {
          val decisionVariable = new ParseDecisionVariable(state)
          val label = predict(decisionVariable)
          decisions += decisionVariable
          transition(state, label)
        }
      }
      decisions
    }
    
    def parse(s: LightweightParseSentence): (Array[Int], Array[String]) = {
      val state = new ParseState(0, 1, JavaHashSet[Int](), s)
      while(state.input < state.parseSentenceLength) {
        if (state.stack < 0)
          noShift(state)
        else {
          val decision = new ParseDecisionVariable(state)
          val label = predict(decision)
          transition(state, label)
        }
      }
      (state.headIndices, state.arcLabels)
    }

    private def transition(state: ParseState, label: ParseDecision) = {
      if (label.leftOrRightOrNo == LEFT) {
        if (state.stack == ROOT_ID) noShift(state)
        else if (state.isDescendantOf(state.inputToken(0), state.stackToken(0))) noPass(state)
        else if (label.shiftOrReduceOrPass == REDUCE) leftReduce(label.label, state)
        else leftPass(label.label, state)
      }
      else if (label.leftOrRightOrNo == RIGHT) {
        if (state.isDescendantOf(state.stackToken(0), state.inputToken(0))) noPass(state)
        else if (label.shiftOrReduceOrPass == SHIFT) rightShift(label.label, state)
        else rightPass(label.label, state)
      }
      else {
        if (label.shiftOrReduceOrPass == SHIFT) noShift(state)
        else if (label.shiftOrReduceOrPass == REDUCE && state.headIndices(state.stackToken(0)) != -1) noReduce(state)
        else noPass(state)
      }
    }

    private def passAux(state: ParseState): Unit = {
      var i = state.stack - 1
      while (i >= 0) {
        if (!state.reducedIds.contains(i)) {
          state.stack = i
          return
        }
        i -= 1
      }
      state.stack = i
    }

    private def leftArc(label: String, state: ParseState)  { state.setHead(state.stackToken(0), state.inputToken(0), label) }
    private def rightArc(label: String, state: ParseState) { state.setHead(state.inputToken(0), state.stackToken(0), label) }

    private def shift(state: ParseState)  { state.stack = state.input; state.input += 1 }
    private def reduce(state: ParseState) { state.reducedIds.add(state.stack); passAux(state) }
    private def pass(state: ParseState)   { passAux(state: ParseState) }

    private def noShift(state: ParseState)  { shift(state) }
    private def noReduce(state: ParseState) { reduce(state) }
    private def noPass(state: ParseState)   { pass(state) }
    private def leftReduce(label: String, state: ParseState) { leftArc(label, state);  reduce(state) }
    private def leftPass(label: String, state: ParseState)   { leftArc(label, state);  pass(state)   }
    private def rightShift(label: String, state: ParseState) { rightArc(label, state); shift(state)  }
    private def rightPass(label: String, state: ParseState)  { rightArc(label, state); pass(state)   }
  }

  trait NonProjectiveOracle {
    import ParserConstants._
    def predict(decisionVariable: ParseDecisionVariable): ParseDecision

    def getGoldDecision(state: ParseState): String = {
      val goldLRN = getGoldLRN(state)
      val shiftOrReduceOrPass =
        goldLRN match {
          case LEFT  => if (shouldGoldReduce(hasHead=true, state=state)) REDUCE else PASS
          case RIGHT => if (shouldGoldShift(state=state)) SHIFT else PASS
          case _ => {
            if (shouldGoldShift(state=state)) SHIFT
            else if (shouldGoldReduce(hasHead=false, state=state)) REDUCE
            else PASS
          }
        }
      goldLRN + " " + shiftOrReduceOrPass + " " + getGoldLabel(state)
    }

    def getGoldLabel(state: ParseState): String = {
      if (state.goldHeads(state.stack) == state.input) state.goldLabels(state.stack)
      else if (state.goldHeads(state.input) == state.stack) state.goldLabels(state.input)
      else ParseDecisionDomain.defaultLabel
    }

    def getGoldLRN(state: ParseState): Int = {
      if (state.goldHeads(state.stack) == state.input) LEFT
      else if (state.goldHeads(state.input) == state.stack) RIGHT
      else NO
    }

    def shouldGoldShift(state: ParseState): Boolean = {
      if (state.goldHeads(state.input) < state.stack) return false
      else
        for (i <- (state.stack - 1) until 0 by -1) if (!state.reducedIds.contains(i)) {
          if (state.goldHeads(i) == state.input)
            return false
        }
      true
    }

    def shouldGoldReduce(hasHead: Boolean, state: ParseState): Boolean = {
      if (!hasHead && state.headIndices(state.stackToken(0)) == -1)
        return false
      for (i <- (state.input + 1) until state.parseSentenceLength)
        if (state.goldHeads(i) == state.stack)
          return false
      true
    }
  }

  object NonprojectiveGoldOracle extends NonProjectiveOracle {
    def predict(decisionVariable: ParseDecisionVariable): ParseDecision = {
      val decision = getGoldDecision(decisionVariable.state)
      computeFeatures(decisionVariable.state, decisionVariable.features, addFeatureString, addConjunctiveFeatureWithNulls)
      decisionVariable.target = ParseDecisionDomain.index(decision)
      decisionVariable.setCategory(decision)(null)
      getParseDecision(decision)
    }
  }

  class NonprojectiveBoostingOracle(basePredict: ParseDecisionVariable => ParseDecision) extends NonProjectiveOracle {
    def predict(decisionVariable: ParseDecisionVariable): ParseDecision = {
      val decision = getGoldDecision(decisionVariable.state)
      computeFeatures(decisionVariable.state, decisionVariable.features, addFeatureString, addConjunctiveFeatureWithNulls)
      decisionVariable.target = ParseDecisionDomain.index(decision)
      basePredict(decisionVariable)
    }
  }
}

class WSJTransitionBasedParser(url:java.net.URL) extends TransitionBasedParser(url)
object WSJTransitionBasedParser extends WSJTransitionBasedParser(cc.factorie.util.ClasspathURL[WSJTransitionBasedParser](".factorie"))

class OntonotesTransitionBasedParser(url:java.net.URL) extends TransitionBasedParser(url)
object OntonotesTransitionBasedParser extends OntonotesTransitionBasedParser(cc.factorie.util.ClasspathURL[OntonotesTransitionBasedParser](".factorie"))

class TransitionBasedParserArgs extends cc.factorie.util.DefaultCmdOptions with SharedNLPCmdOptions{
  val trainFiles =  new CmdOption("train", Nil.asInstanceOf[List[String]], "FILENAME...", "")
  val testFiles =  new CmdOption("test", Nil.asInstanceOf[List[String]], "FILENAME...", "")
  val trainDir = new CmdOption("trainDir", "", "FILENAME", "Directory containing training files.")
  val testDir = new CmdOption("testDir", "", "FILENAME", "Directory containing test files.")
  val devDir = new CmdOption("devDir", "", "FILENAME", "Directory containing dev files.")
  val devFiles = new CmdOption("dev", Nil.asInstanceOf[List[String]], "FILENAME...", "")
  val dataLoader = new CmdOption("loader", "LoadOntonotes5", "STRING", "Class name of data loader to use")
  val cutoff    = new CmdOption("cutoff", 2, "", "")
  val loadModel = new CmdOption("load", "", "", "")
  val nThreads =  new CmdOption("num-threads", 1, "INT", "How many threads to use during training.")
  val useSVM =    new CmdOption("use-svm", false, "BOOL", "Whether to use SVMs to train")
  val modelDir =  new CmdOption("model", "model", "FILENAME", "File in which to save the trained model.")
  val boosting = new CmdOption("bootstrapping", 0, "INT", "The number of bootstrapping iterations to use. 0 means no bootstrapping.")
  val saveModel = new CmdOption("save-model", true,"BOOLEAN","whether to write out a model file or not")
  val l1 = new CmdOption("l1", 0.01, "FLOAT", "l1 regularization weight")
  val l2 = new CmdOption("l2", 0.00001, "FLOAT", "l2 regularization weight")
  val rate = new CmdOption("rate", 1.0,"FLOAT","base learning rate")
  val maxIters = new CmdOption("max-iterations", 7, "INT", "Number of passes through data during training")
  val delta = new CmdOption("delta", 0.1,"FLOAT","learning rate delta")
  val hingeLoss = new CmdOption("hinge", true, "BOOLEAN", "Whether to use hinge or log loss")
  val debug = new CmdOption("debug", false, "BOOLEAN", "Whether to print out debugging info for training (generated features)")
}

object TransitionBasedParserTrainer extends cc.factorie.util.HyperparameterMain {
  def evaluateParameters(args: Array[String]) = {
    val opts = new TransitionBasedParserArgs
    implicit val random = new scala.util.Random(0)
    opts.parse(args)

    assert(opts.trainFiles.wasInvoked || opts.trainDir.wasInvoked)

    def loadSentences(listOpt: opts.CmdOption[List[String]], dirOpt: opts.CmdOption[String]): Seq[Sentence] = {
      var fileList = Seq.empty[String]
      if (listOpt.wasInvoked) fileList = listOpt.value.toSeq
      if (dirOpt.wasInvoked) fileList ++= FileUtils.getFileListFromDir(dirOpt.value)
      fileList.flatMap(fname => opts.dataLoader.value match {
        case "LoadWSJMalt" =>
          load.LoadWSJMalt.fromFilename(fname, loadLemma=load.AutoLabel, loadPos=load.AutoLabel, loadParse=load.GoldLabel, loadNer=false, nerBilou=false).head.sentences.toSeq
        case "LoadOntonotes5" =>
          load.LoadOntonotes5.fromFilename(fname, loadLemma=load.AutoLabel, loadPos=load.AutoLabel, loadParse=load.GoldLabel, loadNer=false, nerBilou=false).head.sentences.toSeq
        case "LoadConll2008" =>
          load.LoadConll2008.fromFilename(fname).head.sentences.toSeq
        case l => throw new Error(s"Not configured to load data using $l")
      })
    }

    val sentencesFull = loadSentences(opts.trainFiles, opts.trainDir)
    val devSentencesFull = loadSentences(opts.devFiles, opts.devDir)
    val testSentencesFull = loadSentences(opts.testFiles, opts.testDir)

    val trainPortionToTake = if(opts.trainPortion.wasInvoked) opts.trainPortion.value else 1.0
    val testPortionToTake =  if(opts.testPortion.wasInvoked) opts.testPortion.value else 1.0
    val sentences = sentencesFull.take((trainPortionToTake*sentencesFull.length).floor.toInt)
    val testSentences = testSentencesFull.take((testPortionToTake*testSentencesFull.length).floor.toInt)
    val devSentences = devSentencesFull.take((testPortionToTake*devSentencesFull.length).floor.toInt)

    println("Total train sentences: " + sentences.size)
    println("Total dev sentences: " + devSentences.size)
    println("Total test sentences: " + testSentences.size)

    val parser = new TransitionBasedParser()
    val testLAS = parser.train(sentences, devSentences, lrate=opts.rate.value, delta=opts.delta.value, cutoff=opts.cutoff.value,
                 numBoostingIterations=opts.boosting.value, useHingeLoss=opts.hingeLoss.value, useSVM=opts.useSVM.value,
                 nThreads=opts.nThreads.value, numIterations=opts.maxIters.value, l1Factor=opts.l1.value,
                 l2Factor=opts.l2.value, debug=opts.debug.value)

    if (opts.saveModel.value) {
      val modelUrl: String = if (opts.modelDir.wasInvoked) opts.modelDir.value else opts.modelDir.defaultValue + System.currentTimeMillis().toString + ".factorie"
      parser.serialize(new java.io.File(modelUrl))
      val serParser = new TransitionBasedParser
      serParser.deserialize(new java.io.File(modelUrl))
      println(serParser.testString(devSentences, "Post serialization test accuracy "))
    }
    if(opts.targetAccuracy.wasInvoked) cc.factorie.assertMinimalAccuracy(testLAS,opts.targetAccuracy.value.toDouble)
    testLAS
  }
}

object TransitionBasedParserTester {
  def main(args: Array[String]) {
    val opts = new TransitionBasedParserArgs
    opts.parse(args)
    assert(opts.testDir.wasInvoked || opts.testFiles.wasInvoked)

    // load model from file if given,
    // else if the wsj command line param was specified use wsj model,
    // otherwise ontonotes model
    val parser = {
      if(opts.modelDir.wasInvoked) new TransitionBasedParser(new File(opts.modelDir.value))
      else opts.dataLoader.value match {
        case "LoadWSJMalt" => WSJTransitionBasedParser
        case "LoadOntonotes5" => OntonotesTransitionBasedParser
      }
    }

    assert(!(opts.testDir.wasInvoked && opts.testFiles.wasInvoked))
    val testFileList = if(opts.testDir.wasInvoked) FileUtils.getFileListFromDir(opts.testDir.value) else opts.testFiles.value.toSeq

    val testPortionToTake =  if(opts.testPortion.wasInvoked) opts.testPortion.value else 1.0
    val testSentencesFull = testFileList.flatMap(fname => opts.dataLoader.value match {
      case "LoadWSJMalt" =>
        load.LoadWSJMalt.fromFilename(fname, loadLemma=load.AutoLabel, loadPos=load.AutoLabel, loadParse=load.GoldLabel, loadNer=false, nerBilou=false).head.sentences.toSeq
      case "LoadOntonotes5" =>
        load.LoadOntonotes5.fromFilename(fname, loadLemma=load.AutoLabel, loadPos=load.AutoLabel, loadParse=load.GoldLabel, loadNer=false, nerBilou=false).head.sentences.toSeq
      case "LoadConll2008" =>
        load.LoadConll2008.fromFilename(fname).head.sentences.toSeq
      case l => throw new Error(s"Not configured to load data using $l")
    })
    val testSentences = testSentencesFull.take((testPortionToTake*testSentencesFull.length).floor.toInt)

    println(parser.testString(testSentences))
  }
}

object TransitionBasedParserOptimizer {
  def main(args: Array[String]) {
    val opts = new TransitionBasedParserArgs
    opts.parse(args)
    val actuallySaveModel = opts.saveModel.value
    opts.saveModel.setValue(false) // don't want to save intermediate models, just the best one
    val memory = 24
    val cores = 9
    opts.nThreads.setValue(cores) // make sure we're using the same amount of cores we're allocating

    val l1 = cc.factorie.util.HyperParameter(opts.l1, new cc.factorie.util.LogUniformDoubleSampler(1e-10, 1e2))
    val l2 = cc.factorie.util.HyperParameter(opts.l2, new cc.factorie.util.LogUniformDoubleSampler(1e-10, 1e2))
    val rate = cc.factorie.util.HyperParameter(opts.rate, new cc.factorie.util.LogUniformDoubleSampler(1e-4, 1e4))
    val delta = cc.factorie.util.HyperParameter(opts.delta, new cc.factorie.util.LogUniformDoubleSampler(1e-4, 1e4))
    //    val cutoff = cc.factorie.util.HyperParameter(opts.cutoff, new cc.factorie.util.SampleFromSeq[Int](Seq(1, 2)))
    //    val bootstrap = cc.factorie.util.HyperParameter(opts.bootstrapping, new cc.factorie.util.SampleFromSeq[Int](Seq(0, 1, 2)))
    //    val maxit = cc.factorie.util.HyperParameter(opts.maxIters, new cc.factorie.util.SampleFromSeq[Int](Seq(5, 7)))

    val qs = new cc.factorie.util.QSubExecutor(memory, "cc.factorie.app.nlp.parse.TransitionBasedParserTrainer", cores)
    val optimizer = new cc.factorie.util.HyperParameterSearcher(opts, Seq(l2, rate, delta), qs.execute, 100, 100, 60)

    val result = optimizer.optimize()
    println("Got results: " + result.mkString(" "))
    if(actuallySaveModel) {
      opts.saveModel.setValue(true)
      println("Running best configuration...")
      import scala.concurrent.duration._
      Await.result(qs.execute(opts.values.flatMap(_.unParse).toArray), 2.hours)
    }
    println("Done")
  }
}
