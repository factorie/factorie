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
package cc.factorie.app.nlp.coref

import scala.collection.mutable

class CorefOptions {
  import ConjunctionOptions._
  def getConfigHash = mutable.HashMap[String,String]() ++= configHash.map(x => (x._1,x._2.toString))

  def setConfigHash(H: mutable.HashMap[String,String]): Unit = {
    H.foreach(x => {
      configHash(x._1 ) = x._2.toBoolean
    })
  }

  def setConfig(key:String,value: Boolean) = {configHash(key) = value}
  protected val configHash = mutable.HashMap[String,Boolean]() ++= Seq(("useEntityType",true),("usePronounRules",false),("trainSeparatePronounWeights",false))
  def useEntityType = configHash("useEntityType")
  def trainSeparatePronounWeights = configHash("trainSeparatePronounWeights")
  def usePronounRules = configHash("usePronounRules")

  var maxPronDist = 6
  var maxMentDist = 1000

  var mergeMentionWithApposition = false
  var useAdaGradRDA = true
  var useAverageIterate = false
  var numTrainingIterations = 5
  var trainPortionForTest = 0.1
  var useEntityLR = false
  var numCompareToTheLeft = 75 //make this smaller for the purpose of when you want coref to be faster by not looking at too many things to the left
  var useExactEntTypeMatch = true

  var mergeFeaturesAtAll = true

  var slackRescale = 2.0

  var conjunctionStyle = NO_CONJUNCTIONS

  var featureSet = "conventional"

  var saveFrequency = 1

  var numThreads = 2
  var featureComputationsPerThread = 2

  var useGoldBoundaries = false

  var lexiconDir = "factorie-nlp-resources/src/main/resources/cc/factorie/app/nlp/lexicon/"

  var allowPosCataphora = false // allow cataphora in training?
  var allowNegCataphora = false
  var allowTestCataphora = false // allow cataphora in test?

  var pruneNegTrain = true // setting to true means only one positive pair is created for each mention, compatible with uiuc implementation
  var numPositivePairsTrain = 75 // number of positive pairs to create before stopping examining further back
  var pruneNegTest = false // setting to true means only one positive pair is created for each mention, compatible with uiuc implementation
  var numPositivePairsTest = 100
  var l1 = .0001

  var mentionAlignmentShiftWidth = 0
  var useNERMentions = false
  var learningRate = 1.0


  def setParameterOptions(opts:ForwardCorefTrainerOpts){
    useNERMentions = opts.useNerMentions.value
    useAverageIterate = opts.useAverageIterate.value
    numTrainingIterations = opts.numTrainingIterations.value
    trainPortionForTest = opts.trainPortionForTest.value
    useEntityLR = opts.entityLR.value
    saveFrequency =  opts.saveFrequency.value
    numThreads = opts.numThreads.value
    featureComputationsPerThread = opts.featureComputationsPerThread.value
    pruneNegTrain = opts.numPositivePairsTrain.value > 0
    pruneNegTest = opts.numPositivePairsTest.value > 0
    numPositivePairsTrain = opts.numPositivePairsTrain.value
    numPositivePairsTest = opts.numPositivePairsTest.value
    useExactEntTypeMatch = opts.useExactEntTypeMatch.value
    slackRescale = opts.slackRescale.value
    mentionAlignmentShiftWidth = opts.mentionAlignmentShiftWidth.value
    useGoldBoundaries = opts.useGoldBoundaries.value
    mergeMentionWithApposition = opts.mergeAppositions.value
    setConfig("usePronounRules",opts.usePronounRules.value)
    numCompareToTheLeft = opts.numCompareToTheLeft.value
    // options still in flux
    mergeFeaturesAtAll = opts.mergeFeaturesAtAll.value
    learningRate = opts.learningRate.value
    conjunctionStyle = conjunctionStyle
  }

}

object ConjunctionOptions {
  val NO_CONJUNCTIONS = 1
  val HASH_CONJUNCTIONS = 2
  val SLOW_CONJUNCTIONS = 3
  val PRON_CONJUNCTIONS = 4
}