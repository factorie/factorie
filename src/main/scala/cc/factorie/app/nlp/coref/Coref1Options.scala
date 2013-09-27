package cc.factorie.app.nlp.coref

import scala.collection.mutable

/**
 * User: apassos
 * Date: 6/27/13
 * Time: 12:20 PM
 */

class Coref1Options {
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

  var mergeMentionWithApposition = false
  var useMIRA = true
  var useAverageIterate = false
  var numTrainingIterations = 5
  var trainPortionForTest = 0.1
  var useEntityLR = false
  var numCompareToTheLeft = 75 //make this smaller for the purpose of when you want coref to be faster by not looking at too many things to the left
  var useExactEntTypeMatch = true

  var mergeFeaturesAtAll = true

  var slackRescale = 2.0

  var conjunctionStyle = NO_CONJUNCTIONS

  var saveFrequency = 1

  var numThreads = 2
  var featureComputationsPerThread = 2

  var useNonGoldBoundaries = false

  var lexiconDir = "factorie-nlp-resources/src/main/resources/cc/factorie/app/nlp/lexicon/"

  var allowPosCataphora = false // allow cataphora in training?
  var allowNegCataphora = false
  var allowTestCataphora = false // allow cataphora in test?
  var pruneNegTrain = true // setting to true means only one positive pair is created for each mention, compatible with uiuc implementation
  var numPositivePairsTrain = 2 // number of positive pairs to create before stopping examining further back
  var pruneNegTest = false // setting to true means only one positive pair is created for each mention, compatible with uiuc implementation
  var numPositivePairsTest = 100

  var mentionAlignmentShiftWidth = 0
  var learningRate = 1.0
}

object ConjunctionOptions {
  val NO_CONJUNCTIONS = 1
  val HASH_CONJUNCTIONS = 2
  val SLOW_CONJUNCTIONS = 3
}