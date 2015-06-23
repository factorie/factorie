package cc.factorie.app.nlp.embedding
import cc.factorie.variable.CategoricalDomain
import cc.factorie.model._
import cc.factorie.la._
import cc.factorie.optimize._
import cc.factorie.util.{IntArrayBuffer}
import scala.util.Random
import java.io._
import cc.factorie.util.DoubleAccumulator
import scala.collection.mutable.{ArrayOps,ArrayBuffer}
import java.util.zip.GZIPOutputStream

class CBOWOptions extends WindowWordEmbedderOptions with IncrementalVocabularyOptions {
  val margin = new CmdOption("margin", 0.1, "DOUBLE", "Margin for WSABIE training.")
  val loss = new CmdOption("loss", "wsabie", "STRING", "Loss function; options are wsabie and log.")
}

object CBOWExample {
  def apply(model:CBOW, wordIndices:Array[Int], centerPosition:Int, window:Int): Option[CBOWExample] = {
    val targetId = wordIndices(centerPosition)
    if (model.discard(targetId)) {     // Skip some of the most common target words
      //println("CBOWExample skipping "+model.domain.category(targetId))
      return None
    }
    val context = new IntArrayBuffer(window*2)
    var i = math.max(centerPosition - window, 0)
    val end = math.min(centerPosition + window, wordIndices.length)
    while (i < end) {
      val wi = wordIndices(i)
      // Next line sometimes discards frequent words from context 
      if (i != centerPosition && !model.discard(wi)) context += wi
      i += 1
    }
    if (context.length < model.opts.minContext.value) return None
    val result = model.opts.loss.value match {
      case "log" => new LogCBOWExample(model, targetId, context.asArray)
      case "wsabie" => new WsabieCBOWExample(model, targetId, context.asArray)
      case unk => throw new Error("Unknown loss "+unk)
    }
    if (false && model.random.nextDouble() < 0.005) {
      val start = math.max(centerPosition - window, 0)
      println(s"CBOWExample raw   ${Range(start, end).map(i => model.domain.category(wordIndices(i))).mkString(" ")}")
      println(s"CBOWExample       ${model.domain.category(targetId)}    ${Range(0, context.length).map(i => model.domain.category(context(i))).mkString(" ")}")
    }
    Some(result)
  }
}

trait CBOWExample extends WindowWordEmbedderExample {
  def targetId: Int
}

class LogCBOWExample(val model:CBOW, val targetId:Int, val inputIndices:Array[Int]) extends CBOWExample {
  val changedWeights = new ArrayBuffer[Weights]
  def outputIndices: Array[Int] = Array(targetId)
  val samples = model.makeNegativeSamples // Do this once up front so that Example.testGradient will work
  def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator): Unit = {
    var targetEmbedding = model.outputEmbedding(targetId)
    val contextEmbedding = new DenseTensor1(model.dims)
    val len = inputIndices.length
    var i = 0; while (i < len) { contextEmbedding += model.inputEmbedding(inputIndices(i)); i += 1 }
    if (model.opts.normalizeX.value)
      contextEmbedding *= (1.0 / len)
    //for (i <- start until start+length) contextEmbedding += model.embedding(context(i))
    // Positive case
    var score = targetEmbedding dot contextEmbedding
    var expScore = math.exp(-score)
    // FIXME this log1p is actually really slow and we don't use it for anything!
//    if (value ne null) value.accumulate(-math.log1p(expScore))
    if (gradient ne null) {
      val stepSize = expScore/(1.0 + expScore)
      gradient.accumulate(model.outputWeights(targetId), contextEmbedding, stepSize)
      changedWeights += model.outputWeights(targetId)
      i = 0; while (i < len) { gradient.accumulate(model.inputWeights(inputIndices(i)), targetEmbedding, stepSize); changedWeights += model.inputWeights(inputIndices(i)); i += 1 }
    }
    // Negative case
    for (n <- 0 until model.opts.negative.value) {
      val falseTarget = samples(n)
      targetEmbedding = model.outputEmbedding(falseTarget)
      score = targetEmbedding dot contextEmbedding
      expScore = math.exp(-score)
      // FIXME this log1p is actually really slow and we don't use it for anything!
//      if (value ne null) value.accumulate(-score - math.log1p(expScore))
      if (gradient ne null) {
        val stepSize = -1.0 / (1.0 + expScore)
        gradient.accumulate(model.outputWeights(falseTarget), contextEmbedding, stepSize)
        changedWeights += model.outputWeights(falseTarget)
        i = 0; while (i < len) { gradient.accumulate(model.inputWeights(inputIndices(i)), targetEmbedding, stepSize); i += 1 }
      }
    }
  }
}

class WsabieCBOWExample(val model:CBOW, val targetId:Int, val inputIndices:Array[Int]) extends CBOWExample {
  val changedWeights = new ArrayBuffer[Weights]
  def outputIndices: Array[Int] = Array(targetId)
  val samples = model.makeNegativeSamples // Do this once up front so that Example.testGradient will work
  def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator): Unit = {
    val contextEmbedding = new DenseTensor1(model.dims)
    val len = inputIndices.length
    var i = 0; while (i < len) { contextEmbedding += model.inputEmbedding(inputIndices(i)); i += 1 }
    // TODO FIX this is weird since normalizeX should average the contexts, not project things
    // Also this should only project onto ball, not surface of sphere since nonconvex -luke
    val inputNormalizer = if (model.opts.normalizeX.value) 1.0 / math.sqrt(len) else 1.0 // TODO Should we have this normalization?  In my quick eyeing of results, it looks worse with normalization than without.
    if (inputNormalizer != 1.0) contextEmbedding *= inputNormalizer // Normalize the input embedding
    // Positive case
    val trueTargetEmbedding = model.outputEmbedding(targetId)
    val trueScore = trueTargetEmbedding dot contextEmbedding
    // Negative cases
    for (s <- samples) {
      val falseTargetId = s
      val falseTargetEmbedding = model.outputEmbedding(falseTargetId)
      val falseScore = falseTargetEmbedding dot contextEmbedding
      val objective = trueScore - falseScore - model.opts.margin.value
      if (objective < 0.0) {
        if (value ne null) value.accumulate(objective)
        if (gradient ne null) {
          gradient.accumulate(model.outputWeights(targetId), contextEmbedding, inputNormalizer) //; touchedWeights += model.outputWeights(targetId)
          gradient.accumulate(model.outputWeights(falseTargetId), contextEmbedding, -inputNormalizer) //; touchedWeights += model.outputWeights(falseTargetId)
          val trueFalseEmbeddingDiff = trueTargetEmbedding - falseTargetEmbedding
          i = 0; while (i < len) {
            gradient.accumulate(model.inputWeights(inputIndices(i)), trueFalseEmbeddingDiff, inputNormalizer); changedWeights += model.inputWeights(inputIndices(i))
            i += 1
          }
        }
      }
    }
  }    
}

class CBOW(override val opts:CBOWOptions) extends WordEmbedder(opts) {
  def newExample(model:WordEmbedder, wordIndices:Array[Int], centerPosition:Int, window:Int): Option[CBOWExample] = CBOWExample(model.asInstanceOf[CBOW], wordIndices, centerPosition, window)
}



object CBOW {

  def main(args:Array[String]): Unit = {
    val opts = new CBOWOptions
    opts.parse(args)
    val cbow = if (opts.incrementalVocabMaxSize.wasInvoked) new CBOW(opts) with IncrementalVocabulary else new CBOW(opts)
    if (opts.trainInput.wasInvoked) {
      //Vocabulary.opts.maxWikiPages.setValue(opts.maxWikiPages.value) // temporary kludge
      cbow.train(opts.trainInput.value)
      cbow.writeInputEmbeddings("embeddings.txt")
      if (opts.incrementalVocabMaxSize.wasInvoked) cbow.writeVocabulary(opts.vocabulary.value)
    } else if (opts.vocabInput.wasInvoked) {
      cbow.buildVocabulary(opts.vocabInput.value)
    } else {
      println("Either option --train-input or --vocab-input is required.")
      System.exit(-1)
    }
    println("CBOW.main done.")
  }
  
}
  
