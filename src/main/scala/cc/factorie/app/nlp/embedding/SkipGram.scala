/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
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
package cc.factorie.app.nlp.embedding

import java.io.File

import cc.factorie.la.{DenseTensor1, WeightsMapAccumulator}
import cc.factorie.model.Weights
import cc.factorie.util.{DoubleAccumulator, IntArrayBuffer}

import scala.collection.mutable.ArrayBuffer

/**
 * Created by asubbaswamy on 8/17/15.
 */
class SkipGramOptions extends WindowWordEmbedderOptions with IncrementalVocabularyOptions {
  val margin = new CmdOption("margin", 0.1, "DOUBLE", "Margin for WSABIE training.")
  val loss = new CmdOption("loss", "wsabie", "STRING", "Loss function; options are wsabie and log.")
  val browse = new CmdOption("browse", false, "true|false", "If true provide prompt for interatively browsing input embeddings.")
}

object SkipGramExample {
  def apply(model:SkipGram, wordIndices:Array[Int], centerPosition:Int, window:Int): Option[SkipGramExample] = {
    val targetId = wordIndices(centerPosition)
    if (model.discard(targetId)) {     // Skip some of the most common target words
      //println("SkipGramExample skipping "+model.domain.category(targetId))
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
      case "log" => new LogSkipGramExample(model, targetId, context.asArray)
      case "wsabie" => new WsabieSkipGramExample(model, targetId, context.asArray)
      case unk => throw new Error("Unknown loss "+unk)
    }
    if (false && model.random.nextDouble() < 0.005) {
      val start = math.max(centerPosition - window, 0)
      println(s"SkipGramExample raw   ${Range(start, end).map(i => model.domain.category(wordIndices(i))).mkString(" ")}")
      println(s"SkipGramExample       ${model.domain.category(targetId)}    ${Range(0, context.length).map(i => model.domain.category(context(i))).mkString(" ")}")
    }
    Some(result)
  }
}

trait SkipGramExample extends WindowWordEmbedderExample {
  def targetId: Int
}

class LogSkipGramExample(val model:SkipGram, val targetId:Int, val outputIndices:Array[Int]) extends SkipGramExample {
  val changedWeights = new ArrayBuffer[Weights]
  def inputIndices: Array[Int] = Array(targetId)
  val samples = model.makeNegativeSamples // Do this once up front so that Example.testGradient will work

  def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator): Unit = {
    var i = 0
    val len = outputIndices.length
    while (i < len) { //for every word in the context...
    val index = outputIndices(i)
      var targetEmbedding = model.inputEmbedding(targetId) //in skip-gram the "target" is the input
      val contextEmbedding = new DenseTensor1(model.dims)
      contextEmbedding += model.outputEmbedding(index)

      if (model.opts.normalizeX.value) contextEmbedding *= (1.0 / len)

      var score = targetEmbedding dot contextEmbedding
      var expScore = math.exp(-score)

      if (gradient ne null) {
        //positive example
        var stepSize = expScore/(1.0 + expScore)
        gradient.accumulate(model.inputWeights(targetId), contextEmbedding, stepSize)
        gradient.accumulate(model.outputWeights(index), targetEmbedding, stepSize)

        //negative examples
        for (n <- 0 until model.opts.negative.value) {
          val falseTarget = samples(n)
          targetEmbedding = model.inputEmbedding(falseTarget)
          score = targetEmbedding dot contextEmbedding
          expScore = math.exp(-score)
          stepSize = -1.0 / (1.0 + expScore)
          gradient.accumulate(model.inputWeights(falseTarget), contextEmbedding, stepSize)
          gradient.accumulate(model.outputWeights(index), targetEmbedding, stepSize)
        }
      }
      i += 1
    }
  }
}

class WsabieSkipGramExample(val model:SkipGram, val targetId:Int, val outputIndices:Array[Int]) extends SkipGramExample {
  val changedWeights = new ArrayBuffer[Weights]
  def inputIndices: Array[Int] = Array(targetId)
  val samples = model.makeNegativeSamples // Do this once up front so that Example.testGradient will work

  def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator): Unit = {
    var i = 0
    val len = outputIndices.length

    while (i < len) { //for each context index
    val index = outputIndices(i)
      val trueTargetEmbedding = model.inputEmbedding(targetId)
      val contextEmbedding = new DenseTensor1(model.dims)
      contextEmbedding += model.outputEmbedding(index)

      val inputNormalizer = if (model.opts.normalizeX.value) 1.0 / math.sqrt(len) else 1.0
      if (inputNormalizer != 1.0) contextEmbedding *= inputNormalizer // Normalize the input embedding

      val trueScore = trueTargetEmbedding dot contextEmbedding

      for (s <- samples) {
        val falseTargetId = s
        val falseTargetEmbedding = model.inputEmbedding(falseTargetId)
        val falseScore = falseTargetEmbedding dot contextEmbedding

        val objective = trueScore - falseScore - model.opts.margin.value

        if (objective < 0.0) {
          if (value ne null) value.accumulate(objective)
          if (gradient ne null) {
            gradient.accumulate(model.inputWeights(targetId), contextEmbedding, inputNormalizer)
            gradient.accumulate(model.inputWeights(falseTargetId), contextEmbedding, -inputNormalizer)

            val trueFalseEmbeddingDiff = trueTargetEmbedding - falseTargetEmbedding
            gradient.accumulate(model.outputWeights(index), trueFalseEmbeddingDiff, inputNormalizer)
          }
        }
      }

      i += 1
    }
  }
}

class SkipGram(override val opts:SkipGramOptions) extends WordEmbedder(opts) {
  def newExample(model:WordEmbedder, wordIndices:Array[Int], centerPosition:Int, window:Int): Option[SkipGramExample] = SkipGramExample(model.asInstanceOf[SkipGram], wordIndices, centerPosition, window)
}

object SkipGram {
  def main(args: Array[String]) {
    val opts = new SkipGramOptions
    opts.parse(args)
    val skipgram = if (opts.incrementalVocabMaxSize.wasInvoked) new SkipGram(opts) with IncrementalVocabulary else new SkipGram(opts)
    if (opts.trainInput.wasInvoked) {
      skipgram.train(opts.trainInput.value.map(new File(_)))
      if (opts.separateIO.value) {
        skipgram.writeInputEmbeddings("input_embeddings.txt")
        skipgram.writeOutputEmbeddings("output_embeddings.txt")
      }
      else skipgram.writeInputEmbeddings("embeddings.txt")
      if (opts.incrementalVocabMaxSize.wasInvoked) skipgram.writeVocabulary(opts.vocabulary.value)
    } else if (opts.vocabInput.wasInvoked) {
      skipgram.buildVocabulary(opts.vocabInput.value)
    } else if (opts.browse.wasInvoked) {
      skipgram.browse()
    } else {
      println("Either option --train-input or --vocab-input or --browse is required.")
      System.exit(-1)
    }
    println("SkipGram.main done.")
  }
}
