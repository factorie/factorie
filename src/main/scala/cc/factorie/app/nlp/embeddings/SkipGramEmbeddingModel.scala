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
package cc.factorie.app.nlp.embeddings
import cc.factorie.la.WeightsMapAccumulator
import cc.factorie.optimize.Example
import cc.factorie.util.DoubleAccumulator

class SkipGramNegSamplingEmbeddingModel(override val opts: EmbeddingOpts) extends WordEmbeddingModel(opts) {
  val negative = opts.negative.value
  val window = opts.window.value
  val rng = new util.Random(5) // set rng seed
  val sample = opts.sample.value.toDouble
  override def process(doc: String): Int = {
    // given a document, below line splits by space and converts each word to Int (by vocab.getId) and filters out words not in vocab
    var sen = doc.stripLineEnd.split(' ').map(word => vocab.getId(word)).filter(id => id != -1)
    val wordCount = sen.size

    // subsampling -> speed increase 
    if (sample > 0)
      sen = sen.filter(id => subSample(id) != -1)

    val senLength = sen.size
    for (senPosition <- 0 until senLength) {
      val currWord = sen(senPosition)
      val b = rng.nextInt(window)
      // make the contexts
      val contexts = new collection.mutable.ArrayBuffer[Int]
      for (a <- b until window * 2 + 1 - b) if (a != window) {
        val c = senPosition - window + a
        if (c >= 0 && c < senLength)
          contexts += sen(c)
      }
      // make the examples
      contexts.foreach(context => {
        trainer.processExample(new SkipGramNegSamplingExample(this, currWord, context, 1))
        (0 until negative).foreach(neg => trainer.processExample(new SkipGramNegSamplingExample(this, currWord, vocab.getRandWordId, -1)))
      })
    }
    return wordCount
  }

  def subSample(word: Int): Int = {
    val prob = vocab.getSubSampleProb(word) // pre-computed to avoid sqrt call every time.
    val alpha = rng.nextInt(0xFFFF) / 0xFFFF.toDouble
    if (prob < alpha) { return -1 }
    else return word
  }
}
class SkipGramNegSamplingExample(model:WordEmbeddingModel, word: Int, context: Int, label: Int) extends Example {

  // to understand the gradient and objective refer to : http://arxiv.org/pdf/1310.4546.pdf
  def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator): Unit = {
    val wordEmbedding = model.weights(word).value // access the word's embedding 
    val contextEmbedding = model.weights(context).value // access the context's embedding 
    val score: Double = wordEmbedding.dot(contextEmbedding)
    val exp: Double = math.exp(-score) // TODO : pre-compute exp table
    var objective: Double = 0.0
    var factor: Double = 0.0
    if (label == 1) {
      objective = -math.log1p(exp)
      factor = exp / (1 + exp)
    }
    if (label == -1) {
      objective = -score - math.log1p(exp)
      factor = -1 / (1 + exp)
    }
    if (value ne null) value.accumulate(objective)
    if (gradient ne null) {
      gradient.accumulate(model.weights(word), contextEmbedding, factor)
      gradient.accumulate(model.weights(context), wordEmbedding, factor)
    }

  }
}

