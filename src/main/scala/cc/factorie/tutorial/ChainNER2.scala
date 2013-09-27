/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie.tutorial
import cc.factorie._
import java.io.File
import model._
import variable._
import optimize._
import cc.factorie.infer.{MaximizeByBPChain, InferByBPChain}
import cc.factorie.variable.{BooleanValue, BooleanDomain, ChainLink}

/** Simple, introductory linear-chain CRF for named-entity recognition,
    using FACTORIE's low-level "imperative" language to define model structure.

    Demonstrates model creation, training and testing.
    Overly simple features to not, however, provide very high accuracy.
    See ChainNER3 for a related example with better features.

    @author Andrew McCallum 
*/
object ChainNER2 {

  // The variable classes
  object TokenDomain extends CategoricalVectorDomain[String]
  class Token(val string:String, val label:Label) extends BinaryFeatureVectorVariable[String] {
    def domain = TokenDomain
  }
  object LabelDomain extends CategoricalDomain[String]
  class Label(labelName: String, word: String) extends LabeledCategoricalVariable(labelName) with ChainLink[Label,Sentence] {
    val token = new Token(word, this)
    def domain = LabelDomain
  }
  class Sentence extends variable.Chain[Sentence,Label]
  
  // The model
  val excludeSkipEdges = true
  val model = new TemplateModel with Parameters {
    addTemplates(
      // Bias term on each individual label
      new DotTemplateWithStatistics1[Label] {
        //def statisticsDomains = Tuple1(LabelDomain)
        val weights = Weights(new la.DenseTensor1(LabelDomain.size))
      },
      // Transition factors between two successive labels
      new DotTemplateWithStatistics2[Label, Label] {
        //def statisticsDomains = ((LabelDomain, LabelDomain))
        val weights = Weights(new la.DenseTensor2(LabelDomain.size, LabelDomain.size))
        def unroll1(label: Label) = if (label.hasNext) Factor(label, label.next) else Nil
        def unroll2(label: Label) = if (label.hasPrev) Factor(label.prev, label) else Nil
      },
      // Factor between label and observed token
      new DotTemplateWithStatistics2[Label, Token] {
        //def statisticsDomains = ((LabelDomain, TokenDomain))
        val weights = Weights(new la.DenseTensor2(LabelDomain.size, TokenDomain.dimensionSize))
        def unroll1(label: Label) = Factor(label, label.token)
        def unroll2(token: Token) = Factor(token.label, token)
      },
      new DotTemplate2[Label,Label] /*DotStatistics1[BooleanValue]*/ {
        //def statisticsDomains = Tuple1(BooleanDomain)
        val weights = Weights(new la.DenseTensor1(BooleanDomain.size))
        def unroll1(label: Label) = if (excludeSkipEdges) Nil else for (other <- label.chainAfter; if other.token.string == label.token.string) yield Factor(label, other)
        def unroll2(label: Label) = if (excludeSkipEdges) Nil else for (other <- label.chainBefore; if other.token.string == label.token.string) yield Factor(other, label)
        override def statistics(v1:Label#Value, v2:Label#Value) = BooleanValue(v1.intValue == v2.intValue)
      }
    )
  }
  
  // The training objective
  val objective = HammingObjective
  

  def main(args: Array[String]): Unit = {
    implicit val random = new scala.util.Random(0)
    if (args.length != 2) throw new Error("Usage: ChainNER2 trainfile testfile.")

    // Read in the data
    val trainSentences = load(args(0)).take(1000)
    val testSentences = load(args(1)).take(200)

    // Get the variables to be inferred (for now, just operate on a subset)
    val trainLabels = trainSentences.map(_.links).flatten //.take(10000)
    val testLabels = testSentences.map(_.links).flatten //.take(2000)
    (trainLabels ++ testLabels).foreach(_.setRandomly)
    
    // Train for 5 iterations
    //val pieces = trainLabels.map(l => new SampleRankExample[Variable](l, new GibbsSampler(model, HammingLossObjective)))
    //val trainer = new SGDTrainer[DiffList](new AROW(model), model)
    val pieces = trainSentences.map(s => new LikelihoodExample(s.asSeq, model, InferByBPChain))
    val predictor = MaximizeByBPChain // new VariableSettingsSampler[ChainNerLabel](model, null)
    optimize.Trainer.batchTrain(model.parameters, pieces, evaluate = () => {
      (trainSentences ++ testSentences).foreach(s => predictor(s.asSeq, model))
    })
    println ("Train accuracy = "+ objective.accuracy(trainLabels))
    println ("Test  accuracy = "+ objective.accuracy(testLabels))
  }

  def load(filename:String) : Seq[Sentence] = {
    import scala.io.Source
    import scala.collection.mutable.ArrayBuffer
    val Capitalized = "^[A-Z].*".r
    val Numeric = "^[0-9]+$".r
    val Punctuation = "[-,\\.;:?!()]+".r

    var wordCount = 0
    var sentences = new ArrayBuffer[Sentence]
    val source = Source.fromFile(new File(filename))
    var sentence = new Sentence
    for (line <- source.getLines()) {
      if (line.length < 2) { // Sentence boundary
        sentences += sentence
        sentence = new Sentence
      } else if (line.startsWith("-DOCSTART-")) {
        // Skip document boundaries
      } else {
        val fields = line.split(' ')
        assert(fields.length == 4)
        val word = fields(0)
        val partOfSpeech = fields(1)
        val labelString = fields(3).stripLineEnd
        val label = new Label(labelString, word)
        // Add features to Token
        label.token += "W="+word
        label.token += "SUFFIX3="+word.takeRight(3)
        label.token += "PREFIX3="+word.take(3)
        label.token += "POS="+partOfSpeech
        if (Capitalized.findFirstMatchIn(word) != None) label.token += "CAPITALIZED"
        if (Numeric.findFirstMatchIn(word) != None) label.token += "NUMERIC"
        if (Punctuation.findFirstMatchIn(word) != None) label.token += "PUNCTUATION"
        sentence += label
        wordCount += 1
      }
    }
    println("Loaded "+sentences.length+" sentences with "+wordCount+" words total from file "+filename)
    sentences
  }

}


