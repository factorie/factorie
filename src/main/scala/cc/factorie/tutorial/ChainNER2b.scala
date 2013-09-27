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
import cc.factorie.optimize._
import java.io.File
import cc.factorie.variable._
import cc.factorie.model.{Parameters, DotTemplateWithStatistics2, DotTemplateWithStatistics1, TemplateModel}
import cc.factorie.infer.{VariableSettingsSampler, GibbsSampler}

/** Simple, introductory linear-chain CRF for named-entity recognition,
    using FACTORIE's low-level "imperative" language to define model structure.

    Demonstrates model creation, training and testing.
    See ChainNER2 for a related example that is simpler, with fewer features.

    @author Andrew McCallum 
*/
object ChainNER2b {

  // The variable classes
  object TokenDomain extends CategoricalVectorDomain[String]
  class Token(val word:String, val label:Label) extends BinaryFeatureVectorVariable[String] {
    def domain = TokenDomain
  }
  object LabelDomain extends CategoricalDomain[String]
  class Label(labelName: String, word: String) extends LabeledCategoricalVariable(labelName) with ChainLink[Label,Sentence] {
    val token = new Token(word, this)
    def domain = LabelDomain
  }
  class Sentence extends Chain[Sentence,Label]
  
  // The model
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
        def unroll1(label: Label) = if (label.hasPrev) Factor(label.prev, label) else Nil
        def unroll2(label: Label) = if (label.hasNext) Factor(label, label.next) else Nil
      },
      // Factor between label and observed token
      new DotTemplateWithStatistics2[Label, Token] {
        //def statisticsDomains = ((LabelDomain, TokenDomain))
        val weights = Weights(new la.DenseTensor2(LabelDomain.size, TokenDomain.dimensionSize))
        def unroll1(label: Label) = Factor(label, label.token)
        def unroll2(token: Token) = throw new Error("Token values shouldn't change")
      }
    )
  }
  
  // The training objective
  val objective = new HammingTemplate[Label]
  

  def main(args: Array[String]): Unit = {
    implicit val random = new scala.util.Random(0)
    if (args.length != 2) throw new Error("Usage: ChainNER2 trainfile testfile\n where files are in CoNLL Shared Task 2003 format.")

    // Read in the data
    val trainSentences = load(args(0))
    val testSentences = load(args(1))

    // Get the variables to be inferred
    val trainLabels = trainSentences.map(_.links).flatten.take(10000)
    val testLabels = testSentences.map(_.links).flatten.take(2000)
    val allTokens: Seq[Token] = (trainLabels ++ testLabels).map(_.token)

    // Add features from next and previous tokens
    // println("Adding offset features...")
    allTokens.foreach(t => {
      if (t.label.hasPrev) t ++= t.label.prev.token.activeCategories.filter(!_.contains('@')).map(_+"@-1")
      if (t.label.hasNext) t ++= t.label.next.token.activeCategories.filter(!_.contains('@')).map(_+"@+1")
    })
    // println("Using "+TokenDomain.dimensionSize+" observable features.")
    
    // Train and test
    (trainLabels ++ testLabels).foreach(_.setRandomly)
    val learner = new SampleRankTrainer(new GibbsSampler(model, objective), new ConstantLearningRate)
    val predictor = new VariableSettingsSampler[Label](model)
    for (i <- 1 to 4) {
      //println("Iteration "+i)
      learner.processContexts(trainLabels)
      // trainLabels.take(50).foreach(printLabel _); println; println
      // printDiagnostic(trainLabels.take(400))
      predictor.processAll(testLabels)
      //println ("Train accuracy = "+ objective.accuracy(trainLabels))
      //println ("Test  accuracy = "+ objective.accuracy(testLabels))
    }
    predictor.temperature *= 0.1 // Be more greedy in inference
    repeat(2) { 
      predictor.processAll(trainLabels) 
      predictor.processAll(testLabels)
    }
    println ("Final Train accuracy = "+ objective.accuracy(trainLabels))
    println ("Final Test  accuracy = "+ objective.accuracy(testLabels))
  }



  def printLabel(label:Label) : Unit = {
    println("%-16s TRUE=%-8s PRED=%-8s %s".format(label.token.word, label.target.categoryValue, label.value, label.token.toString))
  }
 
  def printDiagnostic(labels:Seq[Label]) : Unit = {
    for (label <- labels; if label.intValue != label.domain.index("O")) {
      if (!label.hasPrev || label.value != label.prev.value) 
        print("%-7s %-7s ".format(if (label.categoryValue != label.target.categoryValue) label.target.value.category else " ", label.value.category))
      print(label.token.word+" ")
      if (!label.hasNext || label.value != label.next.value) println()
    }
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
        //label.token += "SUFFIX3="+word.takeRight(3)
        //label.token += "PREFIX3="+word.take(3)
        label.token += "POS="+partOfSpeech
        if (Character.isUpperCase(word.head)) label.token += "CAPITALIZED"
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


