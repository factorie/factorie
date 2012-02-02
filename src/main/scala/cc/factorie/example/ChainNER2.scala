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

package cc.factorie.example
import cc.factorie._
import java.io.File

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
  class Label(labelName: String, word: String) extends LabelVariable(labelName) with ChainLink[Label,Sentence] {
    val token = new Token(word, this)
    def domain = LabelDomain
  }
  class Sentence extends Chain[Sentence,Label]
  
  // The model
  val excludeSkipEdges = true
  val model = new TemplateModel(
    // Bias term on each individual label 
    new TemplateWithDotStatistics1[Label], 
    // Transition factors between two successive labels
    new TemplateWithDotStatistics2[Label, Label] {
      def unroll1(label: Label) = if (label.hasPrev) Factor(label.prev, label) else Nil
      def unroll2(label: Label) = if (label.hasNext) Factor(label, label.next) else Nil
    },
    // Factor between label and observed token
    new TemplateWithDotStatistics2[Label, Token] {
      def unroll1(label: Label) = Factor(label, label.token)
      def unroll2(token: Token) = throw new Error("Token values shouldn't change")
    },
    new Template2[Label,Label] with DotStatistics1[BooleanValue] {
      def unroll1(label: Label) = if (excludeSkipEdges) Nil else for (other <- label.chainAfter; if (other.token.string == label.token.string)) yield Factor(label, other)
      def unroll2(label: Label) = if (excludeSkipEdges) Nil else for (other <- label.chainBefore; if (other.token.string == label.token.string)) yield Factor(other, label)
      def statistics(v:Values) = Stat(v._1.intValue == v._2.intValue)
    }
  )
  
  // The training objective
  val objective = new TemplateModel(new HammingLossTemplate[Label])
  

  def main(args: Array[String]): Unit = {
    if (args.length != 2) throw new Error("Usage: ChainNER2 trainfile testfile.")

    // Read in the data
    val trainSentences = load(args(0))
    val testSentences = load(args(1))

    // Get the variables to be inferred (for now, just operate on a subset)
    val trainLabels = trainSentences.flatten.take(10000)
    val testLabels = testSentences.flatten.take(2000)
    (trainLabels ++ testLabels).foreach(_.setRandomly())
    
    // Train for 5 iterations
    val learner = new VariableSettingsSampler[Label](model, objective) with SampleRank with GradientAscentUpdates
    learner.processAll(trainLabels, 5)

    // Predict, also by sampling, visiting each variable 3 times.
    val predictor = new VariableSettingsSampler[Label](model)
    //predictor.processAll(testLabels, 3)
    for (i <- 0 until 3; label <- testLabels) predictor.process(label)
    
    // Evaluate
    println ("Train accuracy = "+ objective.aveScore(trainLabels))
    println ("Test  accuracy = "+ objective.aveScore(testLabels))
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


