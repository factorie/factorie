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

import scala.collection.mutable.{ArrayBuffer,HashMap,HashSet,ListBuffer}
import scala.util.Sorting
import cc.factorie._
import java.io.File
import cc.factorie.util.BinarySerializer
import cc.factorie.optimize.Trainer
import cc.factorie.variable._
import cc.factorie.model._
import cc.factorie.infer.IteratedConditionalModes

object WordSegmenterDemo { 
  
  // The variable types:
  //object LabelDomain extends CategoricalDomain[Boolean]
  class Label(b:Boolean, val token:Token) extends LabeledBooleanVariable(b)  // { def domain = LabelDomain }
  object TokenDomain extends CategoricalVectorDomain[String]
  class Token(val char:Char, isWordStart:Boolean) extends BinaryFeatureVectorVariable[String] with ChainLink[Token,Sentence] {
    def domain = TokenDomain
    val label = new Label(isWordStart, this)
    this += char.toString
    if ("aeiou".contains(char)) this += "VOWEL"
  }
  class Sentence extends Chain[Sentence,Token]

    // The factor templates that define the model
  val model = new TemplateModel with Parameters
  /** Bias term just on labels */
  model += new DotTemplateWithStatistics1[Label] {
    val weights = model.Weights(new la.DenseTensor1(BooleanDomain.size))
    factorName = "Label"
    //def statistics(t:Label#Value) = Stat(t)
    //def statisticsDomains = Tuple1(BooleanDomain)
  }
//  /** Factor between label and observed token */
//  model += new TemplateWithDotStatistics2[Label,Token] with SparseWeights {
//    factorName = "Label,Token"
//    def statisticsDomains = ((BooleanDomain,TokenDomain))
//    def unroll1 (label:Label) = Factor(label, label.token)
//    def unroll2 (token:Token) = throw new Error("Token values shouldn't change")
//  }
//  /** A token bi-gram conjunction  */
//  model += new TemplateWithDotStatistics3[Label,Token,Token] with SparseWeights {
//    factorName = "Label,Token,Token"
//    def statisticsDomains = (((BooleanDomain,TokenDomain,TokenDomain)))
//    def unroll1 (label:Label) = if (label.token.hasPrev) Factor(label, label.token, label.token.prev) else Nil
//    def unroll2 (token:Token) = throw new Error("Token values shouldn't change")
//    def unroll3 (token:Token) = throw new Error("Token values shouldn't change")
//  }
  /** Factor between label and observed token */
  model += new DotTemplateWithStatistics2[Label,Token] {
    factorName = "Label,Token"
    //def statisticsDomains = ((BooleanDomain,TokenDomain))
    val weights = model.Weights(new la.DenseTensor2(BooleanDomain.size, TokenDomain.dimensionSize))
    def unroll1 (label:Label) = Factor(label, label.token)
    def unroll2 (token:Token) = Factor(token.label, token)
  }
  /** A token bi-gram conjunction  */
  model += new DotTemplateWithStatistics3[Label,Token,Token] {
    factorName = "Label,Token,Token"
    //def statisticsDomains = (((BooleanDomain,TokenDomain,TokenDomain)))
    val weights = model.Weights(new la.Dense2LayeredTensor3(BooleanDomain.size, TokenDomain.dimensionSize, TokenDomain.dimensionSize, new la.SparseTensor1(_)))
    def unroll1 (label:Label) = if (label.token.hasPrev) Factor(label, label.token.prev, label.token) else Nil
    def unroll2 (token:Token) = if (token.hasPrev) Factor(token.label, token.prev, token) else Nil
    def unroll3 (token:Token) = if (token.hasNext) Factor(token.next.label, token, token.next) else Nil
  }
  /** Factor between two successive labels */
  model += new DotTemplateWithStatistics2[Label,Label] {
    factorName = "Label,Label"
    val weights = model.Weights(new la.DenseTensor2(BooleanDomain.size, BooleanDomain.size))
    //def statisticsDomains = ((BooleanDomain,BooleanDomain))
    def unroll1 (label:Label) = if (label.token.hasNext) Factor(label, label.token.next.label) else Nil
    def unroll2 (label:Label) = if (label.token.hasPrev) Factor(label.token.prev.label, label) else Nil
  }
  /** Skip edge */
  val skipTemplate = new DotTemplate2[Label,Label] {
    factorName = "Label,Label:Boolean"
    val weights = model.Weights(new la.DenseTensor1(BooleanDomain.size))
    //def statisticsDomains = Tuple1(BooleanDomain)
    def unroll1 (label:Label) =  
      // could cache this search in label.similarSeq for speed
      for (other <- label.token.chain.links; if label.token.char == other.char) yield
        if (label.token.position < other.position) Factor(label, other.label) else Factor(other.label,label)
    def unroll2 (label:Label) = Nil // We handle symmetric case above
    override def statistics(v1:Label#Value, v2:Label#Value) = BooleanValue(v1 == v2)
  }
  //model += skipTemplate

  val objective = HammingObjective



  def main(args: Array[String]) : Unit = {
    implicit val random = new scala.util.Random 
    var startTime:Long = 0
  
    // Read data and create Variables
    val sentences = for (string <- data.toList) yield {
      val sentence = new Sentence
      var beginword = true
      for (c <- string.toLowerCase) {
        if (c >= 'a' && c <= 'z') {
          sentence += new Token(c, beginword)
          beginword = false
        } else
          beginword = true
      }
      for (token <- sentence.links) {
        if (token.hasPrev) token += (token.prev.char+"@-1") else token += "START@-1"
        if (token.hasNext) token += (token.next.char+"@1") else token += "END@+1"
      }
      sentence
    }
    // println("TokenDomain.dimensionDomain.size="+TokenDomain.dimensionDomain.size)

    // Make a test/train split
    val (testSet, trainSet) = sentences.shuffle(random).split(0.5) //RichSeq.split(RichSeq.shuffle(instances), 0.5)
    var trainVariables = trainSet.map(_.links).flatMap(_.map(_.label))
    var testVariables = testSet.map(_.links).flatMap(_.map(_.label))
    //var sampler = new VariableSettingsSampler[Label](model); val predictor = new SamplingMaximizer(sampler)
    val predictor = new IteratedConditionalModes(model)
    
    // println ("Read "+(trainVariables.size+testVariables.size)+" characters")
    // println ("Read "+trainVariables.size+" train "+testVariables.size+" test characters")
    (trainVariables ++ testVariables).foreach(_.setRandomly)
    println ("Initial train accuracy = "+ objective.accuracy(testVariables))
    println ("Initial test  accuracy = "+ objective.accuracy(testVariables))
    
    val exampleFactors = model.factors(trainVariables.tail.head)
    // println("Example Factors: "+exampleFactors.mkString(", "))
    //println("Example Factors score: "+exampleFactors.map(_.score).sum)
  
    // If a saved model was specified on the command-line, then instead of training, take parameters from there, test and exit
    if (args.length > 0) {
      println("Loading model parameters from "+args(0))
      BinarySerializer.deserialize(TokenDomain, model, new File(args(0)))
      //var predictor = SamplingMaximizer[Label](model); predictor.iterations = 6; predictor.rounds = 2
      //predictor.maximize(testVariables, iterations=6, rounds=2)
      predictor.processAll(testVariables)
      println ("Test  accuracy = "+ objective.accuracy(testVariables))
      System.exit(0)
    }

    // Sample and Learn!
    val examples = trainSet.map(_.asSeq.map(_.label)).map(new optimize.PseudolikelihoodExample(_, model))
    Trainer.onlineTrain(model.parameters, examples, maxIterations =15)
    predictor.processAll(testVariables)
    println ("Test  accuracy = "+ objective.accuracy(testVariables))
  }

  val data = Array(
    "Free software is a matter of the users' freedom to run, copy, distribute, study, change and improve the software. More precisely, it refers to four kinds of freedom, for the users of the software.",
    "The freedom to run the program, for any purpose.",
    "The freedom to study how the program works, and adapt it to your needs.",
    "The freedom to redistribute copies so you can help your neighbor.",
    "The freedom to improve the program, and release your improvements to the public, so that the whole community benefits.",
    "A program is free software if users have all of these freedoms. Thus, you should be free to redistribute copies, either with or without modifications, either gratis or charging a fee for distribution, to anyone anywhere. Being free to do these things means (among other things) that you do not have to ask or pay for permission.",
    "You should also have the freedom to make modifications and use them privately in your own work or play, without even mentioning that they exist. If you do publish your changes, you should not be required to notify anyone in particular, or in any particular way.",
    "In order for the freedoms to make changes, and to publish improved versions, to be meaningful, you must have access to the source code of the program. Therefore, accessibility of source code is a necessary condition for free software.",
    "Finally, note that criteria such as those stated in this free software definition require careful thought for their interpretation. To decide whether a specific software license qualifies as a free software license, we judge it based on these criteria to determine whether it fits their spirit as well as the precise words. If a license includes unconscionable restrictions, we reject it, even if we did not anticipate the issue in these criteria. Sometimes a license requirement raises an issue that calls for extensive thought, including discussions with a lawyer, before we can decide if the requirement is acceptable. When we reach a conclusion about a new issue, we often update these criteria to make it easier to see why certain licenses do or don't qualify.",
    "In order for these freedoms to be real, they must be irrevocable as long as you do nothing wrong; if the developer of the software has the power to revoke the license, without your doing anything to give cause, the software is not free.",
    "However, certain kinds of rules about the manner of distributing free software are acceptable, when they don't conflict with the central freedoms. For tutorial, copyleft (very simply stated) is the rule that when redistributing the program, you cannot add restrictions to deny other people the central freedoms. This rule does not conflict with the central freedoms; rather it protects them.",
    "Thus, you may have paid money to get copies of free software, or you may have obtained copies at no charge. But regardless of how you got your copies, you always have the freedom to copy and change the software, even to sell copies.",
    "Rules about how to package a modified version are acceptable, if they don't effectively block your freedom to release modified versions. Rules that ``if you make the program available in this way, you must make it available in that way also'' can be acceptable too, on the same condition. (Note that such a rule still leaves you the choice of whether to publish the program or not.) It is also acceptable for the license to require that, if you have distributed a modified version and a previous developer asks for a copy of it, you must send one.",
    "Sometimes government export control regulations and trade sanctions can constrain your freedom to distribute copies of programs internationally. Software developers do not have the power to eliminate or override these restrictions, but what they can and must do is refuse to impose them as conditions of use of the program. In this way, the restrictions will not affect activities and people outside the jurisdictions of these governments.",
    "Finally, note that criteria such as those stated in this free software definition require careful thought for their interpretation. To decide whether a specific software license qualifies as a free software license, we judge it based on these criteria to determine whether it fits their spirit as well as the precise words. If a license includes unconscionable restrictions, we reject it, even if we did not anticipate the issue in these criteria. Sometimes a license requirement raises an issue that calls for extensive thought, including discussions with a lawyer, before we can decide if the requirement is acceptable. When we reach a conclusion about a new issue, we often update these criteria to make it easier to see why certain licenses do or don't qualify.",
    "The GNU Project was launched in 1984 to develop a complete Unix-like operating system which is free software: the GNU system."
  )


}
