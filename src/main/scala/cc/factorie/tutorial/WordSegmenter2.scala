///* Copyright (C) 2008-2010 University of Massachusetts Amherst,
//   Department of Computer Science.
//   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
//   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//    http://www.apache.org/licenses/LICENSE-2.0
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License. */



package cc.factorie.tutorial

import scala.collection.mutable.{ArrayBuffer,HashMap,HashSet,ListBuffer}
import scala.util.Sorting
import cc.factorie._
import cc.factorie.optimize._
import cc.factorie.variable._
import cc.factorie.model._
import cc.factorie.infer.{MaximizeByBPLoopy, InferByMPLP}

object WordSegmenterDemo2 {

  // The variable types:
  class Label(b:Boolean, val token:Token) extends LabeledBooleanVariable(b)
  object TokenDomain extends CategoricalVectorDomain[String]
  class Token(val char:Char, isWordStart:Boolean) extends BinaryFeatureVectorVariable[String] with ChainLink[Token,Sentence] {
    def domain = TokenDomain
    val label = new Label(isWordStart, this)
    this += char.toString
    if ("aeiou".contains(char)) this += "VOWEL"
  }
  class Sentence extends Chain[Sentence,Token]

  val model = new ModelWithContext[IndexedSeq[Label]] with Parameters {
    val bias = new DotFamilyWithStatistics1[Label] {
      factorName = "Label"
      val weights = Weights(new la.DenseTensor1(BooleanDomain.size))
    }
    val obs = new DotFamilyWithStatistics2[Label,Token] {
      factorName = "Label,Token"
      val weights = Weights(new la.DenseTensor2(BooleanDomain.size, TokenDomain.dimensionSize))
    }
    val markov = new DotFamilyWithStatistics2[Label,Label] {
      factorName = "Label,Label"
      val weights = Weights(new la.DenseTensor2(BooleanDomain.size, BooleanDomain.size))
    }
    val obsmarkov = new DotFamilyWithStatistics3[Label,Label,Token] {
      factorName = "Label,Label,Token"
      val weights = Weights(new la.Dense2LayeredTensor3(BooleanDomain.size, BooleanDomain.dimensionSize, TokenDomain.dimensionSize, new la.SparseTensor1(_)))
    }
    val skip = new DotFamily2[Label,Label] {
      factorName = "Label,Label:Boolean"
      val weights = Weights(new la.DenseTensor1(BooleanDomain.size))
      override def statistics(v1:Label#Value, v2:Label#Value) = BooleanValue(v1 == v2)
    }
    // NOTE if I don't annotate this type here I get a crazy compiler crash when it finds the upper bounds of these template types -luke
    def families: Seq[DotFamily] = Seq(bias, obs, markov, obsmarkov)
    def factorsWithContext(labels:IndexedSeq[Label]): Iterable[Factor] = {
      val result = new ListBuffer[Factor]
      for (i <- 0 until labels.length) {
        result += bias.Factor(labels(i))
        result += obs.Factor(labels(i), labels(i).token)
        if (i > 0) result += markov.Factor(labels(i-1), labels(i))
      }
      result
    }
    def factors(variables:Iterable[Var]): Iterable[Factor] = variables match {
      case variables:IndexedSeq[Label @unchecked] => factorsWithContext(variables)
    }
    override def factors(v:Var) = {
      val label = v.asInstanceOf[Label]
      val result = new ArrayBuffer[Factor](4)
      result += bias.Factor(label)
      result += obs.Factor(label, label.token)
      if (label.token.hasPrev)
        result += markov.Factor(label.token.prev.label, label)
      if (label.token.hasNext)
        result += markov.Factor(label, label.token.next.label)
      result
    }
  }

  val objective = HammingObjective

  def main(args: Array[String]) : Unit = {
    // Read data and create Variables
    implicit val random = new scala.util.Random(0)

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
    val (testSet, trainSet) = sentences.shuffle.split(0.5) //RichSeq.split(RichSeq.shuffle(instances), 0.5)
    val trainVariables = trainSet.map(_.links).flatMap(_.map(_.label))
    val testVariables = testSet.map(_.links).flatMap(_.map(_.label))

    testVariables.foreach(_.setRandomly)
    // println ("Read "+(trainVariables.size+testVariables.size)+" characters")
    // println ("Read "+trainVariables.size+" train "+testVariables.size+" test characters")
    // println ("Initial test accuracy = "+ objective.accuracy(testVariables))

    val exampleFactors = model.factors(trainSet.head.asSeq.map(_.label))
    // println("Example Factors: "+exampleFactors.mkString(", "))

    val startTime = System.currentTimeMillis // do the timing only after HotSpot has warmed up
//    val opt = new Pegasos(l2 = 0.1)
//    val opt = new L2RegularizedConstantRate(rate = 0.01, l2 = 0.005)
//    val opt = new AdaGradRDA(delta = 0.1, eta = 0.1, l1 = 0.1, l2 = 0.0)
    val opt = new AdaGrad
    val examples = trainSet.map(sentence => new StructuredSVMExample(sentence.asSeq.map(_.label), model, infer=MaximizeByBPLoopy))
    Trainer.onlineTrain(model.parameters, examples, maxIterations=15, optimizer=opt)
    for (sentence <- sentences) InferByMPLP.infer(sentence.asSeq.map(_.label), model).mapAssignment.setVariables(null)
    println ("Train accuracy = "+ objective.accuracy(trainVariables))
    println ("Test  accuracy = "+ objective.accuracy(testVariables))
    println("Finished in "+(System.currentTimeMillis-startTime)+" milliseconds.")
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
