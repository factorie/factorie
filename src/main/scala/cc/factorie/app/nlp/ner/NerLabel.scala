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

package cc.factorie.app.nlp.ner
import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.util.Cubbie
import cc.factorie.variable.{LabeledCategoricalVariable, DiffList, CategoricalDomain}

/** The abstract class for all named-entity recognition labels. */
abstract class NerLabel(initialValue:String) extends LabeledCategoricalVariable(initialValue) {
  /** Return "PER" instead of "I-PER". */
  def shortCategoryValue: String = if (categoryValue.length > 1 && categoryValue(1) == '-') categoryValue.substring(2) else categoryValue
  def cubbieSlotName = "ner"
  def cubbieSlotValue = categoryValue
}

class Conll2003SpanNerLabel(val span:NerSpan, initialValue:String) extends NerLabel(initialValue) {
  def domain = ConllNerDomain
}
// TODO this shouldn't extend hcoref Mention
// TODO Consider containing a Span rather than inheriting from Span.
// TODO Consider making this an NERMention, inheriting from NounMention
class NerSpan(sec:Section, labelString:String, start:Int, length:Int)(implicit d:DiffList) extends TokenSpan(sec, start, length) with cc.factorie.app.nlp.hcoref.TokenSpanMention {
  val label = new Conll2003SpanNerLabel(this, labelString)
  def isCorrect = this.tokens.forall(token => token.nerLabel.intValue == label.intValue) &&
    (!hasPredecessor(1) || predecessor(1).nerLabel.intValue != label.intValue) &&
    (!hasSuccessor(1) || successor(1).nerLabel.intValue != label.intValue)
  override def toString = "NerSpan("+length+","+label.categoryValue+":"+this.phrase+")"
}

class NerSpanList extends TokenSpanList[NerSpan]

class NerLabelCubbie extends Cubbie {
  val label = StringSlot("label")
}

object ConllNerDomain extends CategoricalDomain[String] {
  this ++= Vector(
   "O",
   "PER", // even though this never occurs in the CoNLL-2003 training data, it could occur in some other training data
   "ORG",
   "LOC",
   "MISC"
  )
  freeze()
}
class ConllNerLabel(val token:Token, targetValue:String) extends NerLabel(targetValue) { def domain = ConllNerDomain }


object BioConllNerDomain extends CategoricalDomain[String] {
  this ++= Vector(
   "O",
   "B-PER", // even though this never occurs in the CoNLL-2003 training data, it could occur in some other training data
   "I-PER",
   "B-ORG",
   "I-ORG",
   "B-LOC",
   "I-LOC",
   "B-MISC",
   "I-MISC"
  )
  freeze()
}
class BioConllNerLabel(val token:Token, targetValue:String) extends NerLabel(targetValue) { def domain = BioConllNerDomain }


object BilouConllNerDomain extends CategoricalDomain[String] {
  this ++= Vector(
   "O",
   "B-PER",
   "I-PER",
   "L-PER",
   "U-PER",
   "B-ORG",
   "I-ORG",
   "L-ORG",
   "U-ORG",
   "B-LOC",
   "I-LOC",
   "L-LOC",
   "U-LOC",
   "B-MISC",
   "I-MISC",
   "L-MISC",
   "U-MISC"
  )  
  freeze()
}
class BilouConllNerLabel(val token:Token, targetValue:String) extends NerLabel(targetValue) { def domain = BilouConllNerDomain }


object OntonotesNerDomain extends CategoricalDomain[String] {
  this ++= Vector(
      "O",
      "CARDINAL",
      "DATE",
      "EVENT",
      "FAC",
      "GPE",
      "LANGUAGE",
      "LAW",
      "LOC",
      "MONEY",
      "NORP",
      "ORDINAL",
      "ORG",
      "PERCENT",
      "PERSON",
      "PRODUCT",
      "QUANTITY",
      "TIME",
      "WORK_OF_ART"
  )

  freeze()
}
class OntonotesNerLabel(val token:Token, targetValue:String) extends NerLabel(targetValue) { def domain = OntonotesNerDomain }


object BioOntonotesNerDomain extends CategoricalDomain[String] {
  this ++= Vector(
      "O",
      "B-CARDINAL",
      "I-CARDINAL",
      "B-DATE",
      "I-DATE",
      "B-EVENT",
      "I-EVENT",
      "B-FAC",
      "I-FAC",
      "B-GPE",
      "I-GPE",
      "B-LANGUAGE",
      "I-LANGUAGE",
      "B-LAW",
      "I-LAW",
      "B-LOC",
      "I-LOC",
      "B-MONEY",
      "I-MONEY",
      "B-NORP",
      "I-NORP",
      "B-ORDINAL",
      "I-ORDINAL",
      "B-ORG",
      "I-ORG",
      "B-PERCENT",
      "I-PERCENT",
      "B-PERSON",
      "I-PERSON",
      "B-PRODUCT",
      "I-PRODUCT",
      "B-QUANTITY",
      "I-QUANTITY",
      "B-TIME",
      "I-TIME",
      "B-WORK_OF_ART",
      "I-WORK_OF_ART"
  )
  freeze()
}
class BioOntonotesNerLabel(val token:Token, targetValue:String) extends NerLabel(targetValue) { def domain = BioOntonotesNerDomain }

object BilouOntonotesNerDomain extends CategoricalDomain[String] {
  this ++= Vector(
      "O",
      "B-CARDINAL",
      "I-CARDINAL",
      "L-CARDINAL",
      "U-CARDINAL",
      "B-DATE",
      "I-DATE",
      "L-DATE",
      "U-DATE",
      "B-EVENT",
      "I-EVENT",
      "L-EVENT",
      "U-EVENT",
      "B-FAC",
      "I-FAC",
      "L-FAC",
      "U-FAC",
      "B-GPE",
      "I-GPE",
      "L-GPE",
      "U-GPE",
      "B-LANGUAGE",
      "I-LANGUAGE",
      "L-LANGUAGE",
      "U-LANGUAGE",
      "B-LAW",
      "I-LAW",
      "L-LAW",
      "U-LAW",
      "B-LOC",
      "I-LOC",
      "L-LOC",
      "U-LOC",
      "B-MONEY",
      "I-MONEY",
      "L-MONEY",
      "U-MONEY",
      "B-NORP",
      "I-NORP",
      "L-NORP",
      "U-NORP",
      "B-ORDINAL",
      "I-ORDINAL",
      "L-ORDINAL",
      "U-ORDINAL",
      "B-ORG",
      "I-ORG",
      "L-ORG",
      "U-ORG",
      "B-PERCENT",
      "I-PERCENT",
      "L-PERCENT",
      "U-PERCENT",
      "B-PERSON",
      "I-PERSON",
      "L-PERSON",
      "U-PERSON",
      "B-PRODUCT",
      "I-PRODUCT",
      "L-PRODUCT",
      "U-PRODUCT",
      "B-QUANTITY",
      "I-QUANTITY",
      "L-QUANTITY",
      "U-QUANTITY",
      "B-TIME",
      "I-TIME",
      "L-TIME",
      "U-TIME",
      "B-WORK_OF_ART",
      "I-WORK_OF_ART",
      "L-WORK_OF_ART",
      "U-WORK_OF_ART"
  )
  // Convert from an intValue in this domain to an intValue in the OntonotesNerDomain
  def bilouSuffixIntValue(bilouIntValue:Int): Int = if (bilouIntValue == 0) 0 else ((bilouIntValue - 1) / 4) + 1 
  freeze()
}
class BilouOntonotesNerLabel(val token:Token, targetValue:String) extends NerLabel(targetValue) {
  def domain = BilouOntonotesNerDomain
  def baseCategoryValue: String = if (intValue == 0) "O" else categoryValue.drop(2)
}

