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
import cc.factorie.variable._

// A "Tag" is a categorical label associated with a token.

 /** An abstract class for a variable holding the part-of-speech tag of a Token.
     More specific subclasses have a domain, such as BilouConllNerDomain.
     @author Andrew McCallum */
abstract class NerTag(val token:Token, initialCategory:String) extends CategoricalVariable(initialCategory) {
  /** Return "PER" instead of "I-PER". */
  def shortCategoryValue: String = if (categoryValue.length > 1 && categoryValue(1) == '-') categoryValue.substring(2) else categoryValue
  def baseCategoryValue: String = if (intValue == 0) "O" else categoryValue.drop(2)
  // TODO Pick just one of the above.
}

/** A categorical variable holding the named entity type of a TokenSpan.
    More specific subclasses have a domain, such as ConllNerDomain.
    @author Andrew McCallum */
abstract class NerSpanLabel(val span:TokenSpan, initialCategory:String) extends CategoricalVariable(initialCategory)
/** A TokenSpan covering a named entity.  Its entity type is indicated by its "label" member.
    @author Andrew McCallum */
abstract class NerSpan(section:Section, start:Int, length:Int) extends TokenSpan(section, start, length) {
  def label: NerSpanLabel
  override def toString = "NerSpan("+length+","+label.categoryValue+":"+this.phrase+")"
}
// Note: There are currently no labeled counterparts to these SpanLabels.


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
class ConllNerTag(token:Token, initialCategory:String) extends NerTag(token, initialCategory) { def domain = ConllNerDomain }
class LabeledConllNerTag(token:Token, initialCategory:String) extends ConllNerTag(token, initialCategory) with CategoricalLabeling[String]

class ConllNerSpanLabel(span:TokenSpan, initialCategory:String) extends NerSpanLabel(span, initialCategory) { def domain = ConllNerDomain }
class ConllNerSpan(section:Section, start:Int, length:Int, category:String) extends NerSpan(section, start, length) { val label = new ConllNerSpanLabel(this, category) }
class ConllNerSpanBuffer extends TokenSpanBuffer[ConllNerSpan]
//class ConllNerLabel(val token:Token, targetValue:String) extends NerLabel(targetValue) { def domain = ConllNerDomain }


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
  def spanList(section:Section): ConllNerSpanBuffer = {
    val boundaries = iobBoundaries(section.tokens.map(_.attr[BioConllNerTag].categoryValue))
    new ConllNerSpanBuffer ++= boundaries.map(b => new ConllNerSpan(section, b._1, b._2, b._3))
  } 
}
class BioConllNerTag(token:Token, initialCategory:String) extends NerTag(token, initialCategory) { def domain = BioConllNerDomain }
class LabeledBioConllNerTag(token:Token, initialCategory:String) extends BioConllNerTag(token, initialCategory) with CategoricalLabeling[String]
// IobConllNerDomain is defined in app.nlp.package as val IobConllNerDomain = BioConllNerDomain
class IobConllNerTag(token:Token, initialCategory:String) extends NerTag(token, initialCategory) { def domain = IobConllNerDomain }
class LabeledIobConllNerTag(token:Token, initialCategory:String) extends IobConllNerTag(token, initialCategory) with CategoricalLabeling[String]
//class BioConllNerLabel(val token:Token, targetValue:String) extends NerLabel(targetValue) { def domain = BioConllNerDomain }


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
  def spanList(section:Section): ConllNerSpanBuffer = {
    val boundaries = bilouBoundaries(section.tokens.map(_.attr[BilouConllNerTag].categoryValue))
    new ConllNerSpanBuffer ++= boundaries.map(b => new ConllNerSpan(section, b._1, b._2, b._3))
  } 
}
class BilouConllNerTag(token:Token, initialCategory:String) extends NerTag(token, initialCategory) { def domain = BilouConllNerDomain }
class LabeledBilouConllNerTag(token:Token, initialCategory:String) extends BilouConllNerTag(token, initialCategory) with CategoricalLabeling[String]
//class BilouConllNerLabel(val token:Token, targetValue:String) extends NerLabel(targetValue) { def domain = BilouConllNerDomain }


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
class OntonotesNerTag(token:Token, initialCategory:String) extends NerTag(token, initialCategory) { def domain = OntonotesNerDomain }
class LabeledOntonotesNerTag(token:Token, initialCategory:String) extends OntonotesNerTag(token, initialCategory) with CategoricalLabeling[String]

class OntonotesNerSpanLabel(span:TokenSpan, initialCategory:String) extends NerSpanLabel(span, initialCategory) { def domain = OntonotesNerDomain }
class OntonotesNerSpan(section:Section, start:Int, length:Int, category:String) extends NerSpan(section, start, length) { val label = new OntonotesNerSpanLabel(this, category) }
class OntonotesNerSpanBuffer(spans:Iterable[OntonotesNerSpan]) extends TokenSpanBuffer[OntonotesNerSpan]
//class OntonotesNerLabel(val token:Token, targetValue:String) extends NerLabel(targetValue) { def domain = OntonotesNerDomain }


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
class BioOntonotesNerTag(token:Token, initialCategory:String) extends NerTag(token, initialCategory) { def domain = BioOntonotesNerDomain }
class LabeledBioOntonotesNerTag(token:Token, initialCategory:String) extends BioOntonotesNerTag(token, initialCategory) with CategoricalLabeling[String]
class IobOntonotesNerTag(token:Token, initialCategory:String) extends NerTag(token, initialCategory) { def domain = BioOntonotesNerDomain }
class LabeledIobOntonotesNerTag(token:Token, initialCategory:String) extends IobOntonotesNerTag(token, initialCategory) with CategoricalLabeling[String]
//class BioOntonotesNerLabel(val token:Token, targetValue:String) extends NerLabel(targetValue) { def domain = BioOntonotesNerDomain }

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
  def spanList(section:Section): OntonotesNerSpanBuffer = {
    val boundaries = bilouBoundaries(section.tokens.map(_.attr[BilouOntonotesNerTag].categoryValue))
    new OntonotesNerSpanBuffer(boundaries.map(b => new OntonotesNerSpan(section, b._1, b._2, b._3)))
  } 
}
class BilouOntonotesNerTag(token:Token, initialCategory:String) extends NerTag(token, initialCategory) { def domain = BilouOntonotesNerDomain }
class LabeledBilouOntonotesNerTag(token:Token, initialCategory:String) extends BilouOntonotesNerTag(token, initialCategory) with CategoricalLabeling[String]

