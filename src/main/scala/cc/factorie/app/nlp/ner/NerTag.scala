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

package cc.factorie.app.nlp.ner

import cc.factorie.app.nlp._
import cc.factorie.la.{SparseBinaryTensor2, Tensor2}
import cc.factorie.variable._
import scala.collection.JavaConverters._

// A "Tag" is a categorical label associated with a token.

 /** An abstract class for a variable holding the part-of-speech tag of a Token.
     More specific subclasses have a domain, such as BilouConllNerDomain.
     @author Andrew McCallum */
abstract class NerTag(val token:Token, initialCategory:String) extends CategoricalVariable(initialCategory) {
   /** Return "PER" instead of "I-PER". */
   def baseCategoryValue: String = if (categoryValue.length > 1 && categoryValue(1) == '-') categoryValue.substring(2) else categoryValue

   def isEmpty = categoryValue == "O" // this should always be correct, but it might not be
   def spanPrefix = categoryValue.split("-").apply(0)
 }

trait NerSpanBuffer[Tag <: NerSpan] extends TokenSpanBuffer[Tag]

/** A categorical variable holding the named entity type of a TokenSpan.
    More specific subclasses have a domain, such as ConllNerDomain.
    @author Andrew McCallum */
abstract class NerSpanLabel(val span:TokenSpan, initialCategory:String) extends CategoricalVariable(initialCategory)
/** A TokenSpan covering a named entity.  Its entity type is indicated by its "label" member.
    @author Andrew McCallum */
abstract class NerSpan(section:Section, start:Int, length:Int) extends TokenSpan(section, start, length) {
  def label: NerSpanLabel
  override def toString = "NerSpan("+length+","+label.categoryValue+":"+this.string+")"
}
// Note: There are currently no labeled counterparts to these SpanLabels.

/** Base trait for label span encodings like BILOU and BIO
  * @author Kate Silverstein
  */
trait SpanEncoding {
  this: CategoricalDomain[String] =>
  def prefixes: Set[String]
  def encodedTags(baseTags: Seq[String]): Seq[String] = Seq("O") ++ baseTags.filter(_ != "O").flatMap(t => prefixes.map(_ + t))
  def suffixIntVal(i: Int): Int = if (i == 0) 0 else ((i - 1)/prefixes.size)+1
  def isLicitTransition(from:String, to:String):Boolean

  def isLicit(from:this.type#Value, to:this.type#Value):Boolean

  final def permittedMask:Set[(Int, Int)] =
    (for(t1 <- this._indices.values().asScala; // todo there has to be a better way to get this
        t2 <- this._indices.values().asScala
        if isLicit(t1, t2)) yield {
      //println(s"${t1.category} -> ${t2.category}")
      t1.intValue -> t2.intValue }).toSet
}
object BILOU {
  val licitTransitions = Set(
    "O" -> "B",
    "O" -> "U",
    "O" -> "O",

    "B" -> "I",
    "B" -> "L",

    "I" -> "I",
    "I" -> "L",

    "L" -> "O",
    "L" -> "B",
    "L" -> "U",

    "U" -> "U",
    "U" -> "B",
    "U" -> "O"
  )
}

object BIO {
  val licitTransitions = Set(
    "B" -> "I",
    "B" -> "O",
    "B" -> "B",

    "O" -> "B",
    "O" -> "O",

    "I" -> "I",
    "I" -> "O",
    "I" -> "B"
  )
}

/** BILOU span encoding (Beginning, Inside, Last, Outside, Unit) */
trait BILOU extends SpanEncoding {
  this : CategoricalDomain[String] =>
  def prefixes = Set("B-", "I-", "L-", "U-")
  def isLicitTransition(from: String, to: String) = BILOU.licitTransitions contains from -> to

  def splitNerTag(tag:String):(String, Option[String]) = if(tag == "O") "O" -> None else {
    val Array(pre, cat) = tag.split("-")
    if(pre == "U") {
      pre -> None
    } else {
      pre -> Some(cat)
    }
  }

  def isLicit(from: this.type#Value, to: this.type#Value) =
    splitNerTag(from.category) -> splitNerTag(to.category) match {
      case ((fromPre, Some(fromCat)), (toPre, Some(toCat))) => toCat == fromCat && BILOU.licitTransitions.contains(fromPre -> toPre)
      case ((fromPre, _), (toPre, _)) => BILOU.licitTransitions contains fromPre -> toPre
    }

}
/** BIO span encoding (Beginning, Inside, Outside) */
trait BIO extends SpanEncoding {
  this : CategoricalDomain[String] =>
  def prefixes = Set("B-", "I-")
  def isLicitTransition(from: String, to: String) = BILOU.licitTransitions contains from -> to
  def splitNerTag(tag:String):(String, Option[String]) = if(tag == "O") "O" -> None else {
    val Array(pre, cat) = tag.split("-")
    pre -> Some(cat)
  }

  def isLicit(from: this.type#Value, to: this.type#Value) =
    splitNerTag(from.category) -> splitNerTag(to.category) match {
      case ((fromPre, Some(fromCat)), (toPre, Some(toCat))) => toCat == fromCat && BIO.licitTransitions.contains(fromPre -> toPre)
      case ((fromPre, _), (toPre, _)) => BIO.licitTransitions contains fromPre -> toPre
    }
}

object ConllNerDomain extends EnumDomain {
  val O, PER, ORG, LOC, MISC = Value
  freeze()
}
class ConllNerTag(token:Token, initialCategory:String) extends NerTag(token, initialCategory) { def domain = ConllNerDomain }
class LabeledConllNerTag(token:Token, initialCategory:String) extends ConllNerTag(token, initialCategory) with CategoricalLabeling[String]

class ConllNerSpanLabel(span:TokenSpan, initialCategory:String) extends NerSpanLabel(span, initialCategory) with Serializable { def domain = ConllNerDomain }
class ConllNerSpan(section:Section, start:Int, length:Int, category:String) extends NerSpan(section, start, length) with Serializable { val label = new ConllNerSpanLabel(this, category) }
class ConllNerSpanBuffer extends NerSpanBuffer[ConllNerSpan] with Serializable
//class ConllNerLabel(val token:Token, targetValue:String) extends NerLabel(targetValue) { def domain = ConllNerDomain }


object BioConllNerDomain extends CategoricalDomain[String] with BIO {
  this ++= encodedTags(ConllNerDomain.categories)
  freeze()
  //val B_PER = index("B-PER")
  //val I_PER = index("I-PER")
  // TODO add more of these index vals
  def spanList(section:Section): ConllNerSpanBuffer = {
    val boundaries = iobBoundaries(section.tokens.map(_.attr[BioConllNerTag].categoryValue))
    new ConllNerSpanBuffer ++= boundaries.map(b => new ConllNerSpan(section, b._1, b._2, b._3))
  } 
}
class BioConllNerTag(token:Token, initialCategory:String) extends NerTag(token, initialCategory) with Serializable { def domain = BioConllNerDomain }
class LabeledBioConllNerTag(token:Token, initialCategory:String) extends BioConllNerTag(token, initialCategory) with CategoricalLabeling[String] with Serializable
//class BioConllNerLabel(val token:Token, targetValue:String) extends NerLabel(targetValue) { def domain = BioConllNerDomain }


object BilouConllNerDomain extends CategoricalDomain[String] with BILOU {
  this ++= encodedTags(ConllNerDomain.categories)
  freeze()
  def spanList(section:Section): ConllNerSpanBuffer = {
    val boundaries = bilouBoundaries(section.tokens.map(_.attr[BilouConllNerTag].categoryValue))
    new ConllNerSpanBuffer ++= boundaries.map(b => new ConllNerSpan(section, b._1, b._2, b._3))
  } 
}
class BilouConllNerTag(token:Token, initialCategory:String) extends NerTag(token, initialCategory) with Serializable {
  def domain = BilouConllNerDomain
}
class LabeledBilouConllNerTag(token:Token, initialCategory:String)
  extends BilouConllNerTag(token, initialCategory) with CategoricalLabeling[String] with Serializable
//class BilouConllNerLabel(val token:Token, targetValue:String) extends NerLabel(targetValue) { def domain = BilouConllNerDomain }


object OntonotesNerDomain extends EnumDomain {
  val O,
      CARDINAL,
      DATE,
      EVENT,
      FAC,
      GPE,
      LANGUAGE,
      LAW,
      LOC,
      MONEY,
      NORP,
      ORDINAL,
      ORG,
      PERCENT,
      PERSON,
      PRODUCT,
      QUANTITY,
      TIME,
      WORK_OF_ART = Value
  freeze()
}

/** Entity types used in coreference.
    @author Andrew McCallum */
object OntonotesEntityTypeDomain extends EnumDomain {
  val O,
      CARDINAL,
      DATE,
      EVENT,
      FAC,
      GPE,
      LANGUAGE,
      LAW,
      LOC,
      MONEY,
      NORP,
      ORDINAL,
      ORG,
      PERCENT,
      PERSON,
      PRODUCT,
      QUANTITY,
      TIME,
      WORK_OF_ART,
      MISC = Value
  freeze()
}
// OntonotesEntityType is defined in cc.factorie.app.nlp.phrase

class OntonotesNerTag(token:Token, initialCategory:String) extends NerTag(token, initialCategory) {
  def domain = OntonotesNerDomain
}
class LabeledOntonotesNerTag(token:Token, initialCategory:String)
  extends OntonotesNerTag(token, initialCategory) with CategoricalLabeling[String]

class OntonotesNerSpanLabel(span:TokenSpan, initialCategory:String) extends NerSpanLabel(span, initialCategory) with Serializable { def domain = OntonotesNerDomain }
class OntonotesNerSpan(section:Section, start:Int, length:Int, category:String) extends NerSpan(section, start, length) with Serializable { val label = new OntonotesNerSpanLabel(this, category) }
class OntonotesNerSpanBuffer(spans:Iterable[OntonotesNerSpan]) extends NerSpanBuffer[OntonotesNerSpan] with Serializable {
  def this() = this(Iterable.empty[OntonotesNerSpan])
}


object BioOntonotesNerDomain extends CategoricalDomain[String] with BIO {
  this ++= encodedTags(OntonotesNerDomain.categories)
  freeze()
}
class BioOntonotesNerTag(token:Token, initialCategory:String)
  extends NerTag(token, initialCategory) with Serializable { def domain = BioOntonotesNerDomain }
class LabeledBioOntonotesNerTag(token:Token, initialCategory:String)
  extends BioOntonotesNerTag(token, initialCategory) with CategoricalLabeling[String] with Serializable
class IobOntonotesNerTag(token:Token, initialCategory:String)
  extends NerTag(token, initialCategory) with Serializable { def domain = BioOntonotesNerDomain }
class LabeledIobOntonotesNerTag(token:Token, initialCategory:String)
  extends IobOntonotesNerTag(token, initialCategory) with CategoricalLabeling[String] with Serializable
//class BioOntonotesNerLabel(val token:Token, targetValue:String) extends NerLabel(targetValue) { def domain = BioOntonotesNerDomain }

object BilouOntonotesNerDomain extends CategoricalDomain[String] with BILOU {
  this ++= encodedTags(OntonotesNerDomain.categories)
  freeze()
  // Convert from an intValue in this domain to an intValue in the OntonotesNerDomain
  def bilouSuffixIntValue(bilouIntValue:Int): Int = if (bilouIntValue == 0) 0 else ((bilouIntValue - 1) / 4) + 1 
  def spanList(section:Section): OntonotesNerSpanBuffer = {
    val boundaries = bilouBoundaries(section.tokens.map(_.attr[BilouOntonotesNerTag].categoryValue))
    new OntonotesNerSpanBuffer ++= boundaries.map(b => new OntonotesNerSpan(section, b._1, b._2, b._3))
  } 
}
class BilouOntonotesNerTag(token:Token, initialCategory:String)
  extends NerTag(token, initialCategory) with Serializable { def domain = BilouOntonotesNerDomain }
class LabeledBilouOntonotesNerTag(token:Token, initialCategory:String)
  extends BilouOntonotesNerTag(token, initialCategory) with CategoricalLabeling[String] with Serializable

object GermevalNerDomain extends CategoricalDomain[String] {
  this ++= Vector(
   "O",
   "OTH", "OTHpart", "OTHderiv",
   "ORG", "ORGpart", "ORGderiv",
   "LOC", "LOCpart", "LOCderiv",
   "PER", "PERpart", "PERderiv"
  )
  freeze()
}
class GermevalNerTag(token:Token, initialCategory:String) extends NerTag(token, initialCategory) { def domain = GermevalNerDomain }
class LabeledGermevalNerTag(token:Token, initialCategory:String) extends GermevalNerTag(token, initialCategory) with CategoricalLabeling[String]

class GermevalNerSpanLabel(span:TokenSpan, initialCategory:String) extends NerSpanLabel(span, initialCategory) { def domain = GermevalNerDomain }
class GermevalNerSpan(section:Section, start:Int, length:Int, category:String) extends NerSpan(section, start, length) { val label = new GermevalNerSpanLabel(this, category) }
class GermevalNerSpanBuffer extends NerSpanBuffer[GermevalNerSpan]


object BioGermevalNerDomain extends CategoricalDomain[String] with BIO {
  this ++= encodedTags(GermevalNerDomain.categories)
  freeze()
}

// tags for both levels of NER annotation
class Lvl1BioGermevalNerTag(token:Token, initialCategory:String) extends NerTag(token, initialCategory) { def domain = BioGermevalNerDomain }
class LabeledLvl1BioGermevalNerTag(token:Token, initialCategory:String) extends Lvl1BioGermevalNerTag(token, initialCategory) with CategoricalLabeling[String]
class Lvl2BioGermevalNerTag(token:Token, initialCategory:String) extends NerTag(token, initialCategory) { def domain = BioGermevalNerDomain }
class LabeledLvl2BioGermevalNerTag(token:Token, initialCategory:String) extends Lvl2BioGermevalNerTag(token, initialCategory) with CategoricalLabeling[String]

object BilouGermevalNerDomain extends CategoricalDomain[String] with BILOU {
  this ++= encodedTags(GermevalNerDomain.categories)
  freeze()
  def lvl1SpanList(section:Section): GermevalNerSpanBuffer = {
    val boundaries = bilouBoundaries(section.tokens.map(_.attr[Lvl1BilouGermevalNerTag].categoryValue))
    new GermevalNerSpanBuffer ++= boundaries.map(b => new GermevalNerSpan(section, b._1, b._2, b._3))
  } 
  def lvl2SpanList(section:Section): GermevalNerSpanBuffer = {
    val boundaries = bilouBoundaries(section.tokens.map(_.attr[Lvl2BilouGermevalNerTag].categoryValue))
    new GermevalNerSpanBuffer ++= boundaries.map(b => new GermevalNerSpan(section, b._1, b._2, b._3))
  } 
}

// tags for both levels of NER annotation
class Lvl1BilouGermevalNerTag(token:Token, initialCategory:String) extends NerTag(token, initialCategory) { def domain = BilouGermevalNerDomain }
class LabeledLvl1BilouGermevalNerTag(token:Token, initialCategory:String) extends Lvl1BilouGermevalNerTag(token, initialCategory) with CategoricalLabeling[String]
class Lvl2BilouGermevalNerTag(token:Token, initialCategory:String) extends NerTag(token, initialCategory) { def domain = BilouGermevalNerDomain }
class LabeledLvl2BilouGermevalNerTag(token:Token, initialCategory:String) extends Lvl2BilouGermevalNerTag(token, initialCategory) with CategoricalLabeling[String]

