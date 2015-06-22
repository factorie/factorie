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

package cc.factorie.app.nlp.pos
import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.variable._

abstract class PosTag(val token:Token, initialIndex:Int) extends CategoricalVariable[String](initialIndex)

/** Penn Treebank part-of-speech tag domain. */
object PennPosDomain extends CategoricalDomain[String] {
  this ++= Vector(
      "#", // In WSJ but not in Ontonotes
      "$",
      "''",
      ",",
      "-LRB-",
      "-RRB-",
      ".",
      ":",
      "CC",
      "CD",
      "DT",
      "EX",
      "FW",
      "IN",
      "JJ",
      "JJR",
      "JJS",
      "LS",
      "MD",
      "NN",
      "NNP",
      "NNPS",
      "NNS",
      "PDT",
      "POS",
      "PRP",
      "PRP$",
      "PUNC",
      "RB",
      "RBR",
      "RBS",
      "RP",
      "SYM",
      "TO",
      "UH",
      "VB",
      "VBD",
      "VBG",
      "VBN",
      "VBP",
      "VBZ",
      "WDT",
      "WP",
      "WP$",
      "WRB",
      "``",
      "ADD", // in Ontonotes, but not WSJ
      "AFX", // in Ontonotes, but not WSJ
      "HYPH", // in Ontonotes, but not WSJ
      "NFP", // in Ontonotes, but not WSJ
      "XX" // in Ontonotes, but not WSJ
  )
  freeze()
  // Short-cuts for a few commonly-queried tags
  val posIndex = index("POS")
  val nnpIndex = index("NNP")
  val nnpsIndex = index("NNPS")
  val prpIndex = index("PRP")
  val prpdIndex = index("PRP$")
  val wpIndex = index("WP")
  val wpdIndex = index("WP$")
  val ccIndex = index("CC")

  def isNoun(pos:String): Boolean = pos(0) == 'N' 
  def isProperNoun(pos:String) = { pos == "NNP" || pos == "NNPS" }
  def isVerb(pos:String) = pos(0) == 'V'
  def isAdjective(pos:String) = pos(0) == 'J'
  def isPersonalPronoun(pos: String) = pos == "PRP"
}
/** A categorical variable, associated with a token, holding its Penn Treebank part-of-speech category.  */
class PennPosTag(token:Token, initialIndex:Int)
  extends PosTag(token, initialIndex) with Serializable {
  def this(token:Token, initialCategory:String) = this(token, PennPosDomain.index(initialCategory))
  final def domain = PennPosDomain
  def isNoun = PennPosDomain.isNoun(categoryValue)
  def isProperNoun = PennPosDomain.isProperNoun(categoryValue)
  def isVerb = PennPosDomain.isVerb(categoryValue)
  def isAdjective = PennPosDomain.isAdjective(categoryValue)
  def isPersonalPronoun = PennPosDomain.isPersonalPronoun(categoryValue)
}
/** A categorical variable, associated with a token, holding its Penn Treebank part-of-speech category,
    which also separately holds its desired correct "target" value.  */
class LabeledPennPosTag(token:Token, targetValue:String)
  extends PennPosTag(token, targetValue) with CategoricalLabeling[String] with Serializable


/** The "A Universal Part-of-Speech Tagset"
    by Slav Petrov, Dipanjan Das and Ryan McDonald
    http://arxiv.org/abs/1104.2086
    http://code.google.com/p/universal-pos-tags
    
    VERB - verbs (all tenses and modes)
    NOUN - nouns (common and proper)
    PRON - pronouns 
    ADJ - adjectives
    ADV - adverbs
    ADP - adpositions (prepositions and postpositions)
    CONJ - conjunctions
    DET - determiners
    NUM - cardinal numbers
    PRT - particles or other function words
    X - other: foreign words, typos, abbreviations
    . - punctuation
  */
object UniversalPosDomain extends EnumDomain {
  this ++= Vector("VERB", "NOUN", "PRON", "ADJ", "ADV", "ADP", "CONJ", "DET", "NUM", "PRT", "X", ".")
  freeze()
  private val Penn2universal = new scala.collection.mutable.HashMap[String,String] ++= Vector(
      "!" -> ".",
      "#" -> ".",
      "$" -> ".",
      "''" ->  ".",
      "(" -> ".",
      ")" -> ".",
      "," -> ".",
      "-LRB-" -> ".",
      "-RRB-" -> ".",
      "." -> ".",
      ":" -> ".",
      "?" -> ".",
      "CC" -> "CONJ",
      "CD" -> "NUM",
      "CD|RB" -> "X",
      "DT" -> "DET",
      "EX"-> "DET",
      "FW" -> "X",
      "IN" -> "ADP",
      "IN|RP" -> "ADP",
      "JJ" -> "ADJ",
      "JJR" -> "ADJ",
      "JJRJR" -> "ADJ",
      "JJS" -> "ADJ",
      "JJ|RB" -> "ADJ",
      "JJ|VBG" -> "ADJ",
      "LS" -> "X",
      "MD" -> "VERB",
      "NN" -> "NOUN",
      "NNP" -> "NOUN",
      "NNPS" -> "NOUN",
      "NNS" -> "NOUN",
      "NN|NNS" -> "NOUN",
      "NN|SYM" -> "NOUN",
      "NN|VBG" -> "NOUN",
      "NP" -> "NOUN",
      "PDT" -> "DET",
      "POS" -> "PRT",
      "PRP" -> "PRON",
      "PRP$" -> "PRON",
      "PRP|VBP" -> "PRON",
      "PRT" -> "PRT",
      "RB" -> "ADV",
      "RBR" -> "ADV",
      "RBS" -> "ADV",
      "RB|RP" -> "ADV",
      "RB|VBG" -> "ADV",
      "RN" -> "X",
      "RP" -> "PRT",
      "SYM" -> "X",
      "TO" -> "PRT",
      "UH" -> "X",
      "VB" -> "VERB",
      "VBD" -> "VERB",
      "VBD|VBN" -> "VERB",
      "VBG" -> "VERB",
      "VBG|NN" -> "VERB",
      "VBN" -> "VERB",
      "VBP" -> "VERB",
      "VBP|TO" -> "VERB",
      "VBZ" -> "VERB",
      "VP" -> "VERB",
      "WDT" -> "DET",
      "WH" -> "X",
      "WP" -> "PRON",
      "WP$" -> "PRON",
      "WRB" -> "ADV",
      "``" -> ".")
  def categoryFromPenn(PennPosCategory:String): String = Penn2universal(PennPosCategory)
}

/** A categorical variable, associated with a token, holding its Google Universal part-of-speech category.  */
class UniversalPosTag(val token:Token, initialValue:String) extends CategoricalVariable(initialValue) {
  def this(token:Token, other:PennPosTag) = this(token, UniversalPosDomain.categoryFromPenn(other.categoryValue))
  def domain = UniversalPosDomain
}
/** A categorical variable, associated with a token, holding its Google Universal part-of-speech category,
    which also separately holds its desired correct "target" value.  */
class LabeledUniversalPosTag(token:Token, targetValue:String) extends UniversalPosTag(token, targetValue) with CategoricalLabeling[String]


/** Penn Treebank part-of-speech tag domain. */
object SpanishPosDomain extends CategoricalDomain[String] {
  this ++= Vector(
    "a", // adjective
    "c", // conjunction
    "d", // determiner
    "f", // punctuation
    "i", // interjection
    "n", // noun
    "p", // pronoun
    "r", // adverb
    "s", // preposition
    "v", // verb
    "w", // date
    "z", // number
    "_" // unknown
  )
  freeze()

  def isNoun(pos:String): Boolean = pos(0) == 'n'
//  def isProperNoun(pos:String) = { pos == "NNP" || pos == "NNPS" }
  def isVerb(pos:String) = pos(0) == 'v'
  def isAdjective(pos:String) = pos(0) == 'a'
//  def isPersonalPronoun(pos: String) = pos == "PRP"
}
/** A categorical variable, associated with a token, holding its Penn Treebank part-of-speech category.  */
class SpanishPosTag(token:Token, initialIndex:Int) extends PosTag(token, initialIndex) {
  def this(token:Token, initialCategory:String) = this(token, SpanishPosDomain.index(initialCategory))
  final def domain = SpanishPosDomain
  def isNoun = SpanishPosDomain.isNoun(categoryValue)
//  def isProperNoun = SpanishPosDomain.isProperNoun(categoryValue)
  def isVerb = SpanishPosDomain.isVerb(categoryValue)
  def isAdjective = SpanishPosDomain.isAdjective(categoryValue)
//  def isPersonalPronoun = SpanishPosDomain.isPersonalPronoun(categoryValue)
}

/** A categorical variable, associated with a token, holding its Spanish Treebank part-of-speech category,
    which also separately holds its desired correct "target" value.  */
class LabeledSpanishPosTag(token:Token, targetValue:String) extends SpanishPosTag(token, targetValue) with CategoricalLabeling[String]

