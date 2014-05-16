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

package cc.factorie.app.nlp.phrase
import cc.factorie.variable.{CategoricalVariable,CategoricalDomain}
import cc.factorie.app.nlp.Token

/** Categorical variable indicating whether the noun phrase is a pronoun, common noun phrase or proper noun phrase.
    (In earlier versions this was called "MentionType", but it really is an attribute of the Phrase.)
    @author Andrew McCallum */
class NounPhraseType(val phrase:Phrase, targetValue:String) extends CategoricalVariable(targetValue) {
  def domain = NounPhraseTypeDomain
}

/** Categorical domain indicating whether the noun phrase is a pronoun, common noun phrase or proper noun phrase.
    @author Andrew McCallum */
object NounPhraseTypeDomain extends CategoricalDomain(List("PRO", "NOM", "NAM")) // TODO consider renaming these to "PRONOUN", "COMMON", "PROPER". -akm


/** A weak rule-based predictor of noun phrase type. */
object DeterministicNounPhraseTypeLabeler {
  private final val PERSONAL_PRONOUNS = Seq("PRP", "PRP$")
  private final val COMMON_NOUNS      = Seq("NN" , "NNS")
  private final val PROPER_NOUNS      = Seq("NNP", "NNPS")
  private final val ALL_NOUNS         = Seq("NN","NNS","NNP","NNPS","PRP","PRP$")

  private def isPersonalPronoun(t: Token) = PERSONAL_PRONOUNS.contains(t.posTag.categoryValue.toUpperCase)
  private def isCommonNoun     (t: Token) = COMMON_NOUNS.contains(t.posTag.categoryValue.toUpperCase)
  private def isProperNoun     (t: Token) = PROPER_NOUNS.contains(t.posTag.categoryValue.toUpperCase)
  private def isNoun           (t: Token) = ALL_NOUNS.contains(t.posTag.categoryValue.toUpperCase)

  def process(phrase:Phrase): Unit = {
    if (phrase.attr[NounPhraseType] ne null) return

    val nounType =
      if(isPersonalPronoun(phrase.headToken)) "PRO"
      else if(isCommonNoun(phrase.headToken)) "NOM"
      else if(isProperNoun(phrase.headToken)) "NAM"
      else "NOM"
    phrase.attr += new NounPhraseType(phrase,nounType)
  }
}