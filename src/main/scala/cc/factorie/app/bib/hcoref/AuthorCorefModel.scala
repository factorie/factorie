/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
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
package cc.factorie.app.bib.hcoref

import cc.factorie.app.nlp.hcoref._
import cc.factorie.util.DefaultCmdOptions

/**
 * @author John Sullivan
 */
class AuthorCorefModel extends CorefModel[AuthorVars] with DebuggableModel[AuthorVars]

object AuthorCorefModel {
  def fromCmdOptions(opts:AuthorModelOptions):AuthorCorefModel = {
    implicit val authorCorefModel = new AuthorCorefModel
    if(opts.entitySizeWeight.value != 0.0)authorCorefModel += new EntitySizePrior(opts.entitySizeWeight.value,opts.entitySizeExponent.value)
    if(opts.bagTopicsWeight.value != 0.0)authorCorefModel += new DenseCosineDistance(opts.bagTopicsWeight.value,opts.bagTopicsShift.value, {b:AuthorVars => b.topics}, "topics")
    if(opts.bagTopicsEntropy.value != 0.0)authorCorefModel += new DenseBagOfWordsEntropy(opts.bagTopicsEntropy.value, {b:AuthorVars => b.topics}, "topics")
    if(opts.bagCoAuthorWeight.value != 0.0)authorCorefModel += new ChildParentCosineDistance(opts.bagCoAuthorWeight.value,opts.bagCoAuthorShift.value, {b:AuthorVars => b.coAuthors}, "coauthors")
    if(opts.bagCoAuthorEntropy.value != 0.0)authorCorefModel += new BagOfWordsEntropy(opts.bagCoAuthorEntropy.value, {b:AuthorVars => b.coAuthors}, "coauthors")
    if(opts.bagCoAuthorPrior.value != 0.0)authorCorefModel += new BagOfWordsSizePrior(opts.bagCoAuthorPrior.value, {b:AuthorVars => b.coAuthors}, "coauthors")
    if(opts.bagVenuesWeight.value != 0.0)authorCorefModel += new ChildParentCosineDistance(opts.bagVenuesWeight.value,opts.bagVenuesShift.value, {b:AuthorVars => b.venues}, "venues")
    if(opts.bagVenuesEntropy.value != 0.0)authorCorefModel += new BagOfWordsEntropy(opts.bagVenuesEntropy.value, {b:AuthorVars => b.venues}, "venues")
    if(opts.bagVenuesPrior.value != 0.0)authorCorefModel += new BagOfWordsSizePrior(opts.bagVenuesPrior.value, {b:AuthorVars => b.venues}, "venues")
    if(opts.bagKeywordsWeight.value != 0.0)authorCorefModel += new ChildParentCosineDistance(opts.bagKeywordsWeight.value,opts.bagKeywordsShift.value, {b:AuthorVars => b.keywords}, "keywords")
    if(opts.bagKeywordsEntropy.value != 0.0)authorCorefModel += new BagOfWordsEntropy(opts.bagKeywordsEntropy.value, {b:AuthorVars => b.keywords}, "keywords")
    if(opts.bagKeywordsPrior.value != 0.0)authorCorefModel += new BagOfWordsSizePrior(opts.bagKeywordsPrior.value, {b:AuthorVars => b.keywords}, "keywords")
    if(opts.bagFirstWeight.value != 0.0)authorCorefModel += new EntityNameTemplate(opts.bagFirstInitialWeight.value,opts.bagFirstNameWeight.value,opts.bagFirstWeight.value,opts.bagFirstSaturation.value,opts.bagFirstNoNamePenalty.value, {b:AuthorVars => b.firstNames}, "first initial")
    if(opts.bagMiddleWeight.value != 0.0)authorCorefModel += new EntityNameTemplate(opts.bagMiddleInitialWeight.value,opts.bagMiddleNameWeight.value,opts.bagMiddleWeight.value,opts.bagMiddleSaturation.value,opts.bagMiddleNoNamePenalty.value, {b:AuthorVars => b.middleNames}, "middle initial")
    if(opts.bagFirstNoNamePenalty.value != 0.0)authorCorefModel += new EmptyBagPenalty(opts.bagFirstNoNamePenalty.value, {b:AuthorVars => b.firstNames}, "first initial")
    if(opts.bagMiddleNoNamePenalty.value != 0.0)authorCorefModel += new EmptyBagPenalty(opts.bagMiddleNoNamePenalty.value, {b:AuthorVars => b.middleNames}, "first initial")
    authorCorefModel
  }
}

trait AuthorModelOptions extends DefaultCmdOptions {
  //co-authors
  val bagCoAuthorWeight = new CmdOption("model-author-bag-coauthors-weight", 4.0, "N", "Penalty for bag-of-co-authors cosine distance template (author coreference model).")
  val bagCoAuthorShift = new CmdOption("model-author-bag-coauthors-shift", -0.125, "N", "Shift for bag-of-co-authors cosine distance template  (author coreference model).")
  val bagCoAuthorEntropy = new CmdOption("model-author-bag-coauthors-entropy", 0.125, "N", "Penalty on bag-of-co-author entropy (author coreference model).")
  val bagCoAuthorPrior = new CmdOption("model-author-bag-coauthors-prior", 0.25, "N", "Bag of co-author prior penalty, formula is bag.size/bag.oneNorm*weight.")
  //venues
  val bagVenuesWeight = new CmdOption("model-author-bag-venues-weight", 4.0, "N", "Penalty for bag-of-venues cosine distance template (the author coreference model).")
  val bagVenuesShift = new CmdOption("model-author-bag-venues-shift", -0.125, "N", "Shift for bag-of-venues cosine distance template (author coreference model).")
  val bagVenuesEntropy = new CmdOption("model-author-bag-venues-entropy", 0.125, "N", "Penalty on bag-of-venue entropy (author coreference model).")
  val bagVenuesPrior = new CmdOption("model-author-bag-venues-prior", 0.25, "N", "Bag of co-author prior penalty, formula is bag.size/bag.oneNorm*weight.")
  //keywords
  val bagKeywordsWeight = new CmdOption("model-author-bag-keywords-weight", 2.0, "N", "Penalty for bag-of-keywords template  (the author coreference model).")
  val bagKeywordsShift = new CmdOption("model-author-bag-keywords-shift", -0.125, "N", "Bag-of-keywords shift for  (author coreference model).")
  val bagKeywordsEntropy = new CmdOption("model-author-bag-keywords-entropy", 0.25, "N", "Penalty on bag of keywrods entropy(author coreference model).")
  val bagKeywordsPrior = new CmdOption("model-author-bag-keywords-prior", 0.25, "N", "Bag of co-author prior penalty, formula is bag.size/bag.oneNorm*weight.")
  //topics
  val bagTopicsWeight = new CmdOption("model-author-bag-topics-weight", 4.0, "N", "Penalty for bag-of-topics cosine distance template (author coreference model).")
  val bagTopicsShift = new CmdOption("model-author-bag-topics-shift", -0.25, "N", "Shift for bag-of-topics cosine distance template (author coreference model).")
  val bagTopicsEntropy = new CmdOption("model-author-bag-topics-entropy", 0.75, "N", "Penalty on bag of topics entropy  (author coreference model).")
  val bagTopicsPrior = new CmdOption("model-author-bag-topics-prior", 0.25, "N", "Bag of topics prior penalty, formula is bag.size/bag.oneNorm*weight.")
  val bagTopicsEntropyOrdering = new CmdOption("model-author-bag-topics-entropy-ordering", 0.0, "N", "Bag of topics  penalty for when child has higher entropy than parent.")
  val entitySizeExponent = new CmdOption("model-author-size-prior-exponent", 1.2, "N", "Exponent k for rewarding entity size: w*|e|^k")
  val entitySizeWeight = new CmdOption("model-author-size-prior-weight", 0.05, "N", "Weight w for rewarding entity size: w*|e|^k.")
  //author names
  val bagFirstInitialWeight = new CmdOption("model-author-bag-first-initial-weight", 3.0, "N", "Penalty for first initial mismatches.")
  val bagFirstNoNamePenalty = new CmdOption("model-author-bag-first-noname-penalty", 2.0, "N", "Penalty for first initial mismatches.")
  val bagFirstNameWeight = new CmdOption("model-author-bag-first-name-weight", 3.0, "N", "Penalty for first name mismatches")
  val bagFirstSaturation = new CmdOption("model-author-bag-first-saturation", 16.0, "N", "Penalty for first initial mismatches.")
  val bagFirstWeight = new CmdOption("model-author-bag-first-weight", 1.0, "N", "Penalty for first initial mismatches.")
  val bagMiddleInitialWeight = new CmdOption("model-author-bag-middle-initial-weight", 3.0, "N", "Penalty for first initial mismatches.")
  val bagMiddleNameWeight = new CmdOption("model-author-bag-middle-name-weight", 3.0, "N", "Penalty for first name mismatches")
  val bagMiddleSaturation = new CmdOption("model-author-bag-middle-saturation", 26.0, "N", "Penalty for first initial mismatches.")
  val bagMiddleWeight = new CmdOption("model-author-bag-middle-weight", 1.0, "N", "Penalty for first initial mismatches.")
  val bagMiddleNoNamePenalty = new CmdOption("model-author-bag-middle-noname-penalty", 0.25, "N", "Penalty for first initial mismatches.")

  //structural priors
  val depthPenalty = new CmdOption("model-depth-penalty", 0.0, "N", "Penalty depth in the tree.")
  val bagFirstNamePenalty = new CmdOption("model-author-firstname-penalty", 16.0, "N", "Penalty for having multiple first names")
  val bagMiddleNamePenalty = new CmdOption("model-author-middlename-penalty", 16.0, "N", "Penalty for having multiple middle names")
}

