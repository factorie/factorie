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

import cc.factorie.app.nlp.hcoref.{GroundTruth, NodeVariables, SingularCanopy}
import cc.factorie.variable.{BagOfWordsVariable, DenseDoubleBagVariable, DiffList}

/**
 * @author John Sullivan
 */
class AuthorVars(val firstNames:BagOfWordsVariable,
                 val middleNames:BagOfWordsVariable,
                 val topics:DenseDoubleBagVariable,
                 val venues:BagOfWordsVariable,
                 val coAuthors:BagOfWordsVariable,
                 val keywords:BagOfWordsVariable,
                 var canopy:String,
                 val truth:BagOfWordsVariable, val source:String = "") extends NodeVariables[AuthorVars] with SingularCanopy with GroundTruth {

  def this(dim:Int) = this(new BagOfWordsVariable(), new BagOfWordsVariable(), new DenseDoubleBagVariable(dim), new BagOfWordsVariable(), new BagOfWordsVariable(), new BagOfWordsVariable(), "", new BagOfWordsVariable())
  def this() = this(200)

  def getVariables = Seq(firstNames, middleNames, topics, venues, coAuthors, keywords)
  var title = ""
  var fullName = ""

  def --=(other: AuthorVars)(implicit d: DiffList) {
    this.firstNames remove other.firstNames.value
    this.middleNames remove other.middleNames.value
    this.topics remove other.topics.value
    this.venues remove other.venues.value
    this.coAuthors remove other.coAuthors.value
    this.keywords remove other.keywords.value
    this.truth remove other.truth.value
  }

  def ++=(other: AuthorVars)(implicit d: DiffList) {
    this.firstNames add other.firstNames.value
    this.middleNames add other.middleNames.value
    this.topics add other.topics.value
    this.venues add other.venues.value
    this.coAuthors add other.coAuthors.value
    this.keywords add other.keywords.value
    this.truth add other.truth.value
  }

  def --(other: AuthorVars)(implicit d: DiffList) = new AuthorVars(firstNames = this.firstNames -- other.firstNames,
    middleNames = this.middleNames -- other.middleNames,
    topics = this.topics -- other.topics,
    venues = this.venues -- other.venues,
    coAuthors = this.coAuthors -- other.coAuthors,
    keywords = this.keywords -- other.keywords,
    canopy = this.canopy,
    truth = this.truth -- other.truth) // both canopies should be the same under singular canopy

  def ++(other: AuthorVars)(implicit d: DiffList) = new AuthorVars(firstNames = this.firstNames ++ other.firstNames,
    middleNames = this.middleNames ++ other.middleNames,
    topics = this.topics ++ other.topics,
    venues = this.venues ++ other.venues,
    coAuthors = this.coAuthors ++ other.coAuthors,
    keywords = this.keywords ++ other.keywords,
    canopy = this.canopy,
    truth = this.truth ++ other.truth) // both canopies should be the same under singular canopy
}

object AuthorVars {

  def fromNodeCubbie(nc:AuthorNodeCubbie):AuthorVars = {
    require(nc.topicEmbedding.isDefined, "No embedding map defined on record: %s".format(nc.id.toString))
    fromNodeCubbie(nc, nc.topicEmbedding.value.toArray)
  }
  def fromNodeCubbie(nc:AuthorNodeCubbie, embeddingMap:Keystore):AuthorVars = {
    fromNodeCubbie(nc, embeddingMap.generateVector(nc.title.value.split("""\s+""")))
  }

  protected def fromNodeCubbie(nc:AuthorNodeCubbie, topicArray:Array[Double]):AuthorVars = {
    val aVars = new AuthorVars(topicArray.length)
    aVars.fullName = nc.fullName.value
    aVars.title = nc.title.value
    aVars.firstNames ++= nc.firstNameBag.value.fetch
    aVars.middleNames ++= nc.middleNameBag.value.fetch
    aVars.topics.add(topicArray)(null)
    aVars.venues ++= nc.venues.value.fetch
    aVars.coAuthors ++= nc.coauthors.value.fetch
    aVars.keywords ++= nc.keywords.value.fetch
    nc.truth.opt.map(c => aVars.truth ++= c.fetch)
    assert(nc.canopies.value.size == 1, "expected singular canopy found %s in %s".format(nc.canopies.value, nc.id.toString))
    aVars.canopy = nc.canopies.value.head
    aVars
  }
}
