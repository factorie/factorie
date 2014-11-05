package cc.factorie.app.bib.hcoref

import cc.factorie.variable.{Var, DenseDoubleBagVariable, DiffList, BagOfWordsVariable}
import cc.factorie.app.nlp.hcoref.{GroundTruth, NodeVariables, SingularCanopy}

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

  def this(dim:Int=200) = this(new BagOfWordsVariable(), new BagOfWordsVariable(), new DenseDoubleBagVariable(dim), new BagOfWordsVariable(), new BagOfWordsVariable(), new BagOfWordsVariable(), "", new BagOfWordsVariable())

  def getVariables = Seq(firstNames, middleNames, topics, venues, coAuthors, keywords)

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
  def fromNodeCubbie(nc:AuthorNodeCubbie, embeddingMap:Map[String, Array[Double]]):AuthorVars = {
    val aVars = new AuthorVars(embeddingMap.head._2.length)
    aVars.firstNames ++= nc.firstNameBag.value.fetch
    aVars.middleNames ++= nc.middleNameBag.value.fetch
    nc.title.value.split("""\s+""").foldLeft(aVars.topics){ case (topicEmb, word) =>
      embeddingMap.get(word).map(topicEmb.add(_)(null))
      topicEmb
    }
    aVars.venues ++= nc.venues.value.fetch
    aVars.coAuthors ++= nc.coauthors.value.fetch
    aVars.keywords ++= nc.keywords.value.fetch
    nc.truth.opt.map(c => aVars.truth ++= c.fetch)
    assert(nc.canopies.value.size == 1, "expected singular canopy found %s in %s".format(nc.canopies.value, nc.id.toString))
    aVars.canopy = nc.canopies.value.head
    aVars
  }
}
