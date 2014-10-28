package cc.factorie.app.bib.hcoref

import cc.factorie.app.nlp.hcoref.{NodeVariables, NodeCubbie, DBNodeCollection}

/**
 * @author John Sullivan
 */
class AuthorNodeCubbie extends NodeCubbie[AuthorVars] {
  val truth = StringSlot("gt")
  val coauthors =
}
class MongoAuthorCollection extends DBNodeCollection[AuthorVars, AuthorNodeCubbie] {
  def loadAll =
}
