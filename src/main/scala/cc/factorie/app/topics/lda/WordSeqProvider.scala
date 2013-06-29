package cc.factorie.app.topics.lda

import cc.factorie._

/**
 * Created with IntelliJ IDEA.
 * User: vineet
 * Date: 6/27/13
 * Time: 8:23 PM
 * To change this template use File | Settings | File Templates.
 */
trait WordSeqProvider {
  def initializeDocuments(): Stream[CategoricalSeqVariable[String]]
  def getRandomDocument(): CategoricalSeqVariable[String]
  def getWordDomain: CategoricalDomain[String]
  def numDocs:Int
}
