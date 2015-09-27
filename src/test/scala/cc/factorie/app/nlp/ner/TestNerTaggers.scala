package cc.factorie.app.nlp.ner

/**
 * @author Kate Silverstein 
 *         created on 3/23/15
 */

import cc.factorie.app.nlp.load._
import org.scalatest._

class TestNerTaggers extends FlatSpec {
  val conllTestFilename = this.getClass.getResource("/conll-ner-input").getPath
  val ontoTestFilename = this.getClass.getResource("/parser-test-input").getPath
  "LoadConll2003" should "load 2 documents" in {
    val testDocs = LoadConll2003.fromFilename(conllTestFilename)
    println(testDocs.length)
    testDocs.foreach(d => println(d.sections.flatMap(_.tokens).mkString(",")))
    assert(testDocs.length == 2, "failed to load documents")
    assert(testDocs.head.tokenCount > 0, "failed to load document with tokens")
    assert(testDocs.head.sections.flatMap(_.tokens).forall(t => t.attr.contains(classOf[LabeledIobConllNerTag])), "token with no LabeledIobConllNerTag")
    val bilouTestDocs = LoadConll2003(BILOU=true).fromFilename(conllTestFilename)
    assert(bilouTestDocs.length == 2, "failed to load documents")
    assert(bilouTestDocs.head.tokenCount > 0, "failed to load document with tokens")
    assert(bilouTestDocs.head.sections.flatMap(_.tokens).forall(t => t.attr.contains(classOf[LabeledBilouConllNerTag])), "token with no LabeledBilouConllNerTag")
  }
  "LoadOntonotes5" should "load 1 document" in {
    val testDocs = LoadOntonotes5.fromFilename(ontoTestFilename)
    assert(testDocs.length == 1, "failed to load documents")
    assert(testDocs.head.tokenCount > 0, "failed to load document with tokens")
    assert(testDocs.head.sections.flatMap(_.tokens).forall(t => t.attr.contains(classOf[LabeledBilouOntonotesNerTag])), "token with no LabeledBilouOntonotesNerTag")
  }
  // TODO add an actual test for training/testing ChainNer, but without loading all of the lexicons (since this takes awhile) -ks
}
