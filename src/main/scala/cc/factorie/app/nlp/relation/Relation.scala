package cc.factorie.app.nlp.relation

import scala.io.Source
import cc.factorie.app.nlp.{RelationMentionsSet, DocumentAnnotatorPipeline, Document}
import cc.factorie.app.nlp.pos.OntonotesForwardPosTagger
import cc.factorie.app.nlp.ner.NoEmbeddingsConllStackedChainNer
import cc.factorie.app.nlp.parse.OntonotesTransitionBasedParser
import cc.factorie.app.nlp.coref.ParseForwardCoref

/**
 * @author John Sullivan
 */
object Relation {
  val pipelineElements = Seq(
    OntonotesForwardPosTagger,
    NoEmbeddingsConllStackedChainNer,
    OntonotesTransitionBasedParser,
    ParseForwardCoref,
    PatternBasedRelationFinder
  )

  val pipeline = DocumentAnnotatorPipeline(DocumentAnnotatorPipeline.defaultDocumentAnnotationMap.toMap, Nil, pipelineElements.flatMap(_.postAttrs))
  def main(args:Array[String]) {

    val doc = new Document(Source.fromFile(args(0)).getLines().mkString("\n")).setName(args(0).split("""/""").last)

    println("loaded document")
    pipeline process doc
    println("processed pipeline")
    val relMentions = doc.attr[RelationMentionsSet].value

    println("writing mentions")
    relMentions.foreach { rm =>
      rm.relations.value.foreach { relation =>
        if(rm.isArg1First) {
          println(rm.arg1.string + " " + relation.value + " " + rm.arg2.string)
        } else {
          println(rm.arg2.string + " " + relation.value + " " + rm.arg1.string)
        }
      }
    }
  }
}
