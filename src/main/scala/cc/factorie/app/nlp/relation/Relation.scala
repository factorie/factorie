package cc.factorie.app.nlp.relation

import scala.io.Source
import cc.factorie.app.nlp.{DocumentAnnotatorPipeline, Document}
import cc.factorie.app.nlp.pos.OntonotesForwardPosTagger
import cc.factorie.app.nlp.ner.NoEmbeddingsConllStackedChainNer
import cc.factorie.app.nlp.parse.OntonotesTransitionBasedParser
import cc.factorie.app.nlp.coref.ParseForwardCoref
import java.io.FileInputStream

/**
 * @author John Sullivan
 */
object Relation {

  def main(args:Array[String]) {

    val doc = new Document(Source.fromFile(args(0)).getLines().mkString("\n")).setName(args(0).split("""/""").last)

    val relFinder = if(args.length >= 3) {
      new PatternBasedRelationFinder(PatternRelationPredictor.predictorsFromStreams(new FileInputStream(args(1)), new FileInputStream(args(2))))
    } else {
      ConllPatternBasedRelationFinder
    }

    val pipelineElements = Seq(
      OntonotesForwardPosTagger,
      NoEmbeddingsConllStackedChainNer,
      OntonotesTransitionBasedParser,
      ParseForwardCoref,
      relFinder 
    )
    val annoMap = DocumentAnnotatorPipeline.defaultDocumentAnnotationMap.toMap ++ Seq(classOf[RelationMentionsSet] -> (() => relFinder))
    val pipeline = DocumentAnnotatorPipeline(annoMap, Nil, pipelineElements.flatMap(_.postAttrs))
    println("loaded document")
    pipeline process doc
    println("processed pipeline")
    val relMentions = doc.attr[RelationMentionsSet].value

    println("Detected Mentions: ")
    doc.coref.mentions.foreach { mention =>
      println(mention.phrase.string + " with type " + mention.phrase.head.nerTag.baseCategoryValue + " in sentence " + mention.phrase.sentence.string)
    }

    println("writing mentions")
    relMentions.foreach { rm =>
      rm.relations.value.foreach { relation =>
        if(rm.isArg1First) {
          println(rm.arg1.string + " " + relation.value + " " + rm.arg2.string + " %.4f ".format(relation.confidence) + relation.provenance)
        } else {
          println(rm.arg2.string + " " + relation.value + " " + rm.arg1.string + " %.4f ".format(relation.confidence) + relation.provenance)
        }
      }
    }
  }
}
