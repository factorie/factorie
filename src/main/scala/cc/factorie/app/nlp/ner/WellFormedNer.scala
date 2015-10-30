package cc.factorie.app.nlp.ner

import cc.factorie.app.chain.{ChainHelper, ChainCliqueValues}
import cc.factorie.app.nlp.{Document, Token, DocumentAnnotator}
import cc.factorie.la.{Tensor2, DenseTensor2}
import cc.factorie.util.Logger

import scala.reflect.{ClassTag, classTag}

/**
 * Takes an Ner annotator and returns a document annotator that produces only annotations that are well-formed according
 * to the permittedMask on the labelDomain. This is a prerequisite to NerChunkAnnotator in general
 * @author johnsullivan
 */
class WellFormedNer[Tag <: NerTag : ClassTag](ner:ChainNer[Tag]) extends DocumentAnnotator {
  import WellFormedNer._

  lazy val prereqAttrs = ner.prereqAttrs
  lazy val postAttrs = ner.postAttrs

  def tokenAnnotationString(token: Token) = ner.tokenAnnotationString(token)

  private val maskedTransitions:Tensor2 = {
    val mt = new DenseTensor2(ner.labelDomain.dimensionSize, ner.labelDomain.dimensionSize, Double.NegativeInfinity)
    ner.labelDomain.permittedMask.foreach { case (i, j) =>
      mt.update(i,j, 0.0)
    }
    (mt + ner.model.markov.weights.value).asInstanceOf[Tensor2]
  }

  private def maximizeWellFormed(vars:Seq[Tag]): Unit = {
    val result = ChainHelper.viterbiFast(ChainCliqueValues(ner.model.getLocalScores(vars), Seq.fill(math.max(1, vars.size - 1))(maskedTransitions)))
    vars.indices.foreach(i => vars(i).set(result.mapValues(i))(null))
  }

  def process(document: Document) = {
    if(document.tokens.nonEmpty) {
      if (!document.tokens.head.attr.contains(classTag[Tag].runtimeClass))
        document.tokens.map(token => token.attr += ner.newLabel(token, "O"))
      if (!document.tokens.head.attr.contains(classOf[ner.ChainNERFeatures])) {
        document.tokens.map(token => {token.attr += new ner.ChainNERFeatures(token)})
        ner.addFeatures(document, (t:Token)=>t.attr[ner.ChainNERFeatures])
      }
      document.tokens.foreach(_.attr[Tag].setCategory("O")(null))
      document.sentences.collect { case s if s.size > 1 =>
        maximizeWellFormed(s.tokens.map(_.attr[Tag]))
      }
    } else {
      logger.warn("We got an empty document with name: " + document.name)
    }
    document
  }
}

object WellFormedNer {
  private val logger = Logger.getLogger(getClass.getName)
}
