package cc.factorie.app.nlp.parse.nonproj

import cc.factorie.app.classify.LogLinearTemplate2
import cc.factorie.app.classify.ModelBasedClassifier
import cc.factorie.app.classify.SVMTrainer
import cc.factorie.app.classify.LabelList
import cc.factorie.TemplateModel
import cc.factorie.Model
import ParserSupport._
import ParserConstants._
import java.io.File
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.parse.ParseTree
import cc.factorie.app.nlp.parse.ParseTreeLabel
import cc.factorie.app.nlp.parse.ParseTreeLabelDomain
import cc.factorie._
import cc.factorie.app.nlp.pos.PTBPosLabel
import scala.collection.mutable.ListBuffer
import cc.factorie.util.BinarySerializer
import cc.factorie.optimize.{Example, Trainer}

trait Parser extends DocumentAnnotator {
  def trainFromVariables(vs: Seq[ParseDecisionVariable])
  def classify(v: ParseDecisionVariable): ParseDecision

  object labelDomain extends CategoricalDomain[String]
  labelDomain += ParserSupport.defaultCategory
  object featuresDomain extends CategoricalDimensionTensorDomain[String]

  def generateDecisions(ss: Seq[Sentence], mode: Int): Seq[ParseDecisionVariable] = {
    ss.flatMap(s => {
      val oracle: NonProjectiveOracle = {
        if (mode == TRAINING) new NonprojectiveGoldOracle(s, labelDomain, featuresDomain)
        else new NonprojectiveBoostingOracle(s, classify, labelDomain, featuresDomain)
      }
      new NonProjectiveShiftReduce(oracle.predict).parse(s, labelDomain, featuresDomain)
      oracle.instances
    })
  }

  def boosting(ss: Seq[Sentence], addlVs: Seq[ParseDecisionVariable] = Seq.empty[ParseDecisionVariable]) = trainFromVariables(addlVs ++ generateDecisions(ss, BOOSTING))

  def predict(ss: Seq[Sentence], parallel: Boolean = true): Seq[Seq[(Int, String)]] = {
    val parsers = new ThreadLocal[NonProjectiveShiftReduce] { override def initialValue = { new NonProjectiveShiftReduce(predict = classify) }}
    ss.zipWithIndex.map({ case (s, i) =>
      if (i % 1000 == 0)
        println("Parsed: " + i)
      parsers.get.parse(s, labelDomain, featuresDomain).drop(1).map(dt => {
        if (dt.hasHead) dt.head.depToken.thisIdx -> dt.head.label
        else -1 -> null.asInstanceOf[String]
      }).toSeq
    })
  }

  def process1(doc: Document) = { doc.sentences.foreach(process(_)); doc }
  def prereqAttrs = Seq(classOf[Token], classOf[Sentence], classOf[PTBPosLabel])
  def postAttrs = Seq(classOf[ParseTree])

  def process(s: Sentence): Sentence = {
    val p = new NonProjectiveShiftReduce(predict = classify)
    val parse = new ParseTree(s)
    p.parse(s, labelDomain, featuresDomain).drop(1).filter(_.hasHead).map { dt =>
      parse.setParent(dt.thisIdx - 1, dt.head.depToken.thisIdx - 1)
      parse.label(dt.thisIdx - 1).set(ParseTreeLabelDomain.index(dt.head.label))(null)
    }
    s.attr += parse
    s
  }

}
