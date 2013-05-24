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
    var i = 0
    val vs = ss.flatMap(s => {
      i += 1
      if (i % 1000 == 0)
        println("Parsed: " + i)
      val oracle: NonProjectiveOracle = {
        if (mode == TRAINING) new NonprojectiveGoldOracle(s, labelDomain, featuresDomain)
        else new NonprojectiveBoostingOracle(s, classify, labelDomain, featuresDomain)
      }
      val parser = new NonProjectiveShiftReduce(oracle.predict)
      parser.parse(s, labelDomain, featuresDomain)
      oracle.instances
    })
    vs
  }

  def boosting(ss: Seq[Sentence], addlVs: Seq[ParseDecisionVariable] = Seq.empty[ParseDecisionVariable]) {
    val p = new NonProjectiveShiftReduce((v: ParseDecisionVariable) => { classify(v) })
    val newVs = generateDecisions(ss, 2)
    trainFromVariables(addlVs ++ newVs)
  }

  def predict(ss: Seq[Sentence], parallel: Boolean = true): (Seq[Seq[(Int, String)]], Seq[Seq[(Int, String)]]) = {
    val p = new NonProjectiveShiftReduce(predict = (v: ParseDecisionVariable) => { classify(v) })
    val parsers = new ThreadLocal[NonProjectiveShiftReduce] { override def initialValue = { new NonProjectiveShiftReduce(predict = p.predict) }}
    val (gold, pred) = ss.zipWithIndex.map({ case (s, i) =>
      if (i % 1000 == 0)
        println("Parsed: " + i)
      val parser = parsers.get
      val gold = parser.getSimpleDepArcs(s)
      val dts = parser.parse(s, labelDomain, featuresDomain)
      val pred = (dts.drop(1).map { dt =>
        if (dt.hasHead) dt.head.depToken.thisIdx -> dt.head.label
        else -1 -> null.asInstanceOf[String]
      } toSeq)

      (gold, pred)

    }).foldLeft(new ListBuffer[Seq[(Int, String)]], new ListBuffer[Seq[(Int, String)]])({ case (prev, curr) =>
      prev._1 append curr._1
      prev._2 append curr._2
      prev
    })

    (gold.toSeq, pred.toSeq)
  }

  def trainFromSentences(ss: Seq[Sentence]) {
    val vs = generateDecisions(ss, 0)
    trainFromVariables(vs)
  }
  def process1(doc: Document) = { doc.sentences.foreach(process(_)); doc }
  def prereqAttrs = Seq(classOf[Token], classOf[Sentence], classOf[PTBPosLabel])
  def postAttrs = Seq(classOf[ParseTree])

  def process(s: Sentence): Sentence = {
    val p = new NonProjectiveShiftReduce(predict = classify)
    val parse = new ParseTree(s)
    p.parse(s, labelDomain, featuresDomain).drop(1).filter(_.hasHead).map { dt =>
      parse.setParent(dt.thisIdx - 1, dt.head.depToken.thisIdx - 1)
      // TODO: why is this necessary? Shouldn't I be able to do set(s: String)?
      parse.label(dt.thisIdx - 1).set(ParseTreeLabelDomain.index(dt.head.label))(null)
    }
    s.attr += parse
    s
  }

}
