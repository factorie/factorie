package cc.factorie.app.nlp.parse.nonproj

import cc.factorie.app.classify.LogLinearTemplate2
import cc.factorie.app.classify.ModelBasedClassifier
import cc.factorie.app.classify.SVMTrainer
import cc.factorie.app.classify.LabelList
import cc.factorie.TemplateModel
import cc.factorie.Model
import ParserSupport._
import java.io.File

object ParserSVM {
  def main(args: Array[String]): Unit = {
    val trainer = new ParserSVM()
    trainer.run(args)
  }
}

class ParserSVM extends Parser with ParserImpl {
  
  def getEmptyModel(): Model = new TemplateModel(new LogLinearTemplate2[ParseDecisionVariable, NonProjDependencyParserFeatures](lTof, DecisionDomain, NonProjParserFeaturesDomain))
  
  def load(folder: File, gzip: Boolean): ParserClassifier = {
    val c = new BaseParserClassifier(new ModelBasedClassifier(getEmptyModel(), DecisionDomain))
    c.load(folder, gzip)
    c
  }
  
  def save(c: ParserClassifier, folder: File, gzip: Boolean): Unit = c.asInstanceOf[BaseParserClassifier].save(folder, gzip)
  
  def train[B <: ParseDecisionVariable](vs: Seq[B]): ParserClassifier = {
    val backingClassifier = (new SVMTrainer).train(new LabelList[ParseDecisionVariable, NonProjDependencyParserFeatures](vs, lTof)).asInstanceOf[ModelBasedClassifier[ParseDecisionVariable]]
    new BaseParserClassifier(backingClassifier)
  }
  
}
