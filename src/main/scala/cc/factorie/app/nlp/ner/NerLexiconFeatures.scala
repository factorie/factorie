package cc.factorie.app.nlp.ner

import cc.factorie.app.nlp.Token
import cc.factorie.variable.CategoricalVectorVar


trait NerLexiconFeatures {

  def addLexiconFeatures(tokens: IndexedSeq[Token], featureFunc : (Token => CategoricalVectorVar[String]))
  
}

