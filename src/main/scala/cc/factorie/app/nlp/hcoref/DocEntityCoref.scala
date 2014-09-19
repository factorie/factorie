package cc.factorie.app.nlp.hcoref

import cc.factorie.app.nlp.Document
import cc.factorie.app.nlp.coref.WithinDocCoref
import cc.factorie.variable.DiffList

/**
 * @author John Sullivan
 */
object DocEntityCoref {

  class DocEntityCorefModel(namesWeights:Double, namesShift:Double, nameEntropy:Double, contextsWeight:Double, contextsShift:Double, genderWeight:Double, genderShift:Double, mentionWeight:Double, mentionShift:Double, numberWeight:Double, numberShift:Double) extends CorefModel[DocEntityVars] {
    this += new ChildParentCosineDistance(namesWeights, namesShift, {v:DocEntityVars => v.names})
    this += new ChildParentCosineDistance(contextsWeight, contextsShift, {v:DocEntityVars => v.context})
    this += new ChildParentCosineDistance(genderWeight, genderShift, {v:DocEntityVars => v.gender})
    this += new ChildParentCosineDistance(mentionWeight, mentionShift, {v:DocEntityVars => v.mention})
    this += new ChildParentCosineDistance(numberWeight, numberShift, {v:DocEntityVars => v.number})
    this += new BagOfWordsEntropy(nameEntropy, {v:DocEntityVars => v.names})
  }

  def processDocs(docs:Iterable[Document]) {
    assert(docs.forall(_.hasAnnotation(classOf[WithinDocCoref])))

    // by mentions here we mean cross-doc mentions that correspond to within-doc entities
    val mentions = docs.flatMap { doc =>
      doc.coref.entities.map{ entity =>
        //todo is this unique across docs?
        new Mention[DocEntityVars](DocEntityVars.fromWithinDocEntity(entity), entity.uniqueId)
      }
    }


    val model = new DocEntityCorefModel()

    val sampler = new CorefSampler[DocEntityVars](model, mentions, 200000)
      with AutoStoppingSampler[DocEntityVars]
      with CanopyPairGenerator[DocEntityVars]
      with NoSplitMoveGenerator[DocEntityVars]
      with DebugCoref[DocEntityVars] {
      def newInstance(implicit d: DiffList) = new Node[DocEntityVars](new DocEntityVars())

      val autoStopThreshold = 10000
    }

    sampler.infer
  }
}

// todo settable variables like iterations, auto stop
// todo code for model training
// todo train model on tac entity linking
// todo serialize and deserialize models
// todo finish DocEntVars impl