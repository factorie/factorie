package cc.factorie.app.nlp.embeddings;
import cc.factorie.optimize.Example
import cc.factorie.la.WeightsMapAccumulator
import cc.factorie.util.DoubleAccumulator
import cc.factorie.la.DenseTensor1
import cc.factorie.la.Tensor1


class CBOWNegSamplingEmbeddingModel(override val opts : EmbeddingOpts) extends WordEmbeddingModel(opts) {
       val rng = new util.Random
       val negative = opts.negative.value
       val window = opts.window.value
       override def getExamplesFromSingleDocument(doc : String) : Seq[Example] = {
            val examples = new collection.mutable.ArrayBuffer[Example]
            val sen = doc.stripLineEnd.split(' ').map(word => vocab.getId(word)).filter(id => id != -1)
            val senLength = sen.size
            for (senPosition <- 0 until senLength) {
                  val currWord = sen(senPosition) 
                  val contexts = new collection.mutable.ArrayBuffer[Int]
                  // get the contexts for currWord
                  val b = rng.nextInt(window)
                  for (a <- b until window * 2 + 1 - b) if (a != window ) {
                       val c = senPosition - window + a
                       if (c >=0 && c < senLength) 
                           contexts += sen(c)
                  }
                  // make the examples 
                  examples += new CBOWNegSamplingExample(this, currWord, contexts, 1)
                  (0 until negative).foreach( {
                    val negContext = vocab.getRandWordId
                    examples += new CBOWNegSamplingExample(this, currWord, List(negContext), -1)
                  })
            }
            examples     
      }
}




class CBOWNegSamplingExample(model : CBOWNegSamplingEmbeddingModel, word : Int, contexts : Seq[Int], label : Int) extends Example{
  
  // to understand the gradient and objective refer to : http://arxiv.org/pdf/1310.4546.pdf
  def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator): Unit = {
   
     val wordEmbedding = model.weights(word).value
     val contextEmbedding = new DenseTensor1(model.D, 0)
     contexts.foreach(context => contextEmbedding.+=( model.weights(context).value ) ) 
    
     val score : Double = wordEmbedding.dot(contextEmbedding)
     val exp : Double = math.exp(-score) // TODO : pre-compute , costly operation
     
     var objective : Double = 0.0
     var factor : Double = 0.0
     if (label == 1) { 
          objective = -math.log1p(exp)
          factor = exp/(1 + exp)
     }
     if (label == -1) {
          objective = -score -math.log1p(exp)
          factor = -1/(1 + exp)
     }     
    if (value ne null) value.accumulate(objective)
    if (gradient ne null) {
         contexts.foreach(context => gradient.accumulate(model.weights(context), wordEmbedding, factor) )
         gradient.accumulate(model.weights(word), contextEmbedding, factor)
    }
      
    
  }
}
