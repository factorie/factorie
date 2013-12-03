package cc.factorie.tutorial
import scala.collection.mutable.ArrayBuffer
import java.io.File
import cc.factorie.app.strings.Stopwords
import cc.factorie.app.strings.alphaSegmenter
import cc.factorie.app.topics.lda.SparseLDAInferencer
import cc.factorie.directed._
import cc.factorie.variable._

/**
 * LDA example using the SparseLDAInferencer, very efficient.
 */
object EfficientLDA {
  val numTopics = 15
  val beta1 = 0.1
  val alpha1 = 0.1
  val fitDirichlet = false

  implicit val model = DirectedModel()
  object ZDomain extends DiscreteDomain(numTopics)
  object ZSeqDomain extends DiscreteSeqDomain { def elementDomain = ZDomain }
  class Zs(len:Int) extends DiscreteSeqVariable(len) { 
    def domain = ZSeqDomain
  }
  object WordSeqDomain extends CategoricalSeqDomain[String]
  val WordDomain = WordSeqDomain.elementDomain
  class Document(name:String, myTheta:ProportionsVariable, myZs:Zs, words:Seq[String]) extends cc.factorie.app.topics.lda.Document(WordSeqDomain, name, words) {
    this.theta = myTheta
    this.zs = myZs
  }
  val beta = MassesVariable.growableUniform(WordDomain, beta1)
  val alphas = MassesVariable.dense(numTopics, alpha1)

  def main(args: Array[String]): Unit = {
    implicit val random = new scala.util.Random(0)
    val directories = 
      if (args.length > 0) args.toList 
      else if (true) List("11", "12", "10", "09", "08").take(4).map("/Users/mccallum/research/data/text/nipstxt/nips"+_)
      else if (false) List("acq", "earn", "money-fx").map("/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/"+_)
      else List("comp.graphics", "comp.os.ms-windows.misc", "comp.sys.ibm.pc.hardware", "comp.sys.mac.hardware").map("/Users/mccallum/research/data/text/20_newsgroups/"+_)
    val phis = Mixture(numTopics)(ProportionsVariable.growableDense(WordDomain) ~ Dirichlet(beta))
    val documents = new ArrayBuffer[Document]
    val stopwords = new Stopwords; stopwords += "rainbownum"
    for (directory <- directories) {
      for (file <- new File(directory).listFiles; if file.isFile) {
        val theta = ProportionsVariable.sortedSparseCounts(numTopics) ~ Dirichlet(alphas)
        val tokens = alphaSegmenter(file).map(_.toLowerCase).filter(!stopwords.contains(_)).toSeq
        val zs = new Zs(tokens.length) :~ PlatedDiscrete(theta)
        documents += new Document(file.toString, theta, zs, tokens) ~ PlatedCategoricalMixture(phis, zs)
      }
    }

    val sampler = SparseLDAInferencer(ZDomain, WordDomain, documents, alphas.value, beta1, model)

    for (i <- 1 to 30) {
      for (doc <- documents) sampler.process(doc.zs)
      if (i % 5 == 0) {
        sampler.export(phis)
        if (fitDirichlet) {
          sampler.exportThetas(documents)
          MaximizeDirichletByMomentMatching(alphas, model)
          sampler.resetSmoothing(alphas.value, beta1)
        }
      }
    }
  }
}