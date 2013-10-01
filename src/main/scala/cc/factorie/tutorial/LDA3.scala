package cc.factorie.tutorial
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer}
import scala.util.matching.Regex
import scala.io.Source
import java.io.File
import cc.factorie._
import cc.factorie.directed._
import cc.factorie.app.strings.Stopwords
import cc.factorie.app.strings.alphaSegmenter
import cc.factorie.app.topics.lda.SparseLDAInferencer
import cc.factorie.util.DoubleSeq
import cc.factorie.directed._
import cc.factorie.variable._

object LDA3 {
  val numTopics = 15
  val beta1 = 0.1
  val alpha1 = 0.1
  val fitDirichlet = false

  implicit val model = DirectedModel()
  object ZDomain extends DiscreteDomain(numTopics)
  object ZSeqDomain extends DiscreteSeqDomain { def elementDomain = ZDomain }
  class Zs(len:Int) extends DiscreteSeqVariable(len) { 
    def domain = ZSeqDomain
    //def words: Words = model.childFactors(this).first.asInstanceOf[PlatedDiscreteMixture.Factor]._1.asInstanceOf[Words]
  }
  object WordSeqDomain extends CategoricalSeqDomain[String]
  val WordDomain = WordSeqDomain.elementDomain
//  class Words(strings:Seq[String]) extends CategoricalSeqVariable(strings) {
//    def domain = WordSeqDomain
//    def zs = model.parentFactor(this).asInstanceOf[PlatedDiscreteMixture.Factor]._3.asInstanceOf[Zs]
//  }
  //class Document(val file:String, val theta:ProportionsVar, strings:Seq[String]) extends Words(strings)
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
      // println("Reading files from directory " + directory)
      for (file <- new File(directory).listFiles; if file.isFile) {
        // print("."); Console.flush
        val theta = ProportionsVariable.sortedSparseCounts(numTopics) ~ Dirichlet(alphas)
        val tokens = alphaSegmenter(file).map(_.toLowerCase).filter(!stopwords.contains(_)).toSeq
        val zs = new Zs(tokens.length) :~ PlatedDiscrete(theta)
        documents += new Document(file.toString, theta, zs, tokens) ~ PlatedCategoricalMixture(phis, zs)
      }
      // println()
    }
    // println("Read "+documents.size+" documents, "+WordDomain.size+" word types, "+documents.map(_.length).sum+" word tokens.")
    
    //val collapse = new ArrayBuffer[Variable]
    //collapse += phis
    //collapse ++= documents.map(_.theta)
    //val sampler = new CollapsedGibbsSampler(collapse) { def export(m:Seq[Proportions]): Unit = {} }
    //val sampler = new SparseLDAInferencer(numTopics, documents, alphas.tensor, beta1)
    val sampler = SparseLDAInferencer(ZDomain, WordDomain, documents, alphas.value, beta1, model)

    val startTime = System.currentTimeMillis
    for (i <- 1 to 30) {
      for (doc <- documents) sampler.process(doc.zs)
      if (i % 5 == 0) {
        // println("Iteration " + i)
        sampler.export(phis)
        if (fitDirichlet) {
          sampler.exportThetas(documents)
          MaximizeDirichletByMomentMatching(alphas, model)
          sampler.resetSmoothing(alphas.value, beta1)
          // println("alpha = " + alphas.tensor.toSeq.mkString(" "))
          // phis.zipWithIndex.map({case (phi:ProportionsVar, index:Int) => (phi, alphas(index))}).sortBy(_._2).map(_._1).reverse.foreach(t => println("Topic " + phis.indexOf(t) + "  " + t.tensor.top(10).map(dp => WordDomain.category(dp.index)).mkString(" ")+"  "+t.tensor.masses.massTotal.toInt+"  "+alphas(phis.indexOf(t))))
        } else {
          // phis.foreach(t => println("Topic " + phis.indexOf(t) + "  " + t.tensor.top(10).map(dp => WordDomain.category(dp.index)).mkString(" ")+"  "+t.tensor.masses.massTotal.toInt+"  "+alphas(phis.indexOf(t))))
        }
        // println
      }
    }
    //phis.foreach(t => {println("\nTopic "+phis.indexOf(t)); t.top(20).foreach(x => println("%-16s %f".format(x.value,x.pr)))})
    // println("Finished in " + ((System.currentTimeMillis - startTime) / 1000.0) + " seconds")
  }
}