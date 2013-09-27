package cc.factorie.app.topics.lda
import cc.factorie._
import scala.collection.mutable.ArrayBuffer
import cc.factorie.directed._
import cc.factorie.variable.{MassesVariable, CategoricalSeqDomain}

// Unfinished model similar to Labeled-LDA.

object Tag {
  var numTags = 0
}
class Tag(val name:String, val query:Seq[String]) {
  def this(name:String) = this(name, Seq(name))
  def matches(text:String): Boolean = text.contains(name) || query.exists(text.contains(_))
  val index = { Tag.numTags += 1; Tag.numTags - 1 }
}

class TaggedDocument(domain:CategoricalSeqDomain[String], name:String, tokens:Seq[String]) extends Document(domain, name, tokens) {
  val tags = new ArrayBuffer[Tag] 
}

object TaggedLDA {
  val tags = new ArrayBuffer[Tag]
  tags += new Tag("machine learning")
  tags += new Tag("natural language processing", Seq("part-of-speech"))
  tags += new Tag("speech recognition")
  tags += new Tag("neural networks", Seq("neural network"))

  val numTopics = tags.length + 10
  val alpha1 = 0.01
  val numIterations = 50
  val WordSeqDomain = new CategoricalSeqDomain[String] // { override val elementDomain = wordDomain }
  val WordDomain = WordSeqDomain.elementDomain
  val tokenizer = cc.factorie.app.strings.alphaSegmenter
  implicit val model = DirectedModel()
  implicit val random = new scala.util.Random(0)
  val lda = new LDA(WordSeqDomain, numTopics, alpha1)
  
  def main(args:Array[String]): Unit = {
    val directories =
      if (args.length > 0) args.toList 
      else if (true) List("11", "12", "10", "09", "08").take(4).map("/Users/mccallum/research/data/text/nipstxt/nips"+_)
      else if (false) List("acq", "earn", "money-fx").map("/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/"+_)
      else List("comp.graphics", "comp.os.ms-windows.misc", "comp.sys.ibm.pc.hardware", "comp.sys.mac.hardware").map("/Users/mccallum/research/data/text/20_newsgroups/"+_)
    for (directory <- directories) {
      println("Reading files from directory " + directory)
      for (file <- new java.io.File(directory).listFiles; if file.isFile) {
        print("."); Console.flush()
        val text = scala.io.Source.fromFile(file).mkString
        val doc = new TaggedDocument(WordSeqDomain, file.toString, tokenizer(text).map(_.toLowerCase).filter(!cc.factorie.app.strings.Stopwords.contains(_)).toIndexedSeq)
        lda.addDocument(doc, random)
        for (tag <- tags) if (tag.matches(text)) {
          doc.tags += tag
          val masses = MassesVariable.dense(numTopics, alpha1)
          //masses.+=(tag.index, 1.0) // TODO We need the new Masses/Tensor framework for this
          lda.model -= lda.model.parentFactor(doc.theta)
          doc ~ Dirichlet(masses)
        }
      }
      println()
    }
    println("Read "+lda.documents.size+" documents, "+WordDomain.size+" word types, "+lda.documents.map(_.ws.length).sum+" word tokens.")
    
    val startTime = System.currentTimeMillis
    lda.inferTopics(numIterations, 10)
    println("Finished in " + ((System.currentTimeMillis - startTime) / 1000.0) + " seconds")
  }
}