package cc.factorie.example

import scala.collection.mutable.{ArrayBuffer,HashMap,HashSet,ListBuffer}
import scala.collection.jcl.WeakHashMap
import scala.util.Sorting
import scala.reflect.Manifest
import scala.util.matching.Regex
import java.io.File

import cc.factorie.util.Stopwords
import cc.factorie.util.Implicits._

object LDADemo {
  
  // Declare different types of variables
	object Beta extends SymmetricDirichlet[Word](0.01)
  class Topic extends Multinomial[Word] with MixtureComponent[Topic]
  class Z extends MixtureChoice[Topic,Z]; Domain.alias[Z,Topic]
  object Alpha extends SymmetricDirichlet[Z](1.0)
	class Theta extends Multinomial[Z]
	class Word(s:String) extends CoordinatedEnumVariable(s) with MultinomialOutcome[Word]
 	class Document(val file:String) extends ArrayBuffer[Word] { var theta:Theta = _ }

  def main(args: Array[String]) : Unit = {
  	// Read observed data and create Documents
		val documents = new ListBuffer[Document];
		val lexer = new Regex("[a-zA-Z]+")
		for (directory <- if (args.length > 0) args else List("/Users/mccallum/research/data/text/nipstxt/nips05")) {
			for (file <- new File(directory).listFiles; if (file.isFile)) {
				val d = new Document(file.toString)
				d ++= lexer.findAllIn(file.contentsAsString).toList.map(_ toLowerCase).filter(!Stopwords.contains(_)).map(new Word(_))
				documents += d
			}
		}
		println("Read "+documents.size+" documents with "+documents.foldLeft(0)(_+_.size)+" tokens and "+Domain[Word].size+" types.")
  
		// Create random variables
    val numTopics = 5
    val topics = Array.fromFunction(i => new Topic ~ Beta)(numTopics)
		val zs = new ArrayBuffer[Z] 	
  	for (document <- documents) {
  		document.theta = new Theta :~ Alpha
  		for (word <- document) {
  			val z = new Z ~ document.theta
  			word ~ z
  			zs +=z // just to gather the variables we need to sample later 
  		}
  	}
    
		// Fit model 
		val sampler = new GibbsSampler1
		val startTime = System.currentTimeMillis
    for (i <- 1 to 20) {
      sampler.sample(zs, 1)
    	//zs.foreach(sampler.sample(_))
    	print("."); Console.flush
    	if (i % 5 == 0) {
    		println ("Iteration "+i)
    		topics.foreach(t => println("Topic "+t.index+"  "+t.top(20).map(_._1)))
    		println
      }
    }	
    topics.foreach(t => {println("\nTopic "+t.index); t.top(20).foreach(x => println("%-16s %f".format(x._1,x._3)))})
		println("Finished in "+((System.currentTimeMillis-startTime)/1000.0)+" seconds")
    
		0
	}

}

