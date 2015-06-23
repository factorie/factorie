package cc.factorie.app.nlp.embedding
import scala.collection.mutable.LinkedHashMap
import cc.factorie.maths
import cc.factorie.la._

object Browse {
  
  // t1 is the anchor (with some zero dimensions ignored), t2 is the data (which may have fewer zeros)
  var zeroThreshold = 0.01
  def asymmetricDotSimilarity(t1:DenseTensor1, t2:DenseTensor1): Double = {
    val f1 = t1.map(x => if (math.abs(x) < zeroThreshold) 0.0 else x)
    //val f2 = t2.map(x => if (math.abs(x) < zeroThreshold) 0.0 else x)
    //new DenseTensor1(f1) cosineSimilarity new DenseTensor1(f2)
    new DenseTensor1(f1) cosineSimilarity new DenseTensor1(t2)
  }
  def asymmetricDotSimilarity2(t1:DenseTensor1, t2:DenseTensor1): Double = {
    val f1 = t1.map(x => if (math.abs(x) < zeroThreshold) 0.0 else x)
    val f2 = t2.map(x => if (math.abs(x) < zeroThreshold) 0.0 else x)
    new DenseTensor1(f1) cosineSimilarity new DenseTensor1(f2)
  }
  def main(args:Array[String]): Unit = {
    val embeddings = new LinkedHashMap[String,DenseTensor1]
    println("Reading embeddings...")
    var dim = -1
    var lineNum = 0
    for (line <- io.Source.fromFile(args(0)).getLines()) {
      val elts = line.split("\\s+")
      val t = new DenseTensor1(elts.drop(1).map(_.toDouble))
      // t.twoNormalize() // TODO Make this a command-line option
      embeddings(elts.head) = t
      if (dim > 0) { if (t.length != dim) println(s"At line $lineNum expected length $dim but got ${t.length}") } 
      else dim = t.length
      lineNum += 1
    }
    println(s"...Read $dim-dimensional embeddings for ${embeddings.size} words.")
    
    val prompt = "> "
    print(prompt); System.out.flush()
    val cosSimilarity: (DenseTensor1,DenseTensor1)=>Double = (t1,t2) => t1.cosineSimilarity(t2)   // cosine similarity
    val dotSimilarity: (DenseTensor1,DenseTensor1)=>Double = (t1,t2) => t1.dot(t2)                // dot product
    val sigSimilarity: (DenseTensor1,DenseTensor1)=>Double = (t1,t2) => maths.sigmoid(t1.dot(t2)) // sigmoid of dot product
    val maskedDotSimilarity: (DenseTensor1,DenseTensor1)=>Double = (t1,t2) => asymmetricDotSimilarity(t1, t2) //
    val maskedDotSimilarity2: (DenseTensor1,DenseTensor1)=>Double = (t1,t2) => asymmetricDotSimilarity2(t1, t2) //
    val euclideanSimilarity: (DenseTensor1,DenseTensor1)=>Double = (t1,t2) => 1.0 / t1.euclideanDistance(t2) // inverse of Euclidean distance
    var count = 10
    for (line <- io.Source.stdin.getLines()) {
      //val query = embeddings.getOrElse(line.stripLineEnd, null)
      //val query = line.split("\\s+").map(word => embeddings.getOrElse(word, null)).filter(_ eq null).foldLeft(new DenseTensor1(dim))((a,b) => {b += a; b})
      val query = new DenseTensor1(dim)
      val queryWords = line.split("\\s+")
      var similarity: (DenseTensor1,DenseTensor1)=>Double = (t1,t2) => {
        if (t1.length != t2.length) println(s"embedding.Browse t1=${t1.length} t2=${t2.length}")
        t1.cosineSimilarity(t2)
      }
      var operation = 1 // 1 for addition, -1 for subtraction
      for (word <- queryWords) {
        if (word.matches("\\d+")) count = word.toInt
        else if (word == "-") operation = -1
        else if (word == "+") operation = 1
        else if (word == "cos:") similarity = cosSimilarity 
        else if (word == "dot:") similarity = dotSimilarity 
        else if (word == "sig:") similarity = sigSimilarity
        else if (word == "asy:") similarity = asymmetricDotSimilarity
        else if (word == "asy2:") similarity = asymmetricDotSimilarity2
        else if (word == "euc:") similarity = euclideanSimilarity
        else if (word.matches("thresh=[\\d\\.]+")) zeroThreshold = word.split("=")(1).toDouble
        else if (word == "zero:") query.zero()
        else if (word.startsWith("[")) { // Expecting [2] for one-hot at dimension 2
          val oneHot = new DenseTensor1(dim)
          val i =  word.drop(1).dropRight(1).toInt
          oneHot(i) = 1.0
          if (operation == 1) query += oneHot else query -= oneHot
        } else {
          val embedding = embeddings.getOrElse(word, null)
          if (embedding eq null) println(s"'$word' is outside vocabulary.")
          else {
            if (operation == 1) query += embedding else query -= embedding
          }
        }
      }
      if (query.oneNorm != 0.0) {
        println("QUERY: "+line)
        val top = new cc.factorie.util.TopN[String](count)
        for (tuple <- embeddings) top += (0, similarity(query, tuple._2), tuple._1)
        for (entry <- top) 
          println(f"${entry.category}%-25s   ${entry.score}%3.8f    "+
              f"2norm=${embeddings(entry.category).twoNorm}%f3.5  "+
              f"min=${embeddings(entry.category).min}%f3.3  "+
              f"max=${embeddings(entry.category).max}%f3.3  "+
              f"absmin=${embeddings(entry.category).map(math.abs(_)).min}%f3.3  "+
              f"<${zeroThreshold}%1.1gcount=${embeddings(entry.category).filter(x => math.abs(x) < zeroThreshold).size}%d  "
              )
        println()
      }
      print(prompt); System.out.flush()
    }
  }
}
