package cc.factorie.app.nlp.segment
import cc.factorie.app.nlp.Document
import scala.collection.mutable.ArrayBuffer

/**
 * User: apassos
 * Date: 8/19/13
 * Time: 2:00 PM
 */
class BigramStatistics {
  val wordCounts = new collection.mutable.HashMap[String, Int]()
  val bigramCounts = new collection.mutable.LinkedHashMap[(String,String),Int]()
  var totalTokens = 0

  def process(document: Document): Unit = {
    for (token <- document.tokens) {
      totalTokens += 1
      wordCounts(token.string) = 1 + wordCounts.getOrElse(token.string, 0)
      token.getPrev.foreach(prev => {
        bigramCounts((prev.string,token.string)) = 1 + bigramCounts.getOrElse((prev.string,token.string), 0)
      })
    }
  }
  def process(documents: Iterable[Document]): Unit = documents.foreach(process)

  def getLikelyPhrases(countThreshold: Int = 5, scoreThreshold: Double = 100.0): Seq[Seq[String]] = {
    val bigramPhrases = collection.mutable.LinkedHashSet[Seq[String]]()
    val phraseStarts = collection.mutable.HashMap[String,ArrayBuffer[String]]()
    bigramCounts.foreach({ case ((prev,token),count) =>
      val pc = wordCounts(prev)
      val pt = wordCounts(token)
      if (count > countThreshold && pc > countThreshold && pt > countThreshold) {
        // Pointwise mutual information is defined as P(A,B) / P(A) P(B).
        // In this case P(A,B) = bigramCounts(A,B)/totalTokens ,
        // P(A) = wordCounts(A) / totalTokens, P(B) = wordCounts(B) / totalTokens
        // Hence we can write PMI = bigramCounts(A,B) * totalTokens / (wordCounts(A) * wordCounts(B))
        val score = totalTokens * count.toDouble / (pc * pt)
        if (score > scoreThreshold) {
          bigramPhrases += Seq(prev,token)
          phraseStarts.getOrElseUpdate(prev, new ArrayBuffer[String]).append(token)
        }
      }
    })
    // now we should have all interesting bigrams. I'll make the assumption that
    // if A B and B C are interesting phrases then A B C is interesting without checking.
    val trigramPhrases = collection.mutable.HashSet[Seq[String]]()
    bigramPhrases.foreach({ case Seq(prev,token) =>
      phraseStarts.getOrElse(token, Seq()).foreach(last => trigramPhrases += Seq(prev, token, last))
    })
    bigramPhrases.toSeq ++ trigramPhrases.toSeq
  }

  def topMutualInformationBigrams(threshold: Int = 5): Seq[(String,String,Double)] = {
    bigramCounts.toSeq.filter(_._2 > threshold).map({ case ((prev,token),count) =>
      ((prev,token),totalTokens * count.toDouble / (wordCounts(prev) * wordCounts(token)))
    }).sortBy(-_._2).take(100).map({case ((prev,token),score) => (prev,token,score)})
  }
}
