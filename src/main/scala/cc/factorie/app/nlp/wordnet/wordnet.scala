package cc.factorie.app.nlp.wordnet

import scala.collection.immutable.HashMap
import scala.collection.mutable.ArrayBuffer

class WordNet(rootDir: String) {

  /* for converting from hexidecimal */
  val HEX = 16

  /* WordNet Pointer Types */
  val HYPONYM  = "~"
  val HYPERNYM = "@"
  val ANTONYM  = "!"
  val HYPONYM_INSTANCE  = "~i"
  val HYPERNYM_INSTANCE = "@i"

  /* There are 2 types of files we deal with here for wordnet:
       1) the data file - this file has 1 line per synset and
          includes the ids of all related synsets. This means
          that each line includes the synsets that are hypernyms,
          hyponyms, antonyms, etc. of the current synset. This
          file also includse the words in this synset
       2) the index file - this file has 1 line per word and specifies
          among other things, all of the synsets that this word belongs to
     There are both a data and index file for each part of speech
   */
  val dataFilenames = HashMap[String, (String, String)](
    "n"  -> ("data.noun", "index.noun"),
    "v"  -> ("data.verb", "index.verb"),
    "aj" -> ("data.adj",  "index.adj"),
    "av" -> ("data.adv",  "index.adv")
  )

  /* checks if string is WordNet comment (line starts with 2 spaces) */
  private def isComment(s: String): Boolean = s.subSequence(0,2) == "  "

  /* INITIALIZE WordNet */
  var synsetsBuilder       = new HashMap[String, Synset]()
  var lemma2synsetsBuilder = new HashMap[String, ArrayBuffer[String]]()
  for (kv <- dataFilenames) {
    val prefix        = kv._1
    val dataFilename  = kv._2._1
    val indexFilename = kv._2._2

    val indexFile  = scala.io.Source.fromFile(rootDir + indexFilename)
    val indexLines = indexFile.getLines.mkString("\n").split("\n")
    indexFile.close()

    val dataFile  = scala.io.Source.fromFile(rootDir + dataFilename)
    val dataLines = dataFile.getLines.mkString("\n").split("\n")
    dataFile.close()

    val filteredIndexLines = indexLines.filter(line => !isComment(line)).map(line => line.split(" "))
    val filteredDataLines  = dataLines.filter(line => !isComment(line)).map(line => line.split(" "))

    /* Build a lemma 2 synset dictionary */
    for (line <- filteredIndexLines) {
      val word = line(0)
      var synL = line.drop(line.length - line(2).toInt).map(x => prefix + x)

      if (!lemma2synsetsBuilder.contains(word)) {
        lemma2synsetsBuilder += (word -> new ArrayBuffer())
      }
      lemma2synsetsBuilder(word) ++= synL
    }

    for (line <- filteredDataLines) {
      val id      = prefix + line(0)
      val nwords  = Integer.parseInt(line(3), HEX)  // convert from hex to int
      val nptrs   = Integer.parseInt(line(4 + nwords * 2))
      val ptrMap  = HashMap[String, scala.collection.mutable.Set[String]](
        HYPERNYM          -> new scala.collection.mutable.HashSet[String](),
        HYPERNYM_INSTANCE -> new scala.collection.mutable.HashSet[String](),
        ANTONYM           -> new scala.collection.mutable.HashSet[String]()
      )

      for (i <- 0 to nptrs - 1) {
        val l = line.drop(5 + nwords * 2 + i * 4).take(4)
        val (ptr, sid) = (l(0), l(1))
        if (ptr == HYPERNYM || ptr == ANTONYM || ptr == HYPERNYM_INSTANCE) {
          ptrMap(ptr) += (prefix + sid)
        }
      }
      val synPtrs = ptrMap.map(kv => (kv._1, kv._2.toSet)).toMap  // make sets immutable

      synsetsBuilder += (id ->
        new Synset(id, synPtrs(HYPERNYM) ++ synPtrs(HYPERNYM_INSTANCE), synPtrs(ANTONYM)
        )
      )
    }
  }

  val allSynsets    = synsetsBuilder.toMap        // make immutable
  val lemma2synsets = lemma2synsetsBuilder.map(x => (x._1,x._2.toList)).toMap

  /* pass in a lemmatized word and return a sequence of its synsets
   * See class below for more info on synsets */
  def synsets(s: String): Seq[Synset] = {
    val synsetIds = this.lemma2synsets.get(s)
    if (synsetIds == None){
      Seq[Synset]()
    } else {
      synsetIds.get.map(x => allSynsets(x)).toSeq
    }
  }

  /* tests if both words passed in are part of at least 1 of the same
   * synsets; both words passed in must be lemmatized */
  def areSynonyms(w1: String, w2: String): Boolean = {
    val synsetIds1 = this.lemma2synsets.get(w1)
    val synsetIds2 = this.lemma2synsets.get(w2)
    if (synsetIds1 == None || synsetIds2 == None) {
      false
    } else {
      !synsetIds1.get.toSet.intersect(synsetIds2.get.toSet).isEmpty
    }
  }

  /* tests if both words passed in are antonyms (are in antonym synsets) */
  def areAntonyms(w1: String, w2: String): Boolean = {
    val synsetIds1 = this.lemma2synsets.get(w1)
    val synsetIds2 = this.lemma2synsets.get(w2)
    if (synsetIds1 == None || synsetIds2 == None) {
      false
    } else {
      val antIds1 = synsetIds1.get.map(x => allSynsets(x).ants).flatten
      !antIds1.toSet.intersect(synsetIds2.get.toSet).isEmpty
    }
  }

  /* tests w1 is a hypernym of w2 */
  def isHypernymOf(w1: String, w2: String): Boolean = {
    val synsetIds1 = this.lemma2synsets.get(w1)
    val synsetIds2 = this.lemma2synsets.get(w2)
    if (synsetIds1 == None || synsetIds2 == None) {
      false
    } else {
      val hypIds2 = synsetIds2.get.map(x => allSynsets(x).hyps).flatten
      !hypIds2.toSet.intersect(synsetIds1.get.toSet).isEmpty
    }
  }

  /* tests if w1 is a hyponym of w2 */
  def isHyponymOf(w1: String, w2: String): Boolean = isHypernymOf(w2, w1)

  class Synset(val id: String, val hyps: Set[String], val ants: Set[String]) {
    def antonyms(): Set[Synset] = this.ants.map(x => allSynsets(x))

    /* get the parent synsets (hypernyms) of this synset */
    def hypernym(): Set[Synset] = this.hyps.map(x => allSynsets(x))

    /* recursively get all parent synsets (hypernyms) of this synset */
    def hypernyms(): Set[Synset] = {

      def allHypernyms(visited: Set[Synset], unvisited: Set[Synset]):
					      Set[Synset] = unvisited match {
        case Seq(_,_*) => allHypernyms(visited ++ unvisited,
          unvisited.map(x => x.hypernym()).flatten)
        case _ => visited
      }
      allHypernyms(Set[Synset](), this.hypernym())
    }
  }
}

object WordNet {
  def main(args: Array[String]) {
    val wn = new cc.factorie.app.nlp.wordnet.WordNet("/Users/akobren/software/dev/scala/wordnet/data/dict/")
    assert(wn.areAntonyms("good", "evil"))
    assert(!wn.areAntonyms("good", "star"))
    assert(wn.areAntonyms("right", "left"))
    assert(wn.areAntonyms("right", "wrong"))
    assert(!wn.areAntonyms("right", "wasdkjfklj"))

    assert(wn.isHypernymOf("red", "crimson"))
    assert(!wn.isHyponymOf("crimson", "crimson"))
    assert(wn.isHyponymOf("crimson", "red"))

    assert(wn.areSynonyms("soul", "person"))
    assert(!wn.areSynonyms("dog", "cat"))
    assert(!wn.areSynonyms("hot", "cold"))

    println("[done small tests.]")
  }
}
