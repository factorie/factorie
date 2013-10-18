package cc.factorie.app

package object nlp {

  /** Mapping from annotation class (usually stored in an attr) and the DocumentAnnotor from which it can be obtained. */
  type DocumentAnnotatorMap = collection.Map[Class[_], () => DocumentAnnotator]

  /** Given a sequence of strings describing labels in IOB format, such as O I-PER I-LOC B-LOC I-LOC O I-ORG,
      (where I, B prefixes are separated by a dash from the type suffix)
      return a sequence of tuples indicating span start, length and label suffix, such as (3, 2, "LOC"). */
  def iobBoundaries(labels:Seq[String]): Seq[(Int,Int,String)] = {
    val result = new scala.collection.mutable.ArrayBuffer[(Int,Int,String)]
    val strings = labels.map(_.split('-'))
    val iobs = strings.map(_.apply(0))
    val types = strings.map(a => if (a.length > 1) a(1) else "")
    var start = -1; var length = 0; var prevType = ""
    for (i <- 0 until labels.length) {
      val atBoundary = types(i) != prevType || iobs(i) == "B"
      if (start >= 0 && atBoundary) { result.+=((start, i-start, types(i))); start = -1 }
      if (types(i) != "" && atBoundary) start = i
      prevType = types(i)
    }
    result
  }

  def bilouBoundaries(labels:Seq[String]): Seq[(Int,Int,String)] = {
    val result = new scala.collection.mutable.ArrayBuffer[(Int,Int,String)]
    val strings = labels.map(_.split('-'))
    val bilous = strings.map(_.apply(0))
    val types = strings.map(a => if (a.length > 1) a(1) else "")
    var start = -1; var length = 0; var prevType = ""
    for (i <- 0 until labels.length) {
      val atBoundary = bilous(i) == "B" || bilous(i) == "U"
      if (bilous(i) == "U") result.+=((i, 1, types(i)))
      else if (start >= 0 && atBoundary) { result.+=((start, i-start, types(i))); start = -1 }
      if (types(i) != "" && atBoundary) start = i
      prevType = types(i)
    }
    result
  }
  
  def bioBoundaries(labels:Seq[String]): Seq[(Int,Int)] = ???
}