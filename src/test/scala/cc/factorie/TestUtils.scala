package cc.factorie

object TestUtils {
  import scala.collection.Seq

  /**
   * Return all combinations of n elements from
   * given sequence w/o replacement
   */
  def chooseN[T](seq:Seq[T], n:Int):Seq[Seq[T]] = { 
    // Helpers: 
    //   [(a, 1), (b, 2), ...] -> [a, b, ..]
    def removeZipIndex[T](ss:Seq[(T, Int)]) = ss.unzip._1
    //   [(a, 0), (b, 3), (c, 23)] -> 23
    def lastElemZipIndex[T](s:Seq[(T, Int)]) = s.last._2
    //   [(a, 0), (b, 1)] -> [a, b, c, d] -> [c, d]
    def remainingSeq[T](s:Seq[(T, Int)], seq:Seq[(T, Int)]) = seq.view(lastElemZipIndex(s)+1, seq.length)

    def choose[T](n:Int, seq:Seq[(T, Int)]):Seq[Seq[(T, Int)]] = n match {
      case 0 => Nil
      case 1 => seq.map(_ :: Nil)
      case i:Int => choose(i-1, seq) flatMap { 
        ll => remainingSeq(ll, seq).map { e => ll :+ e }}
    }
    choose(n, seq.zipWithIndex) map removeZipIndex _
  }
}
