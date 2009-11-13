package cc.factorie

/* package */ object factorie {

  def repeat[T](n:Int)(f: =>T) : Iterable[T] = for (i <- 0 until n force) yield f
  def repeat(n:Int)(f: =>Unit) : Unit = for (i <- 0 until n force) f
  def time(f: =>Unit) : Long = {
    val start = System.currentTimeMillis
    f
    start - System.currentTimeMillis
  }
  def printTime(f: =>Unit) : Unit = println(time(f)/1000.0+" seconds")

}
