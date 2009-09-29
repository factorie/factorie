package cc.factorie.example

object DirichletDemo {

  def main(args:Array[String]) : Unit = {
  	class Word(s:String) extends EnumVariable(s) with MultinomialOutcome[Word];
  	Domain += new StringDomain[Word] {
  		List("one", "two", "three", "four", "five", "six").foreach(index(_))
  	}

  	val dir  = new Dirichlet[Word](List(1.0,2.0,3.0,4.0,5.0,6.0)) with DirichletMomentMatchingEstimator[Word];
  	//val dir  = new Dirichlet[Word](List(1.0,1.0,1.0,1.0,1.0,1.0)) with DirichletMomentMatchingEstimator[Word];
  
  	val multinomials = for (i <- 1 to 10000) yield dir.sample
  
  	val dir2 = new Dirichlet[Word](1.0) with DirichletMomentMatchingEstimator[Word];
  	multinomials.foreach(m => m ~ dir2)

  	multinomials.take(5).foreach(m => {print("mult "); for (i <- 0 until 6) print(" "+m.pr(i)); println})
  
  	dir2.estimate
  	for (i <- 0 until 6) println(dir2.alpha(i))
  	0
  }
  
}
