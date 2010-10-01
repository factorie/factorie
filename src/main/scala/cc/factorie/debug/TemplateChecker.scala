package cc.factorie.debug

import cc.factorie.{Factor, Template2, Variable}

/**
 * A tool for debugging models and template
 * @author Michael Wick and Sameer Singh
 * @since 0.9
 * @see Template
 */
object TemplateChecker {
  /**
   * Checks whether the unroll methods of a Template2 are "consistent"
   * Consistency is defined by ensuring a variable is obtained when calling unroll on its neighbors
   */
  def checkTemplate[N1 <: Variable, N2 <: Variable](variable: N1, t: Template2[N1, N2]): Boolean = {
    // find the factors that are affected by a change to this variable
    val factorsFromUnroll1: Iterable[Factor] = t.unroll1(variable)
    // first and second neighbors of these factors
    val firstNeighbors = factorsFromUnroll1.map((f: Factor) => f.variable(0))
    val secondNeighbors = factorsFromUnroll1.map((f: Factor) => f.variable(1))
    // get the factors resulting in unroll2 on second neighbors
    val factorsFromUnroll2: Iterable[Factor] = secondNeighbors.flatMap(x => t.unroll2(x.asInstanceOf[N2]))
    // get the first neighbors there
    val firstNeighborsFromUnroll2 = factorsFromUnroll2.map((f: Factor) => f.variable(0))
    // check whether the first neighbors from unroll1 are present in first neighbors from unroll2
    var result: Boolean = true
    for (neighbor1 <- firstNeighbors) {
      var present: Boolean = false
      for (neighbor2 <- firstNeighborsFromUnroll2) {
        present |= (neighbor1 == neighbor2)
      }
      result &= present
    }
    result
  }
}
