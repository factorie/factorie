package cc.factorie.bp

import cc.factorie._
import collection.mutable.{Map, HashMap}
import la.{SparseVector, Vector}


/**
 * @author sameer
 * @date 12/22/11
 */

trait Piece {

  def valueAndGradient: (Double, Map[DotFamily, Vector])
}

class ModelPiece(val model: Model,
                 val vars: Seq[DiscreteVariable with VarWithTargetValue],
                 val families: Seq[DotFamily with Template],
                 val infer: (LatticeBP) => Unit) extends Piece {

  def this(model: Model, vars: Seq[DiscreteVariable with VarWithTargetValue], families: Seq[DotFamily with Template]) = this(model, vars, families, new InferencerBPWorker(_).inferTreewise())
  def this(model: Model, vars: Seq[DiscreteVariable with VarWithTargetValue]) = this(model, vars, model.familiesOfClass[DotFamily with Template])

  // compute the empirical counts of the model
  lazy val empiricalCounts: Map[DotFamily, Vector] = {
    val diff = new DiffList
    vars.foreach(_.setToTarget(diff))
    val result: Map[DotFamily, Vector] = new HashMap
    for (dt <- families) {
      // unroll the factors and count them up
      result(dt) = dt.factors(vars).foldLeft(
        new SparseVector(dt.statisticsVectorLength))((v, f) => {
        v += f.statistics.vector
        v
      })
    }
    // undo the change
    diff.undo
    result
  }

  // lattice to perform BP over
  val fg = new LatticeBP(model, vars.toSet) with SumProductLattice

  def truthScore: Double = {
    val diff = new DiffList
    vars.foreach(_.setToTarget(diff))
    val score = model.score(vars)
    diff.undo
    score
  }

  def valueAndGradient: (Double, Map[DotFamily, Vector]) = {
    // reset Messages
    fg.resetMessages
    // perform BP
    infer(fg)
    // compute the gradient
    val exps = fg.statExpectations
    val gradient: Map[DotFamily, Vector] = new HashMap[DotFamily, Vector]
    for (df <- families) {
      val vector = new SparseVector(df.statisticsVectorLength)
      val expv = exps.get(df)
      if (expv.isDefined) {
        vector += (expv.get * -1.0)
      }
      vector += empiricalCounts(df)
      gradient(df) = vector
    }
    (truthScore - fg.logZ, gradient)
  }
}
