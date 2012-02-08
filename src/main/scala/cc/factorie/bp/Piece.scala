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

class ModelPiece(val model: Model, val vars: Seq[VarWithTargetValue], val infer: (LatticeBP) => Unit = new InferencerBPWorker(_).inferTreeUpDown(2, true)) extends Piece {

  // compute the empirical counts of the model
  lazy val empiricalCounts: Map[DotFamily, Vector] = {
    val diff = new DiffList
    vars.foreach(_.setToTarget(diff))
    val result: Map[DotFamily, Vector] = new HashMap
    for (dt <- model.familiesOfClass[DotFamily with Template]) {
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

  def truthScore: Double = {
    val diff = new DiffList
    vars.foreach(_.setToTarget(diff))
    val score = model.score(vars)
    diff.undo
    score
  }

  def valueAndGradient: (Double, Map[DotFamily, Vector]) = {
    val fg = new LatticeBP(model, vars.toSet) with SumProductLattice
    // perform BP
    infer(fg)
    // compute the gradient
    val exps = fg.statExpectations
    val gradient: Map[DotFamily, Vector] = new HashMap[DotFamily, Vector]
    for (df <- model.familiesOfClass[DotFamily with Template]) {
      val vector = new SparseVector(df.statisticsVectorLength)
      vector += (exps(df) * -1.0)
      vector += empiricalCounts(df)
      gradient(df) = vector
    }
    (truthScore - fg.logZ, gradient)
  }
}
