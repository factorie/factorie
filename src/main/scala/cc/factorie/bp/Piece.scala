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
                 val infer: (LatticeBP) => Unit,
                 val fg: LatticeBP) extends Piece {

  // compute the empirical counts of the model
  lazy val empiricalCounts: Map[DotFamily, Vector] = {
    //val diff = new DiffList
    vars.foreach(_.setToTarget(null))
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
    //diff.undo
    result
  }

  def truthScore: Double = {
    //val diff = new DiffList
    vars.foreach(_.setToTarget(null))
    val score = model.score(vars)
    //diff.undo
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
    val value = truthScore - fg.logZ()
    assert(!value.isNaN && !value.isInfinity && value < 0.0, {
      val sb = new StringBuffer
      sb append ("value: %f\n".format(value))
      sb append ("truthScore: %f\n".format(truthScore))
      sb append ("logZ: %f\n".format(fg.logZ(true)))
      sb.toString
    })
    (value, gradient)
  }
}

object ModelPiece {
  def apply(model: Model, vars: Seq[DiscreteVariable with VarWithTargetValue], families: Seq[DotFamily with Template], infer: (LatticeBP) => Unit, unrollVars: Seq[DiscreteVariable]) =
    new ModelPiece(model, vars, families, infer, {
      val fg = new LatticeBP(vars.toSet) with SumProductLattice
      fg.createFactors(model.factors(unrollVars))
      fg
    })

  def apply(model: Model, vars: Seq[DiscreteVariable with VarWithTargetValue], families: Seq[DotFamily with Template], infer: (LatticeBP) => Unit) =
    new ModelPiece(model, vars, families, infer, new LatticeBP(model, vars.toSet) with SumProductLattice)

  def apply(model: Model, vars: Seq[DiscreteVariable with VarWithTargetValue], families: Seq[DotFamily with Template]) =
    new ModelPiece(model, vars, families, (fg: LatticeBP) => new InferencerBPWorker(fg).inferTreewise(), new LatticeBP(model, vars.toSet) with SumProductLattice)

  def apply(model: Model, vars: Seq[DiscreteVariable with VarWithTargetValue]) =
    new ModelPiece(model, vars, model.familiesOfClass[DotFamily with Template],
      (fg: LatticeBP) => new InferencerBPWorker(fg).inferTreewise(),
      new LatticeBP(model, vars.toSet) with SumProductLattice)

}
