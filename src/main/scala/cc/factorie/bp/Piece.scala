package cc.factorie.bp

import cc.factorie._
import collection.mutable.{Map, HashMap}
import cc.factorie.bp.specialized._
import cc.factorie.la._
import specialized.Viterbi

/**
 * @author sameer
 * @date 12/22/11
 */

trait Piece {

  def valueAndGradient: (Double, Map[DotFamily, Tensor])
}

class ModelPiece(val vars: Seq[DiscreteVariable with VarWithTargetValue],
                 val families: Seq[DotFamily with Template],
                 val infer: (LatticeBP) => Unit,
                 val fg: LatticeBP) extends Piece {

  // compute the empirical counts of the model
  val empiricalCounts: Map[DotFamily, Tensor] = {
    //val diff = new DiffList
    vars.foreach(_.setToTarget(null))
    val result: Map[DotFamily, Tensor] = new HashMap
    // TODO currently fT, when f factors and T templates
    for (dt <- families) {
      val vector = Tensor.newSparse(dt.weights) // new SparseVector(dt.statisticsVectorLength)
      for (factor <- fg.factors) {
        factor match {
          case f: DotFamily#Factor => {
            if (f.family == dt)
              vector += f.statistics.tensor
          }
          case _ =>
        }
      }
      result(dt) = vector
    }
    // undo the change
    //diff.undo
    result
  }

  def truthScore: Double = {
    //val diff = new DiffList
    vars.foreach(_.setToTarget(null))
    var score = 0.0
    for (factor <- fg.factors) {
      score += factor.score
    }

    //diff.undo
    score
  }

  def valueAndGradient: (Double, Map[DotFamily, Tensor]) = {
    // reset Messages
    fg.resetMessages
    // perform BP
    infer(fg)
    // compute the gradient
    val exps = fg.statExpectations
    val gradient: Map[DotFamily, Tensor] = new HashMap[DotFamily, Tensor]
    for (df <- families) {
      val vector = Tensor.newSparse(df.weights) //new SparseVector(df.statisticsVectorLength)
      val expv = exps.get(df)
      if (expv.isDefined) {
        assert(expv.get.length == df.weights.length) //statisticsVectorLength
        vector -= expv.get //* -1.0
      }
      vector += empiricalCounts(df)
      gradient(df) = vector
    }
    val value = truthScore - fg.logZ()
    assert(!value.isNaN && !value.isInfinity && value <= 1e-10, {
      val sb = new StringBuffer
      sb append ("value: %f\n".format(value))
      sb append ("truthScore: %f\n".format(truthScore))
      sb append ("numFactors: %d\n".format(fg.factors.size))
      sb append ("numVars: %d\n".format(fg.nodes.size))
      sb append ("logZ: %f\n".format(fg.logZ(true)))
      var exactZ = 0.0
      val model = new FactorModel(fg.factors.toSeq: _*)
      val iterator = new VariablesSettingsSampler[DiscreteVariable](null).settings(fg.varying.toSeq)
      for (setting <- iterator) {
        sb append "Setting: "
        sb append fg.varying.mkString(",")
        sb append "\n"
        val score = model.score(fg.varying)
        sb append "Score: "
        sb append score
        sb append "\n"
        exactZ += math.exp(score)
      }
      sb append ("exact logZ: " + math.log(exactZ) + "\n")
      sb.toString
    })
    (value, gradient)
  }
}

object ModelPiece {
  def apply(model: Model, vars: Seq[DiscreteVariable with VarWithTargetValue], families: Seq[DotFamily with Template], infer: (LatticeBP) => Unit, unrollVars: Seq[DiscreteVariable]) =
    new ModelPiece(vars, families, infer, {
      val fg = new LatticeBP(vars.toSet) with SumProductLattice
      fg.createFactors(model.factors(unrollVars))
      fg
    })

  def apply(model: Model, vars: Seq[DiscreteVariable with VarWithTargetValue], families: Seq[DotFamily with Template], infer: (LatticeBP) => Unit) =
    new ModelPiece(vars, families, infer, new LatticeBP(model, vars.toSet) with SumProductLattice)

  def apply(model: Model, vars: Seq[DiscreteVariable with VarWithTargetValue], families: Seq[DotFamily with Template]) =
    new ModelPiece(vars, families, (fg: LatticeBP) => new InferencerBPWorker(fg).inferTreewise(), new LatticeBP(model, vars.toSet) with SumProductLattice)

  def apply(model: Model, vars: Seq[DiscreteVariable with VarWithTargetValue]) =
    new ModelPiece(vars, model.familiesOfClass[DotFamily with Template],
      (fg: LatticeBP) => new InferencerBPWorker(fg).inferTreewise(),
      new LatticeBP(model, vars.toSet) with SumProductLattice)

}


// Identical to ModelPiece except fg is not kept around after the gradient calculation
class TransientModelPiece (
          val vars: Seq[DiscreteVariable with VarWithTargetValue],
          val families: Seq[DotFamily with Template],
          val model: Model,
          val infer: (LatticeBP) => Unit)
    extends Piece {

  def newFG() = new LatticeBP(model, vars.toSet) with SumProductLattice

  // compute the empirical counts of the model
  def empiricalCounts(): Map[DotFamily, Tensor] = {
    //val diff = new DiffList
    vars.foreach(_.setToTarget(null))
    val result: Map[DotFamily, Tensor] = new HashMap
    // TODO currently fT, when f factors and T templates
    for (dt <- families) {
      val vector = Tensor.newSparse(dt.weights) //new SparseVector(dt.statisticsVectorLength)
      for (factor <- newFG().factors) {
        factor match {
          case f: DotFamily#Factor => {
            if (f.family == dt)
              vector += f.statistics.tensor
          }
          case _ =>
        }
      }
      result(dt) = vector
    }
    // undo the change
    //diff.undo
    result
  }

  def truthScore(fg: LatticeBP): Double = {
    //val diff = new DiffList
    vars.foreach(_.setToTarget(null))
    var score = 0.0
    for (factor <- fg.factors) {
      score += factor.score
    }

    //diff.undo
    score
  }

  def valueAndGradient: (Double, Map[DotFamily, Tensor]) = {
    var fg = newFG

    // perform BP
    infer(fg)

    // compute the gradient
    val exps = fg.statExpectations

    // compute logZ
    val value = truthScore(fg) - fg.logZ()

    fg = null // ensure garbage collection

    val gradient: Map[DotFamily, Tensor] = new HashMap[DotFamily, Tensor]
    val ec = empiricalCounts()
    for (df <- families) {
      val vector = Tensor.newSparse(df.weights) //new SparseVector(df.statisticsVectorLength)
      val expv = exps.get(df)
      if (expv.isDefined) {
        assert(expv.get.length == df.weights.length) //statisticsVectorLength
        vector += (expv.get * -1.0)
      }
      vector += ec(df)
      gradient(df) = vector
    }
    (value, gradient)
  }
}

object TransientModelPiece {
  def apply(model: Model, vars: Seq[DiscreteVariable with VarWithTargetValue]) =
    new TransientModelPiece(
      vars,
      model.familiesOfClass[DotFamily with Template],
      model,
      (fg: LatticeBP) => new InferencerBPWorker(fg).inferTreewise()
    )
}

/**
 * @author brian martin
 * @date 2/24/11
 *
 * This class makes some assumptions about the model to preserve efficiency.
 * The transTemplate should be such that unroll1 goes to the previous label,
 * and unroll2 goes to the next label.
 *
 * It also assumes that vars are given in the same order as they appear in the chain.
 */
class PerceptronChainPiece[LV <: LabelVariable[_], OV <: DiscreteTensorVar](
                                                                                 localTemplate: TemplateWithDotStatistics2[LV, OV],
                                                                                 transTemplate: TemplateWithDotStatistics2[LV, LV],
                                                                                 vars: Array[LV])
      extends Piece {

  def search(vs: Seq[LV]): Unit = Viterbi.searchAndSetToMax(vs, localTemplate, transTemplate)

  def valueAndGradient: (Double, Map[DotFamily, Tensor]) = {
    // do beam search
    vars.foreach(_.setRandomly())
    search(vars)
    val localGradient = Tensor.newSparse(localTemplate.weights) // new SparseVector(localTemplate.statisticsVectorLength)
    val transGradient = Tensor.newSparse(transTemplate.weights) // new SparseVector(transTemplate.statisticsVectorLength)

    // get local gradient
    for (v <- vars) {
      val localStats = localTemplate.unroll1(v).head.asInstanceOf[DotFamily#Factor].statistics.tensor
      localGradient -= localStats //* -1.0
      if (!transTemplate.unroll2(v).isEmpty) {
        val transStats = transTemplate.unroll2(v).head.asInstanceOf[DotFamily#Factor].statistics.tensor
        transGradient -= transStats //* -1.0
      }
    }
    val d = new DiffList()
    vars.foreach(_.setToTarget(d))
    for (v <- vars) {
      val localStats2 = localTemplate.unroll1(v).head.asInstanceOf[DotFamily#Factor].statistics.tensor
      localGradient += localStats2 // * 1.0
      if (!transTemplate.unroll2(v).isEmpty) {
        val transStats2 = transTemplate.unroll2(v).head.asInstanceOf[DotFamily#Factor].statistics.tensor
        transGradient += transStats2
      }
    }
    d.undo

    (-1.0, Map(localTemplate -> localGradient, transTemplate -> transGradient))
  }
}

class ForwardBackwardPiece[LV <: LabelVariable[_], OV <: DiscreteTensorVar](
                                                                                 vars: Array[LV],
                                                                                 localTemplate: TemplateWithDotStatistics2[LV, OV],
                                                                                 transTemplate: TemplateWithDotStatistics2[LV, LV])
      extends Piece {

  assert(vars.size > 0, "Piece has no variables.")

  val families = Seq(localTemplate, transTemplate).map(_.asInstanceOf[DotFamily with Template])

  val empiricalCounts: Map[DotFamily, Tensor] = {
    vars.foreach(_.setToTarget(null))
    val result: Map[DotFamily, Tensor] = new HashMap
    for (dt <- families) {
      val vector = Tensor.newSparse(dt.weights) //new SparseVector(dt.statisticsVectorLength)
      for (factor <- dt.factors(vars))
        vector += factor.statistics.tensor
      result(dt) = vector
    }
    result
  }

  val m = new TemplateModel(localTemplate, transTemplate)

  def truthScore: Double = {
    vars.foreach(_.setToTarget(null))
    m.score(vars)
  }

  def valueAndGradient: (Double, Map[DotFamily, Tensor]) = {
    val (exps, logZ) = ForwardBackward.featureExpectationsAndLogZ(vars, localTemplate, transTemplate)
    val gradient = new HashMap[DotFamily, Tensor]
    for (df <- families) {
      val vector = Tensor.newSparse(df.weights) // new SparseVector(df.statisticsVectorLength)
      vector += (exps(df) * -1.0)
      vector += empiricalCounts(df)
      gradient(df) = vector
    }
    (truthScore - logZ, gradient)
  }
}

