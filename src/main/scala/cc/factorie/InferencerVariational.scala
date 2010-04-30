package cc.factorie

/*
class MeanFieldInferencer[A<:Variable with IterableSettings](model:Model, variables:Iterable[Variable with QDistribution]) {
  type VQ = Variable with QDistribution
  private val _q = new HashMap[Variable,GenerativeDistribution[_]]
  variables.foreach(v => _q(v) = v.newQ)
  def q[V<:Variable with QDistribution](v:V) = _q(v).asInstanceOf[V#QType]
  
  def infer(v:VQ): Unit = {
    require(_q.containsKey(v))
  }
  def sample(v:A)(implicit d:DiffList): Unit = {
    val factors = model.factors(v)
    val neighbors = factors.flatMap(_.variables).unique
    val variablesNeighbors = neighbors.filter(_q.containsKey(_))
    variablesNeighbors.foreach(_.preChange(v))
    proposals = for (diff <- v.settings) yield {
      val factors = diff.factorsOf[Template](model)
      val modelScore = diff.scoreAndUndo(model)
      new Proposal(diff, modelScore, Math.NaN_DOUBLE, modelScore)
    }
    val proposal = sampleExpProportionally(proposals, _.acceptanceScore)
    proposal.diff.redo
    variablesNeighbors.foreach(_.postChange(v))
  }

}
*/
