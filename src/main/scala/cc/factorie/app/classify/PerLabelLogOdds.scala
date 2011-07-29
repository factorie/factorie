package cc.factorie.app.classify
import cc.factorie._

/** Calculate the weighted log-odds ratio:
    p(w|c) * log(p(w|c)/p(w|!c)) for each word w and label c.
    Ranks highly those words that substantially contribute to positively predicting label c.  
    @author Andrew McCallum
    @since 0.10
 */
class PerLabelLogOdds(instances:Iterable[InstanceVariable]) {
  val categoricalDomain: CategoricalDomain[String] = instances.head.domain.dimensionDomain
  val labelDomain: DiscreteDomain = instances.head.label.domain
  private val logodds = Array.ofDim[Double](labelDomain.size, categoricalDomain.size)
  var labelEntropies = new Array[Double](labelDomain.size)
  init(instances)
  
  def top(labelIndex:Int, n:Int): Seq[(String,Double)] = {
    val entries = logodds(labelIndex).zipWithIndex.sortBy({case (infogain, index) => -infogain}).take(n) //.toList
    entries.map({case (infogain,index)=>new Tuple2(categoricalDomain.getCategory(index).toString, infogain)})
  }
  def top(labelValue:DiscreteValue, n:Int): Seq[(String,Double)] = {
    require(labelValue.domain == labelDomain)
    top(labelValue.intValue, n)
  }
  
  protected def init(instances:Iterable[InstanceVariable]) {
    val numInstances = instances.size
    val instanceDomain = instances.head.domain
    val numFeatures = instanceDomain.dimensionDomain.size
    val labelDomain = instances.head.label.domain
    val numLabels = labelDomain.size
    
    val labelFeatureCounts = Array.ofDim[Double](numLabels, numFeatures)
    val featureCounts = new Array[Int](numFeatures)
    val labelCounts = new Array[Int](numLabels)
    
    for (instance <- instances) {
      assert(instance.domain == instanceDomain)
      assert(instance.label.domain == labelDomain)
      val labelIndex = instance.label.intValue
      labelCounts(labelIndex) += 1
      for (featureIndex <- instance.activeDomain) {
        labelFeatureCounts(labelIndex)(featureIndex) += 1
        featureCounts(featureIndex) += 1
      }
    }
    
    // Calculate per-class weighted-log-odds of each feature, and store it in "igs"
    for (fi <- 0 until numFeatures) {
      for (li <- 0 until numLabels) {
        if (featureCounts(fi) == 0) {
          logodds(li)(fi) = 0.0
        } else {
          if (labelFeatureCounts(li)(fi) == 0)
            logodds(li)(fi) = 0.0
          else {
            // Use +1 "Laplace" smoothing
            val pr_w_c = (labelFeatureCounts(li)(fi) + 1.0) / (labelCounts(li) + numFeatures) 
            val pr_w_nc = (featureCounts(fi) - labelFeatureCounts(li)(fi) + 1.0) / (numInstances - labelCounts(li) + numFeatures)
            logodds(li)(fi) = pr_w_c * math.log(pr_w_c / pr_w_nc)
          }
        }
      }
    }
    
  }

}