package cc.factorie.app.classify
import cc.factorie._
import cc.factorie.util.TopN
import cc.factorie.variable._

/** Calculate the weighted log-odds ratio:
    p(w|c) * log(p(w|c)/p(w|!c)) for each word w and label c.
    Ranks highly those words that substantially contribute to positively predicting label c.  
    @author Andrew McCallum
    @since 0.10
 */
class PerLabelLogOdds[L<:DiscreteVar,F<:VectorVar](labels:Iterable[L], labelToFeatures:L=>F) {
  val instanceDomain: VectorDomain = labelToFeatures(labels.head).domain
  val featureDomain: DiscreteDomain = instanceDomain.dimensionDomain
  val labelDomain: DiscreteDomain = labels.head.domain
  private val logodds = Array.ofDim[Double](labelDomain.size, featureDomain.size)
  var labelEntropies = new Array[Double](labelDomain.size)
  init(labels)
  
  def top(labelIndex:Int, n:Int): TopN[String] = new TopN[String](n, logodds(labelIndex), featureDomain.asInstanceOf[CategoricalDomain[String]].categories)
  def top(labelValue:DiscreteValue, n:Int): TopN[String] = {
    require(labelValue.dim1 == labelDomain.size) // require(labelValue.domain == labelDomain)
    top(labelValue.intValue, n)
  }
  
  protected def init(labels:Iterable[L]) {
    val numInstances = labels.size
    val numFeatures = featureDomain.size
    val numLabels = labelDomain.size
    
    val labelFeatureCounts = Array.ofDim[Double](numLabels, numFeatures)
    val featureCounts = new Array[Int](numFeatures)
    val labelCounts = new Array[Int](numLabels)
    
    for (label <- labels) {
      val instance = labelToFeatures(label)
      assert(instance.domain == instanceDomain)
      assert(label.domain == labelDomain)
      val labelIndex = label.intValue
      labelCounts(labelIndex) += 1
      //for (featureIndex <- instance.activeDomain) 
      instance.value.foreachActiveElement((featureIndex,featureValue) => {
        labelFeatureCounts(labelIndex)(featureIndex) += 1
        featureCounts(featureIndex) += 1
      })
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