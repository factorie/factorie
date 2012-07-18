package cc.factorie.app.bib

object Evaluator{
  def eval(predicted:Seq[AuthorEntity]):Unit ={
    pairF1(predicted)
    bcubedF1(predicted.filter(_.groundTruth != None))
  }
  def pairF1(predicted:Seq[AuthorEntity]):Unit = {
    val labeled = predicted.filter(_.groundTruth != None)
    pairF1(labeled.map(_.entityRoot.id.toString),labeled.map(_.groundTruth.get))
  }
  def pairF1(predictedClusters:Seq[String],trueClusters:Seq[String]):Unit ={
    var fp=0.0
    var fn=0.0
    var tp=0.0
    for(i<-0 until predictedClusters.size){
      val predictedClusterKeyI = predictedClusters(i)
      val trueClusterKeyI = trueClusters(i)
      for(j<-i+1 until predictedClusters.size){
        val predictedClusterKeyJ=predictedClusters(j)
        val trueClusterKeyJ=trueClusters(j)
        if(predictedClusterKeyI==predictedClusterKeyJ){
          if(trueClusterKeyI==trueClusterKeyJ)tp += 1.0
          else fp +=1.0
        }
        else if(trueClusterKeyI==trueClusterKeyJ){
          fn += 1.0
        }
      }
    }
    println("Num labeled: "+predictedClusters.size+" tp: "+tp+" fp: "+fp+" fn: "+fn)
    val p = tp/(tp+fp)
    val r = tp/(tp+fn)
    val f1 = 2*p*r/(p+r)
    println("Pair F1: p="+p+" r="+r+" f1="+f1)
  }
  def bcubedF1(predictedClustering:Seq[AuthorEntity]):Unit ={
    println("predictedClustering.size: "+predictedClustering.size)
    var p: Double = 0.0
    var r: Double = 0.0
    for (i <- 0 until predictedClustering.size){
      val entity = predictedClustering(i)
      val predLabel = entity.entityRoot.id.toString//d.mentions(i).entity
      val truths = predictedClustering.filter((e:AuthorEntity)=>{e.groundTruth!=None && e.groundTruth.get==entity.groundTruth.get}).toSet//predictedClustering.getMentionsFromEID(d, d.mentions(i))
      val preds = (entity.entityRoot.descendantsOfClass[AuthorEntity]).filter((e:AuthorEntity)=>e.groundTruth!=None).toSet
      var correct: Int = 0
      for (j <- preds)
        for (k <- truths)
          if (k.id == j.id)
            correct += 1
      //println("correct:"+correct+" |preds|:"+preds.size.asInstanceOf[Double]+" |truths|:"+truths.size.asInstanceOf[Double])
      p += correct.asInstanceOf[Double] / preds.size.asInstanceOf[Double]
      r += correct.asInstanceOf[Double] / truths.size.asInstanceOf[Double]
      if(correct>preds.size){
        println("ERROR: correct>preds.size")
        println("  correct: "+correct)
        println("  preds: "+preds.size)
        println("  truths: "+truths.size)
      }
    }
    println("p: "+p+" predictedClusteringSize: "+predictedClustering.size)
    p /= predictedClustering.size.asInstanceOf[Double]
    r /= predictedClustering.size.asInstanceOf[Double]
    val f1 = 2 * p * r / (p + r)
    println("BCub F1: p="+p+" r="+r+" f1="+f1)
  }
}