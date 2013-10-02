package cc.factorie.app.bib

import collection.mutable.HashMap
import cc.factorie.app.nlp.hcoref.{BagOfTruths, SparseBagOfWords, ChildParentTemplateWithStatistics, HierEntity}

object Evaluator{
  def debugBagFeature(childBow:SparseBagOfWords,parentBow:SparseBagOfWords, strength:Double=4.0, shift:Double=0.0):Double= {
    val result = childBow.cosineSimilarity(parentBow,childBow)
    (result+shift)*strength
  }
  def printPairwiseFalseNegatives(predicted:Seq[HierEntity],rep:HierEntity=>String=(e:HierEntity)=>e.toString):Unit ={
    println("===Printing False Negatives===")
    val labeled = predicted.filter(_.groundTruth != None).toSeq
    for(i<-0 until  labeled.size){
      for(j<-i+1 until labeled.size){
        val mi = labeled(i)
        val mj = labeled(j)
        if(mi.groundTruth.get == mj.groundTruth.get && !mi.entityRoot.eq(mj.entityRoot)){
          println("False negative pair")
          println("  M1: "+rep(mi).replaceAll("\n","\n  M1: "))
          println("  M2: "+rep(mj).replaceAll("\n","\n  M2: "))
        }
      }
    }
  }
  def printPairwiseFalsePositives(predicted:Seq[HierEntity],rep:HierEntity=>String=(e:HierEntity)=>e.toString):Unit ={
    println("===Printing False Positives===")
    val labeled = predicted.filter(_.groundTruth != None).toSeq
    for(i<-0 until  labeled.size){
      for(j<-i+1 until labeled.size){
        val mi = labeled(i)
        val mj = labeled(j)
        if(mi.groundTruth.get != mj.groundTruth.get && mi.entityRoot.eq(mj.entityRoot)){
          println("False positive pair")
          println("  M1: "+rep(mi).replaceAll("\n","\n  M1: "))
          println("  M2: "+rep(mj).replaceAll("\n","\n  M2: "))
          println("  Overlaps:")
          println("    keyw: "+mi.attr[BagOfKeywords].map(_._1).toSet.intersect(mj.attr[BagOfKeywords].map(_._1).toSet))
          println("    coau "+mi.attr[BagOfCoAuthors].map(_._1).toSet.intersect(mj.attr[BagOfCoAuthors].map(_._1).toSet))
          println("    venu: "+mi.attr[BagOfVenues].map(_._1).toSet.intersect(mj.attr[BagOfVenues].map(_._1).toSet))

          /*
          var bok = debugBagFeature(mi.attr[BagOfKeywords].value,mj.attr[BagOfKeywords].value,4.0, 0.0)
          var boc = debugBagFeature(mi.attr[BagOfCoAuthors].value,mj.attr[BagOfCoAuthors].value,4.0, 0.0)
          var bov = debugBagFeature(mi.attr[BagOfVenues].value,mj.attr[BagOfVenues].value,4.0, 0.0)
          println("  Bags with eachother")
          println("    -BagOfKeywords : "+bok)
          println("    -BagOfCoAuthors: "+boc)
          println("    -BagOfVenues   : "+bov)
          println("    -TEST: "+test)
          println("    *TOTAL: "+(bok+boc+bov))
          */
          if(mi.parentEntity!=null){
            println("  P1: "+rep(mi.parentEntity.asInstanceOf[HierEntity]).replaceAll("\n","\n  P1: "))
            val bok = debugBagFeature(mi.attr[BagOfKeywords].value,mi.parentEntity.attr[BagOfKeywords].value,4.0, 0.0) + debugBagFeature(mj.attr[BagOfKeywords].value,mi.parentEntity.attr[BagOfKeywords].value,4.0, 0.0)
            val boc = debugBagFeature(mi.attr[BagOfCoAuthors].value,mi.parentEntity.attr[BagOfCoAuthors].value,4.0, 0.0) + debugBagFeature(mj.attr[BagOfCoAuthors].value,mi.parentEntity.attr[BagOfCoAuthors].value,4.0, 0.0)
            val bov = debugBagFeature(mi.attr[BagOfVenues].value,mi.parentEntity.attr[BagOfVenues].value,4.0, 0.0) + debugBagFeature(mj.attr[BagOfVenues].value,mi.parentEntity.attr[BagOfVenues].value,4.0, 0.0)
            println("  Bags with parent")
            println("    -BagOfKeywords : "+bok)
            println("    -BagOfCoAuthors: "+boc)
            println("    -BagOfVenues   : "+bov)
            println("    *TOTAL: "+(bok+boc+bov))
            println("      parent eq? "+ mi.parentEntity.eq(mj.parentEntity))
          }
        }
      }
    }
  }
  def eval(predicted:Seq[HierEntity]):Unit ={
    printPairF1(predicted)
    bcubedF1(predicted.filter(_.groundTruth != None))
  }
  def pairF1(predicted:Seq[HierEntity]):Seq[Double] = {
    val labeled = predicted.filter(_.groundTruth != None)
    pairF1LabeledOnly(labeled)
  }
  def printPairF1(predicted:Seq[HierEntity]):Seq[Double] = {
    val labeled = predicted.filter(_.groundTruth != None)
    val result = pairF1LabeledOnly(labeled)
    println("Pair F1: p="+result(1)+" r="+result(2)+" f1="+result(0))
    result
  }
  def pairF1LabeledOnly(labeled:Seq[HierEntity]):Seq[Double] = pairF1(labeled.map(_.entityRoot.id.toString),labeled.map(_.groundTruth.get))
  def pairF1(predictedClusters:Seq[String],trueClusters:Seq[String]):Seq[Double] ={
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
    //println("Num labeled: "+predictedClusters.size+" tp: "+tp+" fp: "+fp+" fn: "+fn)
    val p = tp/(tp+fp)
    val r = tp/(tp+fn)
    val f1 = 2*p*r/(p+r)
    //if(printScores)println("Pair F1: p="+p+" r="+r+" f1="+f1)
    Seq(f1,p,r)
  }
  def bcubedF1(predictedClustering:Seq[HierEntity]):Unit ={
    println("predictedClustering.size: "+predictedClustering.size)
    var p: Double = 0.0
    var r: Double = 0.0
    for (i <- 0 until predictedClustering.size){
      val entity = predictedClustering(i)
      val predLabel = entity.entityRoot.id.toString//d.mentions(i).entity
      val truths = predictedClustering.filter((e:HierEntity)=>{e.groundTruth!=None && e.groundTruth.get==entity.groundTruth.get}).toSet//predictedClustering.getMentionsFromEID(d, d.mentions(i))
      val preds = entity.entityRoot.descendantsOfClass[HierEntity].filter((e:HierEntity)=>e.groundTruth!=None).toSet
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
  def analysisForEntityId(id:String,entities:Seq[HierEntity]):Unit ={
    var targetEntity:HierEntity = null
    for(e <- entities){
      if(e.id.toString==id){
        targetEntity=e
      }
    }
    if(targetEntity!=null)analysisForEntity(targetEntity)
  }
  def analysisForEntity(e:HierEntity):Unit ={
    val top = e.entityRoot.asInstanceOf[HierEntity]
    val truthIds = new HashMap[String,Int]
    println("ID: "+e.id+"  TOP ID: "+top.id)
    for(mention <- top.descendantsOfClass[HierEntity].filter(_.isObserved)){
      if(mention.groundTruth != None)truthIds(mention.groundTruth.get) = truthIds.getOrElse(mention.groundTruth.get,0) + 1
      else truthIds("unlabeled") = truthIds.getOrElse("unlabeled",0) + 1
    }
    println("Label composition:")
    for((k,v) <- truthIds)
      println("   "+v+" "+k)
  }
  def analysisForTruthKey(truthKey:String,entities:Seq[HierEntity]):Unit ={
    val relevant = entities.filter((e:HierEntity) => e.groundTruth!=None && e.groundTruth.get == truthKey).map((e:HierEntity) => e.entityRoot).toSet.toSeq

  }

}