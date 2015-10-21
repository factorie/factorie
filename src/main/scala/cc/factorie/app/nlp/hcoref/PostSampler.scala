package cc.factorie.app.nlp.hcoref

import cc.factorie._

import scala.collection.mutable
import scala.util.Random

/**
 * @author johnsullivan
 *
 * Direct Scoring Model simply exposes the cosine distance scores of a coref model as a distance function on
 * nodes to aid [[cc.factorie.app.nlp.hcoref.PostSampler]]
 */
trait DirectScoringModel[Vars <: NodeVariables[Vars]] extends CorefModel[Vars] {
  def scoreDistance(a:Node[Vars], b:Node[Vars]) = {
    templates.collect{ case dist:ChildParentCosineDistance[Vars] =>
      dist.score(a -> b, a.variables, b.variables)
    }.sum
  }
}

/**
 * @author johnsullivan
 *
 * PostSampler provides a suite of methods to change the structure of the trees to work better as
 * hierarchical summaries rather than coreference hypotheses
 */
trait PostSampler[Vars <: NodeVariables[Vars], Model <: DirectScoringModel[Vars]] {
  this: CorefSampler[Vars] with MoveGenerator[Vars] with Logger =>

  implicit val random:Random

  def scoreDist(n1:Node[Vars], n2:Node[Vars]) = model.asInstanceOf[Model].scoreDistance(n1, n2)

  def postSample: Unit = {
    var branches = mentions.flatMap(_.getParent).toSet.toSeq //mentions.collect {case m if !m.isMention && m.getParent.isDefined => m.parent}.toSet.toSeq
    val orphans = mentions.filter(m => m.isMention && m.getParent.isEmpty)
    log("About to slot %d orphans into %d branches".format(orphans.size, branches.size))
    val threshold = (branches.size * orphans.size) / 10 //never look at more than 10% of the possible size

    var idx = 0
    val oIter = orphans.iterator
    while(oIter.hasNext) {
      var curIdx = 0
      var maxScore = Double.MinValue
      var maxIdx = -1
      var maxl1 = Double.MinValue
      var maxl2 = Double.MinValue
      val candidates = mutable.ArrayBuffer[Node[Vars]]()
      val orphan = oIter.next()
      val bIter = branches.iterator
      while(bIter.hasNext && (!(maxScore > 0.0 && maxl1 > 0.0 && maxl2 > 0.0) || !(candidates.length > threshold))) {
        val branch = bIter.next()
        val score = scoreDist(orphan, branch)
        candidates += branch

        if(score > maxScore) {
          maxIdx = curIdx
          maxl2 = maxl1
          maxl1 = maxScore
          maxScore = score
        } else if(score > maxl1) {
          maxl2 = maxl1
          maxl1 = score
        } else if(score > maxl2) {
          maxl2 = score
        }
        curIdx += 1
      }

      idx += 1
      if(candidates.nonEmpty) {
        orphan.alterParent(Some(candidates(maxIdx)))(null)
      }
      branches = branches.shuffle
    }
    if(mentions.count(m => m.isMention && m.getParent.isEmpty) > 0) {
      log("At this point we should have no more mentions but we have " + mentions.count(m => m.isMention && m.getParent.isEmpty))
    }
    log("done")

  }



  def retryMentions: Unit = {
    var orphans = mentions.filter(m => m.isMention && m.getParent.isEmpty).toSeq
    log("trying merger on %d mentions".format(orphans.size))
    val minScore = -1.5
    var scoreThresh = 0.0
    var remainingMents = orphans.size
    var remainingM1 = -1
    while (orphans.nonEmpty && !((scoreThresh == minScore) && remainingM1 == remainingMents)) {
      val threshold = orphans.size / 10
      val candidates =
        for(i <- orphans.indices;
            j <- i + 1 until threshold;
            n1 = orphans(i);
            n2 = orphans(j);
            score = scoreDist(n1, n2)
            if score > scoreThresh) yield {
          (n1, n2, score)
        }
      candidates.sortBy(-_._3).headOption match {
        case Some((n1, n2, _)) =>
          new MergeUp[Vars](n1,n2)({d:DiffList => newInstance(d)}).perform(null)
        case None =>
          scoreThresh = math.max(minScore, scoreThresh - 0.1)
      }

      remainingM1 = remainingMents
      orphans = mentions.filter(m => m.isMention && m.getParent.isEmpty).toSeq
      remainingMents = orphans.size
    }
    log("done trying to merge mentions with %d mentions left and a score threshold of %.2f".format(orphans.size, scoreThresh))
  }

  def getScoreMatrx(ns:Seq[Node[Vars]], threshold:Int = 10):Seq[(Node[Vars], Node[Vars], Double)] =
    (for(i <- ns.indices;
         j <- i + 1 until math.min(ns.size, threshold);
         n1 = ns(i);
         n2 = ns(j);
         s = scoreDist(n1, n2)) yield {(n1, n2, s)}).sortBy(_._3)

  def dropInRoots: Unit = {
    val roots = mentions.map(_.root).filterNot(_.isMention).toSeq
    log("dropping in %d roots".format(roots.size))
    val scoreMat = getScoreMatrx(roots, 50)
    val mergedRoots = mutable.HashSet[Node[Vars]]()
    var idx = roots.size - 1
    while(idx > roots.size - 50) {
      if(idx % 10 == 0) print(".")
      val (n1, n2, _) = scoreMat(idx)

      if((!mergedRoots.contains(n1) || !mergedRoots.contains(n2)) && !(n1 == n2)) {
        if(n1.children.size > n2.children.size && !mergedRoots.contains(n1)) {
          mergedRoots += n2
          new MergeLeft[Vars](n1, n2).perform(null)
        } else {
          mergedRoots += n1
          new MergeLeft[Vars](n2, n1).perform(null)
        }
      }
      idx -= 1
    }
    log("\nDone dropping roots, now we have %d roots".format(mentions.map(_.root).toSet.size))
  }

  def internalReshuffle: Unit = {
    val threshold = mentions.size / 10
    val roots = mentions.collect {case m if !m.root.isMention => m.root}.toSet.toSeq
    log("Started reshuffle")
    def processNode(node:Node[Vars]): Unit = {
      implicit val diff:DiffList = null
      if(node.children.nonEmpty && node.children.size >= threshold) {
        val children = node.children.toSeq
        val sub1 = newInstance(null)
        val sub2 = newInstance(null)
        sub1 alterParent Some(node)
        sub2 alterParent Some(node)

        val scores = getScoreMatrx(children).map{case(a,b,c) => (a,b) -> c}
        val ((n1, n2), _) = scores.head
        n1 alterParent Some(sub1)
        n2 alterParent Some(sub2)
        val relScoreMap = mutable.HashMap[Node[Vars], (Double, Double)]().withDefault(_ => (Double.NaN, Double.NaN))
        for(((c1, c2), sc) <- scores.tail
            if (c1 == n1 || c1 == n2 || c2 == n1 || c2 == n2) && !(n1==c1 && n2==c2)) { // if we have a pair that has one of n1/n2, but not both
          if(c1 == n1) {
            val (_, s2) = relScoreMap(c2)
            relScoreMap(c2) = sc -> s2
          } else if(c1 == n2) {
            val (s1, _) = relScoreMap(c2)
            relScoreMap(c2) = s1 -> sc
          } else if(c2 == n1) {
            val (_, s2) = relScoreMap(c1)
            relScoreMap(c1) = sc -> s2
          } else if(c2 == n2) {
            val (s1, _) = relScoreMap(c1)
            relScoreMap(c1) = s1 -> sc
          } else {
            throw new IllegalStateException("c1: %s c2: %s n1: %s n2: %s sc:%.2f".format(c1, c2, n1, n2, sc))
          }
        }
        relScoreMap.foreach { case(n, (s1, s2)) =>
          assert(!s1.isNaN)
          assert(!s2.isNaN)
          n alterParent Some(if (s1 > s2) sub1 else sub2)
        }

        log("\tafter reshuffle we have two nodes of size %d and %d".format(sub1.children.size, sub2.children.size))
        sub1.children foreach processNode
        sub2.children foreach processNode
      }
    }
    roots foreach processNode
    log("finished reshuffle")
  }

  def semiBIRCH: Unit = {
    val roots = mentions.roots.sortBy(- _.mentions.size)
    val numMents = roots.foldLeft(0)(_ + _.mentions.size)
    val cutoff = 0.7

    var mentCnt = 0
    val (bigRoots, smallRoots) = roots.foldLeft((Seq.empty[Node[Vars]], Seq.empty[Node[Vars]])) { case ((bigNodes, rest), r) =>
      if(mentCnt < (numMents * cutoff)) {
        mentCnt += r.mentions.size
        (bigNodes :+ r) -> rest
      } else {
        bigNodes -> (rest :+ r)
      }
    }
    def dropNode(candidates:Seq[Node[Vars]], node:Node[Vars]): Unit = {
      val (s, newParent) = (candidates.map(c => scoreDist(c, node) -> c) ++ node.getParent.map(p => scoreDist(p, node) -> p)).sortBy(-_._1).head
      log ("giving %s to %s as a child with score %.4f".format(node, newParent, s))
      if(newParent != node.parent) {
        node.alterParent(Some(newParent))(null)
        dropNode(newParent.children.filterNot(c => c.isMention || c == node).toSeq, node)
      } else {
        log("%s settled down with %s as a parent".format(node, node.getParent))
      }
    }
    smallRoots.foreach(dropNode(bigRoots, _))
  }

}
