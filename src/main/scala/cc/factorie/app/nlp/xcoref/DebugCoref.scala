package cc.factorie.app.nlp.xcoref

import cc.factorie.infer.Proposal

/**
 * @author John Sullivan
 */
trait DebugCoref[Vars <: NodeVariables[Vars]]{
  this: CorefSampler[Vars] with PairContextGenerator[Vars] with MoveGenerator[Vars]=>

  var printEvery:Int = 10000

  var acceptedProps = 0.0
  var totalProps = 0
  lazy val begin = System.currentTimeMillis()
  var startTime = 0L
  var stopTime = 0L

  beforeInferHooks += { _ =>
    startTime = begin
  }

  proposalHooks += {p:Proposal[(Node[Vars], Node[Vars])] =>
    totalProps +=1
    if(p.diff.size != 0) acceptedProps += 1
    if(totalProps % printEvery == 0) {
      stopTime = System.currentTimeMillis()
      val elapsedSecs = (stopTime - startTime) / 1000.0
      val elapsedFromBegin = (stopTime - begin) / 1000.0
      val percentAccepted = (acceptedProps / totalProps)*100
      val propsPerSec = printEvery.toDouble / elapsedSecs
      val totalPropsPerSec = totalProps.toDouble / elapsedFromBegin
      val depths = mentions.map(_.depth)
      val maxDepth = depths.max
      val minDepth = depths.min
      val aveDepth = depths.sum.toDouble / depths.size
      val roots = mentions.map(_.root).toSet
      val rootChildrens = roots.map(_.children.size)
      val rootMentions = roots.map(_.mentionCountVar.value)
      val maxChildren = rootChildrens.max
      val minChildren = rootChildrens.min
      val aveChildren = rootChildrens.sum.toDouble / rootChildrens.size
      val maxMentions = rootMentions.max
      val minMentions = rootMentions.min
      val aveMentions = rootMentions.sum.toDouble / rootMentions.size
      println(f"After $totalProps%d proposals $percentAccepted%.2f%% accepted in $elapsedFromBegin%.3f secs ($totalPropsPerSec%.2f proposals/sec). This round of $printEvery%d took $elapsedSecs%.3f secs ($propsPerSec%.2f proposals/sec)")
      println(f"\t max depth: $maxDepth min depth: $minDepth ave depth: $aveDepth%.2f")
      println(f"\t max children: $maxChildren min children: $minChildren ave children: $aveChildren%.2f")
      println(f"\t max mentions: $maxMentions min mentions: $minMentions ave mentions: $aveMentions%.2f")
      startTime = stopTime
    }
  }
}
