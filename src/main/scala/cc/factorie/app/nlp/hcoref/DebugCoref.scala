/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */
package cc.factorie.app.nlp.hcoref

import cc.factorie.infer.Proposal

/**
 * @author John Sullivan
 */
trait DebugCoref[Vars <: NodeVariables[Vars]]{
  this: CorefSampler[Vars] with PairGenerator[Vars] with MoveGenerator[Vars]=>

  var printEvery:Int = 10000

  var acceptedProps = 0.0
  var acceptedThisRound = 0.0
  var totalProps = 0
  lazy val begin = System.currentTimeMillis()
  var startTime = 0L
  var stopTime = 0L

  beforeInferHooks += { _ =>
    startTime = begin
  }

  proposalHooks += {p:Proposal[(Node[Vars], Node[Vars])] =>
    totalProps +=1
    if(p.diff.size != 0) {
      acceptedProps += 1
      acceptedThisRound += 1
    }
    if(totalProps % printEvery == 0) {
      stopTime = System.currentTimeMillis()
      val elapsedSecs = (stopTime - startTime) / 1000.0
      val elapsedFromBegin = (stopTime - begin) / 1000.0
      val percentAccepted = (acceptedProps / totalProps)*100
      val percentAcceptedThisRound = (acceptedThisRound / printEvery)*100
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
      println(f"After $totalProps%d proposals $percentAccepted%.2f%% ($percentAcceptedThisRound%.2f%% this round) accepted in $elapsedFromBegin%.3f secs ($totalPropsPerSec%.2f proposals/sec). This round of $printEvery%d took $elapsedSecs%.3f secs ($propsPerSec%.2f proposals/sec)")
      println(f"\t max depth: $maxDepth min depth: $minDepth ave depth: $aveDepth%.2f")
      println(f"\t max children: $maxChildren min children: $minChildren ave children: $aveChildren%.2f")
      println(f"\t max mentions: $maxMentions min mentions: $minMentions ave mentions: $aveMentions%.2f")
      //println("%d non mention samples".format(multiSamples))
      startTime = stopTime
      acceptedThisRound = 0.0
    }
  }
}
