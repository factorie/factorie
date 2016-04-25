/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
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



package cc.factorie.util
import scala.collection.mutable.{ArrayBuffer, LinkedHashMap}

/** Track and time bounded regions of code. 
    @author Sebastian Riedel
*/
trait Tracker {
  def marked(text: String, start: Boolean)
}

class TimingCollector extends Tracker {
  private val starts = new ArrayBuffer[Long]
  private val durations = new LinkedHashMap[String, Long]
  private val counts = new LinkedHashMap[String, Int]
  private val texts = new ArrayBuffer[String]
                                      
  def marked(text: String, start: Boolean) = {
    if (start) {
      starts += System.currentTimeMillis
      texts += text
    } else {
      val duration = System.currentTimeMillis - starts.remove(starts.size - 1)
      val text = texts.remove(texts.size - 1)
      durations(text) = durations.getOrElse(text, 0l) + duration
      counts(text) = counts.getOrElse(text, 0) + 1
    }
  }

  def timings = durations.keySet.map(text =>
    text +
    "\n\tTotal Time: " + durations(text) +
    "\n\tTotal Calls: " + counts(text) +
    "\n\tAvg. Time: " + durations(text).toDouble /
    counts(text)).mkString("\n")
  
}

trait Trackable {
  def mark(text: String, start: Boolean) = {for (t <- Trackers) t.marked(text, start)}
  def |**(text: String) = mark(text, start = true)
  def **|(text: String) = mark(text, start = false)
  def **|() = mark("End of Event", start = false)
}

object Trackers extends ArrayBuffer[Tracker]

/* To use:
val timer = new TimingCollector()
Trackers += timer
...
println(timer.timings)

 * 
 * Then in code:
 * 

class SumProductBeliefPropagation extends MarginalInference with Trackable {
...
private def updateOutgoingMessages = {

       |**("Term marginalization")
       val incomingBeliefs = new MutableBeliefs[Any,EnvVar[Any]]
       for (edge <- edges)
incomingBeliefs.setBelief(edge.node.variable, edge.node2factor)
       val outgoingBeliefs = term.marginalize(incomingBeliefs)
       **|

       |**("Divide by incoming message and normalize")
       for (edge <- edges) edge.factor2node =
(outgoingBeliefs.belief(edge.node.variable) /
edge.node2factor).normalize
       **|
     }
...

*/
