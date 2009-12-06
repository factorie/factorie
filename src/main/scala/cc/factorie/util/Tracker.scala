/* Copyright (C) 2008-2009 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0.
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.util
import scala.collection.mutable.{ArrayBuffer,LinkedHashMap}

/** Track and time bounded regions of code. 
    @author Sebastian Riedel
*/
trait Tracker {
  def marked(text: String, start: Boolean)
}

class TimingCollector extends Tracker {
  private val starts = new ArrayBuffer[Long];
  private val durations = new LinkedHashMap[String, Long];
  private val counts = new LinkedHashMap[String, Int];
  private val texts = new ArrayBuffer[String];
                                      
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
  def |**(text: String) = mark(text, true);
  def **|(text: String) = mark(text, false);
  def **| = mark("End of Event", false);
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
