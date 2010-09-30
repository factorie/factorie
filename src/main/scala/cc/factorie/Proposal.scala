/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie

/** For storing one of the proposals considered.
    "diff" is the list of changes represented by this Proposal.
    "modelScore" is the difference in score after-before, according to the model.
    "objectiveScore" is the difference in score after-before, according to the objective function.
    "acceptanceScore" is the score that will be used to accept/rank/select among multiple proposals.  It is typically the modelScore multiplied by a temperature.
    Note that objectiveScore may not be truly set, in which case it will have value Double.NaN. */
class Proposal(val diff:DiffList, val modelScore:Double, val objectiveScore:Double,  val acceptanceScore:Double) {
  override def toString = "Proposal("+diff+", "+modelScore+", "+objectiveScore+", "+acceptanceScore+")"
}

// TODO The following trait is currently unused.  Remove it?
/** An object (typically a variable or a world) that can propose changes to itself, 
    and possibly also other variables through variable value coordination. */
@deprecated("May be removed in a future version of the library.")
trait Proposer {
  /** Make a random proposal.  Return Metropolis-Hastings' log(q(old|new)/q(new|old)) */
  def propose(model:Model, d:DiffList): Double
}
