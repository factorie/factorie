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

import cc.factorie.infer.SettingsSampler

/**
 * @author John Sullivan
 */
trait DefaultMoveGenerator[Vars <: NodeVariables[Vars]]  extends MoveGenerator[Vars]{
  this :SettingsSampler[(Node[Vars], Node[Vars])] =>

  def settings(c:(Node[Vars], Node[Vars])) = new MoveSettingIterator[Vars] {
    var (e1, e2) = c

    val moves = new scala.collection.mutable.ArrayBuffer[Move[Vars]]()

    if(e1.root != e2.root) {
      if(e1.isMention && e1.isRoot && e2.isMention && e2.isRoot) {
        moves += new MergeUp[Vars](e1, e2)({d => newInstance(d)})
      } else {
        while (e1 != null) {
          if(e1.mentionCountVar.value >= e2.mentionCountVar.value) {
            moves += new MergeLeft[Vars](e1, e2)
          } else {
            moves += new MergeLeft[Vars](e2, e1)
          }
          e1 = e1.parent.getOrElse(null.asInstanceOf[Node[Vars]])
        }
      }
    } else {
      if(e1.mentionCountVar.value > e2.mentionCountVar.value) {
        moves += new SplitRight[Vars](e2, e1)
      } else {
        moves += new SplitRight[Vars](e1, e2)
      }
    }

    moves += new NoMove[Vars]
  }
}
