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
package cc.factorie.app.nlp.hcoref

import cc.factorie.infer.SettingsSampler
import cc.factorie.variable.SettingIterator
import cc.factorie.{DiffList, Model, TemplateModel}

/**
 * @author John Sullivan
 */
trait DebuggableTemplate {
  protected var _debug: Boolean = false
  def debugOn() = _debug = true
  def debugOff() = _debug = false
  def name: String

  /** methods implementing this trait need to call report manually during the scoring process
    * to print out debug results */
  def report(score:Double, weight:Double) {
    if(_debug) {
      println("\t%.4f = %.4f * %.4f (score * weight)  [%s]".format(score * weight, score, weight, name))
    }
  }
}

class DebugDiffList extends DiffList {
  override def scoreAndUndo(model:Model): Double = {
    println("scoring and undoing")
    model.asInstanceOf[TemplateModel].families.collect{case t:DebuggableTemplate => t.debugOn()}

    if (this.length == 0) return 0.0  // short-cut the simple case
    println("=====DEBUGGING MODEL SCORE=====")
    println("----NEXT WORLD----")
    var s = model.currentScore(this)
    println("  next: "+ s)
    //log(Log.DEBUG)("DiffList scoreAndUndo  pre-undo score=" + s)
    this.undo()
    // We need to re-calculate the Factors list because the structure may have changed
    println("----CURRENT WORLD----")
    val s2 = model.currentScore(this)
    println("  current: "+s2)
    s -= s2
    println("TOTAL SCORE: "+s)
    model.asInstanceOf[TemplateModel].families.collect{case t:DebuggableTemplate => t.debugOff()}
    s
  }}

object DebugModel {
  def debugOn(model:Model) {
    model.asInstanceOf[TemplateModel].templates.foreach {
      case debuggable:DebuggableTemplate => debuggable.debugOn()
      case _ => ()
    }
  }

  def debugOff(model:Model) {
    model.asInstanceOf[TemplateModel].templates.foreach{
      case debuggable:DebuggableTemplate => debuggable.debugOff()
      case _ => ()
    }
  }
}

trait DebugSettingIterator extends SettingIterator {
  override def newDiffList = new DebugDiffList
}

trait DebugDiffListMoveGenerator[Vars <: NodeVariables[Vars]] extends MoveGenerator[Vars]{
  this :SettingsSampler[(Node[Vars], Node[Vars])] =>

  DebugModel.debugOn(model)

  def settings(c:(Node[Vars], Node[Vars])) = new DebugSettingIterator with MoveSettingIterator[Vars] {

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
          e1 = e1.getParent.getOrElse(null.asInstanceOf[Node[Vars]])
        }
      }
    }

    moves += new NoMove[Vars]
  }
}
