package cc.factorie.app.nlp.hcoref

import cc.factorie.infer.SettingsSampler

/**
 * @author John Sullivan
 */
trait NoSplitMoveGenerator[Vars <: NodeVariables[Vars]] extends MoveGenerator[Vars]{
  this :SettingsSampler[(Node[Vars], Node[Vars])] =>

  def settings(c:(Node[Vars], Node[Vars])) = new MoveSettingIterator[Vars] {

    var (e1, e2) = c

    val moves = new scala.collection.mutable.ArrayBuffer[Move[Vars]]()

    if(e1.root != e2.root) {
      if(e1.isMention && e1.isRoot && e2.isMention && e2.isRoot) {
        moves += new MergeUp[Vars](e1, e2)({d => newInstance(d)})
      } else if(e1.isMention && e2.isMention) {
        if(e1.parent != null) {
          moves += new MergeLeft[Vars](e1.parent, e2)
        }
        if(e2.parent != null) {
          moves += new MergeLeft[Vars](e2.parent, e1)
        }
      } else {
        while (e1 != null) {
          if(e1.mentionCountVar.value >= e2.mentionCountVar.value && !e1.isMention) {
            moves += new MergeLeft[Vars](e1, e2)
          } else {
            if(e2.isMention) { // we should only be here if e2 has a parent
              moves += new MergeLeft[Vars](e2.parent, e1)
            } else {
              moves += new MergeLeft[Vars](e2, e1)
            }
          }
          e1 = e1.getParent.getOrElse(null.asInstanceOf[Node[Vars]])
        }
      }
    }

    moves += new NoMove[Vars]
  }
}