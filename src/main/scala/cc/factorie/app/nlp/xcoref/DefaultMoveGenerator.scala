package cc.factorie.app.nlp.xcoref

import cc.factorie.infer.SettingsSampler

/**
 * @author John Sullivan
 */
trait DefaultMoveGenerator[Vars <: NodeVariables[Vars]]  extends MoveGenerator[Vars]{
  this :SettingsSampler[(Node[Vars], Node[Vars])] =>

  def settings(c:(Node[Vars], Node[Vars])) = new MoveSettingIterator[Vars] {
    val (e1, e2) = c

    val moves = new scala.collection.mutable.ArrayBuffer[Move[Vars]]()

    if(e1.root != e2.root) {
      if(e1.isMention && e1.isRoot && e2.isMention && e2.isRoot) {
        moves += new MergeUp[Vars](e1, e2)({d => newInstance(d)})
      } else {
        if(e1.mentionCountVar.value > e2.mentionCountVar.value) {
          moves += new MergeLeft[Vars](e1, e2)
        } else {
          moves += new MergeLeft[Vars](e2, e1)
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
