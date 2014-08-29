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
    }

    moves += new NoMove[Vars]
  }
}