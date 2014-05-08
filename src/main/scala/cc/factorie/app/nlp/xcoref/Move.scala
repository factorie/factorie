package cc.factorie.app.nlp.xcoref

import cc.factorie.variable.{SettingIterator, DiffList}
import cc.factorie.infer.SettingsSampler

/**
 * User: escher, John Sullivan, akobren
 * Date: 10/28/13
 *
 */
trait MoveSettingIterator[Vars <: NodeVariables[Vars]] extends SettingIterator{
  def moves:IndexedSeq[Move[Vars]]

  var i = 0

  def hasNext = i < moves.size
  def next(diff:DiffList) = {val d = newDiffList; moves(i).perform(d); i += 1; d}
  def reset = i = 0
}


trait MoveGenerator[Vars <: NodeVariables[Vars]]  {

  this: SettingsSampler[(Node[Vars], Node[Vars])] =>

  def newInstance(implicit d:DiffList):Node[Vars]
}

trait Move[Vars <: NodeVariables[Vars]] {

  def name: String

  def perform(d:DiffList):Unit

  def isSymmetric(node1:Node[Vars], node2:Node[Vars]): Boolean // is the move symmetric for this pair of nodes?

  def isValid(node1: Node[Vars], node2:Node[Vars]): Boolean
  def operation(node1: Node[Vars], node2:Node[Vars])(d:DiffList): DiffList
  final def apply(node1:Node[Vars], node2:Node[Vars])(d:DiffList):DiffList = Option(d) match {
    case Some(diff) => operation(node1, node2)(diff)
    case None => operation(node1, node2)(new DiffList)
  }
}
/*
trait DefaultMoveGenerator[Vars <: NodeVariables[Vars]] extends MoveGenerator[Vars] {
  this: SettingsSampler[(Node[Vars], Node[Vars])] =>
  val moves = IndexedSeq(new NoMove[Vars], new MergeLeft[Vars], new SplitRight[Vars], new MergeUp[Vars]({d:DiffList => this.newInstance(d)}))

  @inline
  protected def expandedContext(context: (Node[Vars], Node[Vars])): Iterable[(Node[Vars], Node[Vars])] = List(context)
}

trait RootCheckingMoveGenerator[Vars <: NodeVariables[Vars]] extends MoveGenerator[Vars] {
  this: SettingsSampler[(Node[Vars], Node[Vars])] =>
  override protected def expandedContext(context: (Node[Vars], Node[Vars])): Iterable[(Node[Vars], Node[Vars])] = {
    val (n1, n2) = context
    val r1 = n1.root; val r2 = n2.root
    val l = new mutable.ArrayBuffer[(Node[Vars], Node[Vars])](3)
    if(r1 != n1) l += r1 -> n2
    if(r2 != n2) l += r2 -> n1
    l += context
    l
  }
}

trait ParentCheckingMovingGenerator[Vars <: NodeVariables[Vars]] extends MoveGenerator[Vars] {
  this: SettingsSampler[(Node[Vars], Node[Vars])] =>

  override protected def expandedContext(context: (Node[Vars], Node[Vars])): Iterable[(Node[Vars], Node[Vars])] = {
    val (n1, n2) = context
    List(n1 -> n2, n1.root -> n2.root) ++ n1.lineage.map(_ -> n2) //++ n2.lineage.map(_.asInstanceOf[N] -> n1.asInstanceOf[N])
  }
}
*/
class NoMove[Vars <: NodeVariables[Vars]] extends Move[Vars] {
  def name = "No Move"

  def perform(d:DiffList) = Unit

  def isSymmetric(node1: Node[Vars], node2: Node[Vars]): Boolean = true

  def isValid(node1: Node[Vars], node2: Node[Vars]): Boolean = true
  def operation(node1: Node[Vars], node2: Node[Vars])(d:DiffList) = {
    d
  }
}

class MergeLeft[Vars <: NodeVariables[Vars]](val left:Node[Vars], val right:Node[Vars]) extends Move[Vars] {

  def this() = this(null.asInstanceOf[Node[Vars]], null.asInstanceOf[Node[Vars]])

  def perform(d:DiffList) {
    operation(right, left)(d)
  }

  def name = "Merge Left"
  def isValid(right: Node[Vars], left: Node[Vars]) = right.root != left.root && !left.isMention && left.mentionCountVar.value >= right.mentionCountVar.value
  def isSymmetric(node1: Node[Vars], node2: Node[Vars]): Boolean = false

  def operation(right: Node[Vars], left: Node[Vars])(d:DiffList) = {
    right.alterParent(Option(left))(d)
    d
  }
}

class SplitRight[Vars <: NodeVariables[Vars]](val left:Node[Vars], val right:Node[Vars]) extends Move[Vars] {

  def this() = this(null.asInstanceOf[Node[Vars]], null.asInstanceOf[Node[Vars]])

  def perform(d:DiffList) {
    operation(right, left)(d)
  }

  def name = "Split Right"
  def isValid(right: Node[Vars], left: Node[Vars]): Boolean = left.root == right.root && right.mentionCountVar.value >= left.mentionCountVar.value
  def isSymmetric(node1: Node[Vars], node2: Node[Vars]): Boolean = false

  def operation(right:Node[Vars], left: Node[Vars])(d:DiffList) = {
    right.alterParent(None)(d)
    d
  }
}

class MergeUp[Vars <: NodeVariables[Vars]](val left:Node[Vars], val right:Node[Vars])(newInstance:(DiffList => Node[Vars])) extends Move[Vars] {

  def this(newInstance:(DiffList => Node[Vars])) = this(null.asInstanceOf[Node[Vars]], null.asInstanceOf[Node[Vars]])(newInstance)

  def perform(d:DiffList) {
    operation(right, left)(d)
  }

  def name = "Merge Up"
  def isValid(right: Node[Vars], left: Node[Vars]): Boolean = left.root != right.root && (left.isRoot && right.isRoot) && (left.isMention && right.isMention)
  def isSymmetric(node1: Node[Vars], node2: Node[Vars]): Boolean = true

  def operation(right: Node[Vars], left: Node[Vars])(d:DiffList) = {
    val newParent = newInstance(d)
    right.alterParent(Some(newParent))(d)
    left.alterParent(Some(newParent))(d)
    d
  }
}
