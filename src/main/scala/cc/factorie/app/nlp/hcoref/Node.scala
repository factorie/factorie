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

import cc.factorie.Cubbie
import cc.factorie.app.nlp.coref.{CrossDocEntity, CrossDocMention}
import cc.factorie.util.Hooks1
import cc.factorie.variable._

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * @author harshal, Jack Sullivan
 * @date: 10/3/13
 */

/** The node class is a generic class representing the nodes of a tree
  * designed to be used with the hierarchical coref inference procedure
  *
  * The idea behind this procedure is that we build up a tree of nodes by
  * randomly sampling from the nodes in the tree and proposing to move them
  * to another part of the tree (or as a part of a new tree).  The decision
  * of whether to accept or reject a proposal is defined by the sampler; users
  * have the ability to attach arbitrary NodeVariables to a node; these NodeVariables
  * represent attributes on the nodes; after a move, the nodes may propogate their
  * attributes to their parents after being moved (user specified)
  */
@SerialVersionUID(1l)
class Node[Vars <: NodeVariables[Vars]](val variables:Vars, val uniqueId: String)(implicit d: DiffList) extends CrossDocEntity with Serializable {


  override def hashCode = Option(uniqueId).map(_.hashCode).getOrElse(super.hashCode)
  override def equals(a:Any) = a match {
    case other:Node[Vars] if Option(this.uniqueId).isDefined && Option(other.uniqueId).isDefined && this.variables.getClass == other.variables.getClass => other.uniqueId == this.uniqueId
    case otw => super.equals(a)
  }


  def mentions = leaves

  type ParentType = Node[Vars]

  def this(variables:Vars)(implicit d:DiffList) = this(variables, java.util.UUID.randomUUID.toString)(d)

  variables.node = this

  class UnrepresentedChildren(initVal:Int=0) extends IntegerVariable(0) {
    val node = Node.this
    if(initVal > 0) this.set(initVal)(d)
    def inc(implicit diff:DiffList) = this.set(this.value + 1)(diff)
    def dec(implicit diff:DiffList) = this.set(this.value - 1)(diff)
  }

  class MentionCount(initVal:Int=0) extends IntegerVariable(0) {
    val node = Node.this
    if(initVal > 0) this.set(initVal)(d)
  }
  class Mentions extends SetVariable[Mention[Vars]]{
    val node = Node.this
  }
  class Children extends SetVariable[Node[Vars]]{
    val node = Node.this
  }
  class IsRoot(initVal:Boolean = parentRef.dst == null)(implicit d:DiffList) extends BooleanVariable(false) {
    val node = Node.this
    if(initVal) this.set(initVal)(d)
  }
  class IsMention(initVal:Boolean) extends BooleanVariable(initVal){
    val node = Node.this
  }
  class Exists(initVal:Boolean) extends BooleanVariable(false) {
    val node = Node.this
    if(initVal) this.set(initVal)(d)
  }

  private val parentRef = new ArrowVariable[Node[Vars], Node[Vars]](this, null)
  val childrenVar:Children = new Children
  val mentionsVar:Mentions = new Mentions
  val mentionCountVar:MentionCount = new MentionCount
  val isRootVar:IsRoot = new IsRoot
  val isMentionVar:IsMention = new IsMention(false)
  val existsVar:Exists = new Exists(true)


  def children = childrenVar.value
  def isMention = isMentionVar.booleanValue
  def isRoot = isRootVar.booleanValue
  def exists = existsVar.booleanValue

  def descendents:Iterable[Node[Vars]] = {
    this.children ++ this.children.collect {
      case n if n.children.nonEmpty => n.descendents
      case n if n.children.isEmpty => Nil
    }.flatten
  }

  def parent = getParent.getOrElse(null.asInstanceOf[Node[Vars]])
  final def getParent:Option[Node[Vars]] = Option(parentRef.dst)
  def leaves:Iterable[Mention[Vars]] = mentionsVar.value

  @tailrec
  final def root:Node[Vars] = getParent match {
    case Some(p) => p.root
    case None => this
  }
  @tailrec
  final def  ancestor(numGenerationsBack:Int):Node[Vars] = {
    if(numGenerationsBack == 0){
      this
    } else if(numGenerationsBack > this.depth -1) {
      throw new IllegalArgumentException("Cannot go back deeper than depth (%d) received: %d".format(this.depth -1, numGenerationsBack))
    } else {
      this.getParent.get.ancestor(numGenerationsBack -1)
    }
  }


  final def lineage:Iterable[Node[Vars]] = {
    def lineageHelper(pOpt:Option[Node[Vars]], parents:List[Node[Vars]]):List[Node[Vars]] = {
      pOpt match {
        case Some(p) => lineageHelper(p.getParent, p :: parents)
        case None => parents
      }
    }
    lineageHelper(Some(this),Nil)
  }

  def size:Int = 1 + children.map(_.size).sum //todo Make Faster!

  final def depth:Int = {
    @tailrec
    def helper(pOpt:Option[Node[Vars]], cDepth:Int):Int = {
      pOpt match {
        case Some(p) => helper(p.getParent, cDepth + 1)
        case None => cDepth
      }
    }
    helper(getParent,1)
  }

  def markForDeletion {
    parentRef.set(null)(null)
    deleteHooks(this)
  }

  val deleteHooks = new Hooks1[Node[Vars]]


  val loadedFromDb = false
  protected val deletionRecord:mutable.HashSet[String] = null

  protected def deleteHook[N <: Node[_]](node: N) {
    if(node.loadedFromDb) {
      deletionRecord add node.uniqueId
    }
  }
  deleteHooks.append(deleteHook)

  @tailrec
  private def propagateAddition(addedVariables:Vars)(implicit d:DiffList):Unit = this.getParent match {
    case Some(p) => {
      p.variables ++= addedVariables
      p.propagateAddition(addedVariables)(d)
    }
    case None => Unit
  }

  @tailrec
  private def propagateRemoval(removedVariables:Vars)(implicit d:DiffList):Unit = this.getParent match {
    case Some(p) => {
      p.variables --= removedVariables
      p.propagateRemoval(removedVariables)(d)
    }
    case None => Unit
  }

  final def alterParent(newParent:Option[Node[Vars]])(implicit d :DiffList) {
    val oldParent = this.getParent
    oldParent match {
      case Some(oParent) => {
        propagateRemoval(this.variables)(d)
        oParent.childrenVar.remove(this)(d)
        Node.propagateUpdateMentionCount(-this.mentionCountVar.value, oldParent)(d)
        Node.propagateRemoveMentions(this.mentionsVar.value, oldParent)(d)
      // TODO AK: With the current moves, a move cannot make an old parent into a root, but if there ever were, we'd need a way to add that change to the DiffList
      }
      case None => Unit
    }
    newParent match {
      case Some(ment) if ment.isMention =>
        throw new IllegalStateException("We should never be making a mention a parent, but we tried to make %s %s's parent".format(ment, this))
      case Some(nParent) => {
        parentRef.set(nParent)(d)
        propagateAddition(this.variables)(d)
        if(this.isRoot) {
          isRootVar.set(false)(d)
        }
        nParent.childrenVar.add(this)(d)
        Node.propagateUpdateMentionCount(this.mentionCountVar.value, newParent)(d)
        Node.propagateAddMentions(this.mentionsVar.value, newParent)(d)
      }
      case None => {
        parentRef.set(null)(d)
        isRootVar.set(true)(d)
        if(this.childrenVar.value.size <= 1 && !isMention) {// if we no longer have a parent, we aren't a mention, and we have one or fewer children, we no longer exist.
          existsVar.set(false)(d)
        }
      }
    }
    if(oldParent.isDefined && oldParent.get.leaves.size == 0) { // This make the old parent garbage collectible
      oldParent.get.alterParent(None)(d)
    }
    if(oldParent.isDefined && oldParent.get.children.size == 1) {
      oldParent.get.children.head.alterParent(oldParent.get.getParent)(d)
    }
  }

  override def toString = s"Node($uniqueId, $variables)"
}

object Node {
  @tailrec
  protected def propagateRemoveMentions[Vars <: NodeVariables[Vars]](mentionVar: Node[Vars]#Mentions#Value, parentRef:Option[Node[Vars]])(implicit d:DiffList):Unit = parentRef match {
    case Some(p) => {
      p.mentionsVar.removeAll(mentionVar)(d)
      propagateRemoveMentions(mentionVar,p.getParent)(d)
    }
    case None => Unit
  }

  @tailrec
  protected def propagateAddMentions[Vars <: NodeVariables[Vars]](mentionVar: Node[Vars]#Mentions#Value, parentRef:Option[Node[Vars]])(implicit d:DiffList):Unit = parentRef match {
    case Some(p) => {
      p.mentionsVar.addAll(mentionVar)(d)
      propagateAddMentions(mentionVar,p.getParent)(d)
    }
    case None => Unit
  }
  @tailrec
  protected def propagateUpdateMentionCount[Vars <: NodeVariables[Vars]](update: Int, parentRef:Option[Node[Vars]])(implicit d:DiffList):Unit = parentRef match {
    case Some(p) => {
      p.mentionCountVar.set(p.mentionCountVar.value + update)(d)
      propagateUpdateMentionCount(update,p.getParent)(d)
    }
    case None => Unit
  }
}


@SerialVersionUID(1l)
class Mention[Vars <: NodeVariables[Vars]](v:Vars, id: String, var withinDocEntityId:String = null)(implicit d:DiffList) extends Node[Vars](v, id)(d) with CrossDocMention {

  def entity = root
  def string = this.toString

  def mark:Unit = ()

  def this(variables:Vars)(implicit d:DiffList) = this(variables, java.util.UUID.randomUUID.toString)(d)
  override final val size = 1
  override final val leaves = List(this)
  override final val isMentionVar = new IsMention(true)
  override final val mentionCountVar = new MentionCount(1)
  mentionsVar += this

  override def toString = s"Mention($uniqueId, $variables)"
}

@SerialVersionUID(1l)
trait NodeVariables[Self <: NodeVariables[Self]] extends SelfVariable[Self] with Serializable {

  this: Self =>

  var node:Node[Self] = null

  def ++(other:Self)(implicit d:DiffList):Self
  def --(other:Self)(implicit d:DiffList):Self
  def ++=(other:Self)(implicit d:DiffList):Unit
  def --=(other:Self)(implicit d:DiffList):Unit

  def getVariables: Seq[Var]
  def size:Int = getVariables.size
  override def toString:String = "%s(%s)".format(this.getClass.getSimpleName, getVariables.map(_.toString).mkString(", "))
}

trait NodeCubbie[Vars <: NodeVariables[Vars]] extends Cubbie {

  type N = Node[Vars]
  val deletionSet:mutable.HashSet[String]

  val parentRef = RefSlot("parentRef", () => newNodeCubbie)
  val isMention = BooleanSlot("isMention")
  val canopies = StringListSlot("canopies")
  val source = StringSlot("src")

  def newNode(v: Vars, id:String)    = new Node(v,id)(null) {
    override val loadedFromDb = true
    override val deletionRecord = deletionSet
  }
  def newMention(v: Vars, id:String) = new Mention(v,id)(null) {
    override val loadedFromDb = true
    override val deletionRecord = deletionSet
  }

  def newNodeCubbie : NodeCubbie[Vars]

  def fetch(v: Vars) = if(isMention.value) newMention(v, this.id.toString) else newNode(v, this.id.toString)

  def store(node: N) = {

    node.getParent match{
      case Some(n) => parentRef := n
      case None    =>
    }
    isMention := node.isMention
    this
  }
}