package cc.factorie.app.nlp.xcoref

import scala.annotation.tailrec
import scala.collection.mutable
import cc.factorie.variable._
import cc.factorie.Cubbie
import cc.factorie.util.Hooks1

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
class Node[Vars <: NodeVariables[Vars]](val variables:Vars, val id: String)(implicit d: DiffList) {

  def this(variables:Vars)(implicit d:DiffList) = this(variables, java.util.UUID.randomUUID.toString)(d)

  variables.node = this
  private type RuntimeType = this.type


  //def newInstance(implicit d:DiffList):RuntimeType = new Node[Vars](variables.emptyInstance)(d)

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

  final def parent:Option[Node[Vars]] = Option(parentRef.dst)
  def leaves:Iterable[Mention[Vars]] = mentionsVar.value

  @tailrec
  final def root:Node[Vars] = parent match {
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
      this.parent.get.ancestor(numGenerationsBack -1)
    }
  }


  final def lineage:Iterable[Node[Vars]] = {
    def lineageHelper(pOpt:Option[Node[Vars]], parents:List[Node[Vars]]):List[Node[Vars]] = {
      pOpt match {
        case Some(p) => lineageHelper(p.parent, p :: parents)
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
        case Some(p) => helper(p.parent, cDepth + 1)
        case None => cDepth
      }
    }
    helper(parent,1)
  }

  def markForDeletion {
    parentRef.set(null)(null)
    deleteHooks(this)
  }

  val deleteHooks = new Hooks1[Node[Vars]]

  @tailrec
  private def propagateAddition(addedVariables:Vars)(implicit d:DiffList):Unit = this.parent match {
    case Some(p) => {
      p.variables ++= addedVariables
      p.propagateAddition(addedVariables)(d)
    }
    case None => Unit
  }

  @tailrec
  private def propagateRemoval(removedVariables:Vars)(implicit d:DiffList):Unit = this.parent match {
    case Some(p) => {
      p.variables --= removedVariables
      p.propagateRemoval(removedVariables)(d)
    }
    case None => Unit
  }

  final def alterParent(newParent:Option[Node[Vars]])(implicit d :DiffList) {
    val oldParent = this.parent
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
      oldParent.get.children.head.alterParent(oldParent.get.parent)(d)
    }
  }
}

object Node {
  @tailrec
  protected def propagateRemoveMentions[Vars <: NodeVariables[Vars]](mentionVar: Node[Vars]#Mentions#Value, parentRef:Option[Node[Vars]])(implicit d:DiffList):Unit = parentRef match {
    case Some(p) => {
      p.mentionsVar.removeAll(mentionVar)(d)
      propagateRemoveMentions(mentionVar,p.parent)(d)
    }
    case None => Unit
  }

  @tailrec
  protected def propagateAddMentions[Vars <: NodeVariables[Vars]](mentionVar: Node[Vars]#Mentions#Value, parentRef:Option[Node[Vars]])(implicit d:DiffList):Unit = parentRef match {
    case Some(p) => {
      p.mentionsVar.addAll(mentionVar)(d)
      propagateAddMentions(mentionVar,p.parent)(d)
    }
    case None => Unit
  }
  @tailrec
  protected def propagateUpdateMentionCount[Vars <: NodeVariables[Vars]](update: Int, parentRef:Option[Node[Vars]])(implicit d:DiffList):Unit = parentRef match {
    case Some(p) => {
      p.mentionCountVar.set(p.mentionCountVar.value + update)(d)
      propagateUpdateMentionCount(update,p.parent)(d)
    }
    case None => Unit
  }
}


class Mention[Vars <: NodeVariables[Vars]](v:Vars, id: String)(implicit d:DiffList) extends Node[Vars](v, id)(d) {

  def this(variables:Vars)(implicit d:DiffList) = this(variables, java.util.UUID.randomUUID.toString)(d)
  override final val size = 1
  override final val leaves = this :: Nil
  override final val isMentionVar = new IsMention(true)
  override final val mentionCountVar = new MentionCount(1)
  mentionsVar += this
}

trait NodeVariables[Self <: NodeVariables[Self]] extends SelfVariable[Self] {

  this: Self =>

  var node:Node[Self] = null

  def ++(other:Self)(implicit d:DiffList):Self
  def --(other:Self)(implicit d:DiffList):Self
  def ++=(other:Self)(implicit d:DiffList):Unit
  def --=(other:Self)(implicit d:DiffList):Unit

  def getVariables: Seq[Var]
  def size:Int = getVariables.size

  def nameString:String
}

trait Persistence {
  this: Node[_] with Persistence =>
  protected val loadedFromDb: Boolean

  protected def deleteHook[N <: Node[_]](node: N) {
    node match {
      case n: N with Persistence if n.loadedFromDb =>
        Persistence.deleted += n
      case _ =>
    }
  }
  deleteHooks.append(deleteHook)

  def wasLoadedFromDb = loadedFromDb
  def wasDeleted      = Persistence.deleted(this)
}

object Persistence {
  private val deleted = new mutable.HashSet[Node[_]]()
}

trait NodeSource {
  def source:String
  def moveable:Boolean

  def useable:Boolean = source != "wp" && moveable
}

trait NodeCubbie[Vars <: NodeVariables[Vars], N  <: Node[Vars]] extends Cubbie {
//  type NVC <: Cubbie
  val parentRef = RefSlot("parentRef", () => newNodeCubbie)
  val isMention = BooleanSlot("isMention")
  val wikiUrl = StringSlot("wurl")
  val canopies = StringListSlot("canopies")
  val moveable = BooleanSlot("mv")
  val source = StringSlot("src")
//  protected var _nodeVarsCubbie: Option[NVC] = None

//  def nodeVarsCubbie = _nodeVarsCubbie

  def newNode(v: Vars, id:String)    = new Node(v,id)(null) { protected val loadedFromDb = true }
  def newMention(v: Vars, id:String) = new Mention(v,id)(null) { protected val loadedFromDb = true }

  def newNodeCubbie : NodeCubbie[Vars, N]

  def fetch(v: Vars) = if(isMention.value) newMention(v, this.id.toString) else newNode(v, this.id.toString)

//  def storeVars(v: Vars): NVC

  def store(node: N) = {
//    _nodeVarsCubbie = Some(storeVars(node.variables))
    node.parent match{
      case Some(n) => parentRef := n
      case None    =>
    }
    isMention := node.isMention
    this
  }
}

/*************************************************************************************/
/*
abstract class MyNodeVariables(names:BagOfWordsVariable, context:BagOfWordsVariable) extends NodeVariables[MyNodeVariables] {
  def combine(other: MyNodeVariables): MyNodeVariables = {
    this.names ++= other.names.activeCategories
    this.context ++= other.context.activeCategories
    this
  }

  def remove(other: MyNodeVariables): MyNodeVariables = this

  def getVariables = Seq(names, context)
}

class HcorefNodeCubbie extends NodeCubbie[MyNodeVariables, Node[MyNodeVariables] with Persistence] {

  def newNodeCubbie: HcorefNodeCubbie = new HcorefNodeCubbie()
}

class HcorefCubbieCollection(names:Seq[String], mongoDB:DB)
  extends MongoNodeCollection[MyNodeVariables,Node[MyNodeVariables] with Persistence,HcorefNodeCubbie](names, mongoDB){

  protected def newBOWCubbie = new edu.umass.cs.iesl.variable.BOWCubbie()

  protected def newNodeVars(vars: cc.factorie.Var*): MyNodeVariables = {
    val names = vars(0).asInstanceOf[BagOfWordsVariable]
    val context = vars(1).asInstanceOf[BagOfWordsVariable]
    new MyNodeVariables(names, context)
  }

  protected def newNodeCubbie: HcorefNodeCubbie = new HcorefNodeCubbie

  protected def newNode(v: MyNodeVariables, nc: HcorefNodeCubbie) = {
    if(nc.isMention.value){
      new Mention(v,nc.id.toString) with Persistence {
        protected val loadedFromDb = true
      }
    }
    else {
      new Node(v,nc.id.toString) with Persistence {
        protected val loadedFromDb = true
      }
    }
  }
}

object Trial {
  val m1Vars = new MyNodeVariables(new BagOfWordsVariableWithDomain(Seq("Andrew", "McCallum", "A. McCallum")),
    new BagOfWordsVariableWithDomain(Seq("CRF", "MEMM", "Umass")))
  val m1 = new Mention(m1Vars) with Persistence{ protected val loadedFromDb = false }
  val m2 = new Mention(m1Vars) with Persistence{ protected val loadedFromDb = false }
  val n  = new Node(new MyNodeVariables(new BagOfWordsVariableWithDomain(Nil),new BagOfWordsVariableWithDomain(Nil)))
    with Persistence{ protected val loadedFromDb = false }
  m1.alterParent(Some(n))
  m2.alterParent(Some(n))
  //persist
  val mongoConn = new MongoClient("localhost",27017)
  val mongoDb   = mongoConn.getDB("test")
  val corefCollection = new HcorefCubbieCollection(Seq("mentions","names","context"), mongoDb)
  corefCollection += m1
  corefCollection += m2
  corefCollection += n
  //retrieve
  val mentions = corefCollection.loadAll
}

*/