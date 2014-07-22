package cc.factorie.app.nlp.hcoref

import cc.factorie.variable.{DiffList, SettingIterator}
import cc.factorie.infer.{SettingsSampler, Proposal}
import scala.collection.mutable

trait Verbosity {
  var samplingStep:Int = 0
  var deltaScore:Double = 0.0

  def dScoreString = "%.5f".format(deltaScore)
  var prec:Double = 0.0
  var recall:Double = 0.0
  var fOne:Double = 0.0

  var srcId:String =""
  var srcIsMent:Boolean = false
  var srcIsEnt:Boolean = false
  var srcBagSize:Int = 0
  var srcDepth:Int = 0
  var srcMentionCount:Int = 0

  var dstId:String = ""
  var dstIsMent:Boolean = false
  var dstIsEnt:Boolean = false
  var dstBagSize:Int = 0
  var dstDepth:Int = 0
  var dstMentionCount:Int = 0

  var moveType:String = ""
  var consideredBefore:Boolean = false
  var accepted:Boolean = false

  var newParentId:String = ""

  var numEnts:Int = 0
  var numSubEnts:Int = 0

  def writeOut:String = s"$samplingStep\t$moveType\t$srcId\t$dstId\t$newParentId\t$dScoreString\t$prec\t$recall\t$fOne\t$accepted\t$consideredBefore\t$srcDepth\t$dstDepth\t$srcBagSize\t$dstBagSize\t$srcMentionCount\t$dstMentionCount\t$srcIsMent\t$dstIsMent\t$srcIsEnt\t$dstIsEnt\t$numEnts\t$numSubEnts"
}
object Verbosity {
  def header:String = "Sampling Steps\tMove Type\tSource Id\tDest Id\tNew Parent Id\tDelta Model Score\tPrecision\tRecall\tF1\tAccepted?\tConsidered Before?\tSource Depth\tDest Depth\tSource Bag Size\tDest Bag Size\tSource Mention Count\t Dest MentionCount\tSource Mention?\tDest Mention?\tSource Entity?\t Dest Entity?\tNumber of Entities\tNumber of Sub-Entities"

}

trait VerboseSettingIterator extends SettingIterator {
  override def newDiffList: DiffList = new DiffList with Verbosity
}

trait VerboseMove[Vars <: NodeVariables[Vars]] extends Move[Vars] {

  def getBagSize(n:Node[Vars]):Int

  override abstract def operation(node1:Node[Vars], node2:Node[Vars])(d:DiffList):DiffList = {
    d match {
      case v:Verbosity =>
        v.srcId = node2.id.toString
        v.srcDepth = node2.depth - 1
        v.srcBagSize = getBagSize(node2)
        v.srcIsEnt = node2.isRoot
        v.srcIsMent = node2.isMention
        v.srcMentionCount = node2.mentionCountVar.value

        v.dstId = node1.id.toString
        v.dstDepth = node1.depth - 1
        v.dstBagSize = getBagSize(node1)
        v.dstIsEnt = node1.isRoot
        v.dstIsMent = node1.isMention
        v.dstMentionCount = node1.mentionCountVar.value


        v.moveType = this.name

      case _ => println("Difflist not Verbosity: %s (in move %s)".format(d, this.name))
    }
    val res = super.operation(node1, node2)(d)
    d match {
      case v:Verbosity =>
        node1.parent match {
          case Some(p) => v.newParentId = p.id
          case None => Unit
        }
      case _ => Unit
    }
    res
  }
}

trait VerboseMoveGenerator[Vars <: NodeVariables[Vars]] extends MoveGenerator[Vars] {
  this :SettingsSampler[(Node[Vars], Node[Vars])] =>

  def outerGetBagSize(n:Node[Vars]):Int

  def settings(c:(Node[Vars], Node[Vars])) = new VerboseSettingIterator with MoveSettingIterator[Vars] {
    var (e1, e2) = c

    val moves = new scala.collection.mutable.ArrayBuffer[Move[Vars]]()

    if(e1.root != e2.root) {
      if(e1.isMention && e1.isRoot && e2.isMention && e2.isRoot) {
        moves += new MergeUp[Vars](e1, e2)({d => newInstance(d)}) with VerboseMove[Vars] {def getBagSize(n:Node[Vars]) = outerGetBagSize(n)}
      } else {
        while (e1 != null) {
          if(e1.mentionCountVar.value >= e2.mentionCountVar.value) {
            moves += new MergeLeft[Vars](e1, e2) with VerboseMove[Vars] {def getBagSize(n:Node[Vars]) = outerGetBagSize(n)}
          } else {
            moves += new MergeLeft[Vars](e2, e1) with VerboseMove[Vars] {def getBagSize(n:Node[Vars]) = outerGetBagSize(n)}
          }
          e1 = e1.parent.getOrElse(null.asInstanceOf[Node[Vars]])
        }
      }
    } else {
      if(e1.mentionCountVar.value > e2.mentionCountVar.value) {
        moves += new SplitRight[Vars](e2, e1) with VerboseMove[Vars] {def getBagSize(n:Node[Vars]) = outerGetBagSize(n)}
      } else {
        moves += new SplitRight[Vars](e1, e2) with VerboseMove[Vars] {def getBagSize(n:Node[Vars]) = outerGetBagSize(n)}
      }
    }

    moves += new NoMove[Vars] with VerboseMove[Vars] {def getBagSize(n:Node[Vars]) = outerGetBagSize(n)}
  }
}

trait VerboseSampler[C] {
  this: SettingsSampler[C] =>

  private val _verbosities = new mutable.ArrayBuffer[Verbosity]()
  def verbosities:Iterable[Verbosity] = _verbosities
  private var numSamples = 0

  override def proposals(context:C): Seq[Proposal[C]] = {
    val result = new mutable.ArrayBuffer[Proposal[C]]
    // the call to 'next' is actually what causes the change in state to happen
    var i = 0
    val si = settings(context)
    while (si.hasNext) {
      val d = si.next()
      assert(model ne null) // TODO!!! Clean up and delete this
      val (m,o) = d.scoreAndUndo(model, objective)
      d match {
        case v:Verbosity => {
          v.deltaScore = m
          v.samplingStep = numSamples
        }
        case _ => println("Difflist not Verbosity: %s (In Proposal)".format(d))
      }
      //if (proposalsCache.length == i) proposalsCache.append(null)
      result += new Proposal(d, m, o, m/temperature, context)
      i += 1
    }
    result
  }

  override def processProposals(props: Seq[Proposal[C]]): DiffList = {
    if (props.size == 0 && skipEmptyProposals) return newDiffList
    proposalsHook(props)
    val proposal = props.size match {
      case 0 => throw new Error("No proposals created.")
      case 1 => props.head
      case _ => {
        val p = pickProposal(props)
        p.diff match {
          case v:Verbosity => v.accepted = true
          case _ => println("Difflist not Verbosity: %s (In Proposal)".format(p.diff))
        }
        //p.diff.asInstanceOf[Verbosity].accepted = true
        p
      }
    }
    props.filter(_.diff.size != 0).foreach { prop =>
      prop.diff match {
        case v:Verbosity =>
          if(!v.accepted) {
            v.newParentId = ""
          }
          _verbosities += v

        case _ => Unit
      }
    }
    numSamples += 1
    proposal.diff.redo()
    proposalHook(proposal)
    proposal.diff
  }
}