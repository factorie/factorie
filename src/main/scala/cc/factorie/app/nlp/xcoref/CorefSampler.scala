package cc.factorie.app.nlp.xcoref

import cc.factorie.infer.{Proposal, SettingsSampler}
import scala.util.Random
import cc.factorie.util.Hooks1
import scala.reflect.ClassTag

/**
 * User:harshal, John Sullivan
 * Date: 10/28/13
 */
abstract class CorefSampler[Vars <: NodeVariables[Vars]](model:CorefModel[Vars], val mentions:Iterable[Node[Vars]], val iterations:Int)(implicit override val random:Random, val varsTag:ClassTag[Vars])
  extends SettingsSampler[(Node[Vars], Node[Vars])](model) {
  this: PairContextGenerator[Vars] with MoveGenerator[Vars] =>

  this.temperature = 0.001

  val beforeInferHooks = new Hooks1[Unit]
  protected def beforeInferHook = beforeInferHooks()

  def infer {
    beforeInferHook
    processAll(contexts)
  }

}
