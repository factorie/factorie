package cc.factorie.app.nlp.xcoref

import scala.util.Random
import scala.reflect.ClassTag

/**
 * @author John Sullivan
 */
abstract class HierarchicalCorefSampler[Vars <: NodeVariables[Vars] with Canopy](model :CorefModel[Vars], mentions:Iterable[Node[Vars]], iterations:Int)(implicit random:Random, ct:ClassTag[Vars])
  extends CorefSampler[Vars](model, mentions, iterations)(random, ct)
  with DefaultMoveGenerator[Vars]
  with CanopyPairGenerator[Vars]