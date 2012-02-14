package cc.factorie.bp

import cc.factorie._
import collection.mutable.{Queue, ArrayBuffer, HashSet}

/**
 * @author sameer
 * @date 2/7/12
 */

class InferencerBP(lattice: LatticeBP) extends VariableInferencer[Variable] {
  type LatticeType = LatticeBP

  def infer(variables: Iterable[Variable], varying: Iterable[Variable]) = {
    // TODO: No access to model yet
    val lattice = new LatticeBP(varying.toSet) with SumProductLattice
    val inf = new InferencerBPWorker(lattice)
    inf.inferLoopyBP()
    lattice
  }
}

class InferencerBPWorker(lattice: LatticeBP) {
  private def _bfs(root: MessageNode, checkLoops: Boolean): Seq[MessageFactor] = {
    val visited: HashSet[MessageFactor] = new HashSet
    val result = new ArrayBuffer[MessageFactor]
    val toProcess = new Queue[(MessageNode, MessageFactor)]
    root.neighbors foreach (f => toProcess += Pair(root, f))
    while (!toProcess.isEmpty) {
      val (origin, factor) = toProcess.dequeue()
      if (!checkLoops || !visited(factor)) {
        visited += factor
        result += factor
        for (node <- factor.nodes; if (node != origin && node.varies)) {
          node.neighbors filter (_ != factor) foreach (f => toProcess += Pair(node, f))
        }
      }
    }
    result
  }

  def inferUpDown(variable: DiscreteVariable, checkLoops: Boolean = true): Unit = inferTreewise(lattice.node(variable), checkLoops)

  // Perform a single iteration of up-down BP using the given root to discover the tree. For
  // loopy models, enable checkLoops to avoid infinite loops.
  def inferTreewise(root: MessageNode = lattice.nodes.head, checkLoops: Boolean = true) {
    // perform BFS
    val bfsOrdering: Seq[MessageFactor] = _bfs(root, checkLoops)
    // send messages leaf to root
    for (factor <- bfsOrdering.reverse) {
      factor.prepareAllIncoming()
      factor.updateAllOutgoing()
    }
    // send root to leaves
    for (factor <- bfsOrdering) {
      factor.prepareAllIncoming()
      factor.updateAllOutgoing()
    }
  }

  // Perform up-down scheduling of messages. Picks a random root, and performs BFS
  // ordering to discover the tree (efficiently if checkLoops is false), resulting in
  // exact inference for tree-shaped models. For loopy models, enable checkLoops to avoid
  // infinite loops, and have iterations>1 till convergence.
  def inferTreeUpDown(iterations: Int = 1, checkLoops: Boolean = true) {
    for (iteration <- 0 until iterations) {
      // find a random root
      val root = lattice.nodes.sampleUniformly
      // treewise
      inferTreewise(root, checkLoops)
      // println("Iteration %d max delta range: %f".format(iteration, currentMaxDelta))
    }
  }

  def inferLoopyBP(iterations: Int = 1) {
    for (iteration <- 0 until iterations) {
      for (factor <- lattice.mfactors.toSeq.shuffle(cc.factorie.random)) {
        //for every factor first calculate all incoming beliefs
        factor.prepareAllIncoming()
        //synchronous belief updates on all outgoing edges
        factor.updateAllOutgoing()
      }
      // println("Iteration %d max delta range: %f".format(iteration, currentMaxDelta))
    }
  }

}