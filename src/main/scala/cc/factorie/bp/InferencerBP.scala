package cc.factorie.bp

import cc.factorie._
import collection.mutable.{Queue, ArrayBuffer, HashSet}

/**
 * @author sameer
 * @date 2/7/12
 */

class InferencerBP extends VariableInferencer[Variable] {
  type LatticeType = LatticeBP

  def infer(variables: Iterable[Variable], varying: Iterable[Variable]) = {
    // TODO: No access to model yet
    val lattice = new LatticeBP(varying.map(_.asInstanceOf[DiscreteVariable]).toSet) with SumProductLattice
    val inf = new InferencerBPWorker(lattice)
    inf.inferLoopyBP()
    lattice
  }
}

class InferencerBPWorker(lattice: LatticeBP) {
  private def _bfs(root: MessageNode, checkLoops: Boolean): Seq[(Edge, Boolean)] = {
    val visited: HashSet[(Edge, Boolean)] = new HashSet
    val result = new ArrayBuffer[(Edge, Boolean)]
    val toProcess = new Queue[(Edge, Boolean)]
    root.edges foreach (e => toProcess += Pair(e, true))
    while (!toProcess.isEmpty) {
      val (edge, varRoot) = toProcess.dequeue()
      if (!checkLoops || !visited((edge, varRoot))) {
        visited += Pair(edge, varRoot)
        result += Pair(edge, varRoot)
        val edges =
          if (varRoot) edge.f.edges.filter(_ != edge)
          else {
            if (edge.n.varies) edge.n.edges.filter(_ != edge) else Seq.empty[Edge]
          }
        edges.foreach(ne => toProcess += Pair(ne, !varRoot))
      }
    }
    result
  }

  def inferTreewise(variable: DiscreteVariable = lattice.varying.head, checkLoops: Boolean = true): Unit = _inferTreewise(lattice.node(variable), checkLoops)

  // Perform a single iteration of up-down BP using the given root to discover the tree. For
  // loopy models, enable checkLoops to avoid infinite loops.
  private def _inferTreewise(root: MessageNode = lattice.nodes.head, checkLoops: Boolean = true) {
    // perform BFS
    val bfsOrdering: Seq[(Edge, Boolean)] = _bfs(root, checkLoops)
    // send messages leaf to root
    var i = bfsOrdering.length - 1
    while (i >= 0) {
      val (e, varRoot) = bfsOrdering(i)
      if (varRoot) {
        e.fToV
      } else {
        e.vToF
      }
      i -= 1
    }
    lattice match {
      case l: MaxProductLattice => {
        l.finalPass = true
        root.maxAsMarginal
      }
      case _ =>
    }
    i = 0
    // send root to leaves
    while (i < bfsOrdering.length) {
      val (e, varRoot) = bfsOrdering(i)
      if (varRoot) {
        e.vToF
      } else {
        e.fToV
      }
      i += 1
    }
  }

  // Perform up-down scheduling of messages. Picks a random root, and performs BFS
  // ordering to discover the tree (efficiently if checkLoops is false), resulting in
  // exact inference for tree-shaped models. For loopy models, enable checkLoops to avoid
  // infinite loops, and have iterations>1 till convergence.
  def inferUsingTreeSchedule(iterations: Int = 1, checkLoops: Boolean = true) {
    for (iteration <- 0 until iterations) {
      // find a random root
      val root = lattice.nodes.sampleUniformly
      // treewise
      _inferTreewise(root, checkLoops)
      // println("Iteration %d max delta range: %f".format(iteration, currentMaxDelta))
    }
  }

  def inferLoopyBP(iterations: Int = 1) {
    for (iteration <- 0 until iterations) {
      for (factor <- lattice.mfactors.toSeq.shuffle(cc.factorie.random)) {
        //for every factor first calculate all incoming beliefs
        factor.receiveFromAll
        //synchronous belief updates on all send edges
        factor.sendToAll
      }
      // println("Iteration %d max delta range: %f".format(iteration, currentMaxDelta))
    }
  }

  def inferParallelLoopyBP(iterations: Int = 1) {
    for (iteration <- 0 until iterations) {
      lattice.mfactors.toSeq.shuffle(cc.factorie.random).par.foreach(factor => {
        //for every factor first calculate all incoming beliefs
        factor.receiveFromAll
        //synchronous belief updates on all send edges
        factor.sendToAll
      })
      // println("Iteration %d max delta range: %f".format(iteration, currentMaxDelta))
    }
  }
}