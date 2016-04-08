/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
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
package cc.factorie.util

import cc.factorie.la.Tensor2

import scala.annotation.tailrec

/**
 * A solver for weighted bipartite matching, also known as the assignment problem.
 *
 * This does not use the Hungarian algorithm; instead it uses the augmenting paths
 * algorithm which works by finding in sequence a best matching with one edge, a
 * best matching with two edges, etc, until it find a best matching with maximal
 * size. Then it goes back and returns the best overall matching out of those.
 *
 * Transforming the best matching with n nodes into the best matching with n+1 nodes
 * is done by finding the best augmenting path: define a new graph with a source
 * connected with zero weight edges to the unmatched nodes on the "left" side of the
 * matching, and likewise a target connected to the unmatched nodes on the right side.
 * For the bipartite graph, have the edges in the matching have positive weight equal
 * to their cost and going from right to left, while edges not in the matching have
 * negative weights equal to their costs and go from left to right.
 *
 * Then a shortest path from the source to target will necessarily add one edge to the
 * matching for each edge it removes, and add one more edge than that.
 *
 * To see a proof of this algorithm see the lecture notes in
 * http://compgeom.cs.uiuc.edu/~jeffe/teaching/algorithms/2009/notes/18-maxflowext.pdf
 *
 * Note that we can't use Dijkstra for shortest-paths because of the negative-weighted
 * edges. We can't have negatively weighted cycles because the existence of such a
 * cycle would imply that the current state is not a maximal matching with its size.
 *
 * This is useful for implementing the CEAF evaluation metrics in coreference
 * resolution, which assign scores between pairs of truth and returned entities and
 * define the precision and recall of a clustering as a function of the best
 * possible matching between truth and returned clusters.
 * 
 * @author Alexandre Passos
 */
class AssignmentSolver(val weights: Tensor2) {
  // This is the bellman-ford algorithm applied to the specific graph we
  // described earlier
  def shortestPath(currentParents: Array[Int]): Set[(Int,Int)] = {
    // from the source to the left nodes the cost is zero
    val leftScores = Array.fill(weights.dim1)(Double.PositiveInfinity)
    for (i <- 0 until weights.dim1) if (currentParents(i) == -1) leftScores(i) = 0
    val leftParents = Array.fill(weights.dim1)(-1)
    val rightScores = Array.fill(weights.dim2)(Double.PositiveInfinity)
    val rightParents = Array.fill(weights.dim2)(-1)
    for (iter <- 0 until 2*(weights.dim1 + weights.dim2);
         source <- 0 until weights.dim1; target <- 0 until weights.dim2) {
      if (currentParents(source) == target) {
        // edge goes from right to left with positive cost
        if (leftScores(source) > rightScores(target) + weights(source, target)) {
          leftScores(source) = rightScores(target) + weights(source, target)
          leftParents(source) = target
        }
      } else {
        // edge goes from left to right with negative cost
        if (rightScores(target) > leftScores(source) - weights(source, target)) {
          rightScores(target) = leftScores(source) - weights(source, target)
          rightParents(target) = source
        }
      }
    }
    val returnSet = collection.mutable.ListBuffer[(Int,Int)]()
    @tailrec def addEdges(right: Int) {
      if(returnSet.length >= weights.dim1) {
        println("Warning, Cycle in CM, CE calculation");
        return
      }
      val left = rightParents(right)
      returnSet += ((left,right))
      assert(returnSet.length <= weights.dim1)
      if (leftParents(left) != -1) {
        val right2 = leftParents(left)
        returnSet += ((left, right2))
        addEdges(right2)
      }
    }
    val currentChildren = Array.fill(weights.dim2)(-1)
    for (i <- 0 until currentParents.length) if (currentParents(i) != -1) currentChildren(currentParents(i)) = i
    val rightWinner = (0 until weights.dim2).filter(currentChildren(_) == -1).minBy(rightScores)
    addEdges(rightWinner)
    returnSet.toSet
  }

  def solve(): Seq[(Int,Int)] = {
    val N = math.min(weights.dim1, weights.dim2)
    val bestMatchings = (0 to N).map(_ => collection.mutable.HashSet[(Int,Int)]())
    val matchScores = (0 to N).map(_ => 0.0).toArray
    for (currentSize <- 1 to N) {
      val parents = Array.fill(weights.dim1)(-1)
      bestMatchings(currentSize-1).foreach(edge => parents(edge._1) = edge._2)
      val augmentingPath = shortestPath(parents).toSet
      //println("augmenting path: " + augmentingPath.mkString(" "))
      for (edge <- bestMatchings(currentSize-1).diff(augmentingPath)) bestMatchings(currentSize) += edge
      for (edge <- augmentingPath.diff(bestMatchings(currentSize-1))) bestMatchings(currentSize) += edge
      for (edge <- bestMatchings(currentSize)) matchScores(currentSize) += weights(edge._1, edge._2)
    }
    val i = (0 to N).maxBy(matchScores(_))
    bestMatchings(i).toSeq
  }
}

