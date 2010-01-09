/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.util.tree

/***
 * Test for the Tree data structure
 **/

object TreeTest {
  val n0 = new TreeNode("0")
  val n00 = new TreeNode("00")
  val n01 = new TreeNode("01")
  val n000 = new TreeNode("000")
  val n001 = new TreeNode("001")
  val n010 = new TreeNode("010")
  val n011 = new TreeNode("011")
  val n0100 = new TreeNode("0100")
  val n0101 = new TreeNode("0101")

  def createTree = {
    //n0.addChild(n01)
    n0.addChildNodes(n00, n01)
    n00.addChildNodes(n000, n001)
    n01.addChildNodes(n010, n011)
    n010.addChildNodes(n0100, n0101)
    new Tree(n0)
  }

  def testCommonAncestor = {
    assert(Tree.findCommonAncestor(n0101, n00) == n0)
    assert(Tree.findCommonAncestor(n0101, n0100) == n010)
    assert(Tree.findCommonAncestor(n000, n010) == n0)
    assert(Tree.findCommonAncestor(n0, n011) == n0)
    assert(Tree.findCommonAncestor(n0101, n00) == n0)
  }

  def main(args: Array[String]): Unit = {
    val tree = createTree
    tree.print
    println
    println(tree.nodes)
    testCommonAncestor
  }
}
