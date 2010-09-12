/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
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

/***
 * Test for the Tree data structure
 **/

// TODO this should be moved to the factorie/src/test/scala directory
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
