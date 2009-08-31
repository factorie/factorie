package cc.factorie.util

/***
 * Data structure that represents a tree
 *
 **/

import scala.collection.mutable.{ArrayBuffer, HashSet, Set}

class Tree[T](protected var root: TreeNode[T]) {
	def setRoot(troot: TreeNode[T]) = root = troot

	def setRoot(troot: T) = root = new TreeNode(troot)

	def nodes: Iterable[T] = {
		val arr = new ArrayBuffer[T];
		dfs(arr.+=);
		arr
	}

	def dfsTerminate(func: (T => Boolean)): Boolean = {
		dfs(root, func)
	}

	def dfs(func: (T => Unit)): Boolean = {
		dfsTerminate((node: T) => {func(node); false})
	}

	def dfs(node: TreeNode[T], func: (T => Boolean)): Boolean = {
		// work on this node
		var flag = func(node.value)
		for (child <- node.childNodes; if (!flag))
			flag = dfs(child, func)
		flag // notify upwards that the flag is set
	}

	def print = dfs((node: T) => Console.print(node.toString + " .. "))
}


class TreeNode[T](val _value: T) {
	protected var _parent: TreeNode[T] = _ //signifies root
	protected val _children: ArrayBuffer[TreeNode[T]] = new ArrayBuffer[TreeNode[T]]

	def value: T = _value

	def addChild(child: T): Unit = {
		val childNode = new TreeNode(child)
		_children += childNode
		childNode._parent = this
	}

	def addChild(child: TreeNode[T]): Unit = {
		_children += child
		child._parent = this
	}

	def addChildren(children: Iterable[T]): Unit =
		children.foreach(c => addChild(c))

	def addChildren(children: T*): Unit =
		children.foreach(c => addChild(c))

	def addChildNodes(children: Iterable[TreeNode[T]]): Unit =
		children.foreach(c => addChild(c))

	def addChildNodes(children: TreeNode[T]*): Unit =
		children.foreach(c => addChild(c))

	def children: Iterable[T] = childNodes.map(_ value)

	def childNodes: Iterable[TreeNode[T]] = _children

	def parent: TreeNode[T] = _parent

	def siblingNodes: Iterable[TreeNode[T]] =
		if (_parent != null)
			_parent.childNodes.filter(s => s != this)
		else null

	def siblings: Iterable[T] =
		if (_parent != null)
			siblingNodes.map(_ value)
		else null

	def ancestors: ArrayBuffer[TreeNode[T]] = {
		val ancs = new ArrayBuffer[TreeNode[T]]
		var curr: TreeNode[T] = _parent
		while (curr != null) {
			ancs += curr
			curr = curr.parent
		}
		ancs
	}
}

object Tree {
	def findCommonAncestor[T](node1: TreeNode[T], node2: TreeNode[T]): TreeNode[T] = {
		val node1ancs: Set[TreeNode[T]] = new HashSet
		node1ancs += node1
		node1ancs ++= node1.ancestors
		val node2ancs = new ArrayBuffer[TreeNode[T]]
		node2ancs += node2
		node2ancs ++= node2.ancestors
		var result: TreeNode[T] = null
		for (parent <- node2ancs)
			if ((result == null) && node1ancs.contains(parent))
				result = parent
		result
	}

}
