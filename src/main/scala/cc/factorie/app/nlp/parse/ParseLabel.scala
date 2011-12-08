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

package cc.factorie.app.nlp.parse
import cc.factorie._
import cc.factorie.app.nlp._
import collection.mutable.{HashSet, ArrayBuffer}

// Representation for a dependency parse

object ParseLabelDomain extends CategoricalDomain[String]
class ParseLabel(val edge:ParseEdge, targetValue:String) extends LabelVariable(targetValue) { def domain = ParseLabelDomain } 

object ParseFeaturesDomain extends CategoricalVectorDomain[String]
class ParseFeatures(val token:Token) extends BinaryFeatureVectorVariable[String] { def domain = ParseFeaturesDomain }

class ParseEdge(theChild:Token, initialParent:Token, labelString:String) extends ArrowVariable[Token,Token](theChild, initialParent) {
  @inline final def child = src
  @inline final def parent = dst
  val label = new ParseLabel(this, labelString)
  val childEdges = new ParseChildEdges

  // Initialization
  child.attr += this // Add the edge as an attribute to the child node.
  if (initialParent ne null) initialParent.attr[ParseEdge].childEdges.add(this)(null)
  // Note that the line above requires that the parent already have a ParseEdge attribute.
  // One way to avoid this need is to create the ParseEdges with null parents, and then set them later.
  
  override def set(newParent: Token)(implicit d: DiffList): Unit =
    if (newParent ne parent) {
      // update parent's child pointers
      if (parent ne null) parent.attr[ParseEdge].childEdges.remove(this)
      if (newParent ne null) newParent.attr[ParseEdge].childEdges.add(this)
      super.set(newParent)(d)
    }
}

class ParseChildEdges extends SetVariable[ParseEdge]

// Example usages:
// token.attr[ParseEdge].parent
// token.attr[ParseEdge].childEdges.map(_.child)
// token.attr[ParseChildEdges].map(_.child)

