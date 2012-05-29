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

class Parser {
  
  object ParserModel extends TemplateModel(
    // Bias term on each individual label 
    new TemplateWithDotStatistics1[ParseLabel] {
      def statisticsDomains = Tuple(ParseLabelDomain)
    },
    // Factor between label and observed token
    new Template3[ParseEdge,ParseFeatures,ParseFeatures] with DotStatistics2[DiscreteTensorValue,DiscreteTensorValue] with SparseWeights {
      def statisticsDomains = Tuple(ParseFeaturesDomain, ParseFeaturesDomain)
      def unroll1(n:ParseEdge) = Factor(n, n.child.attr[ParseFeatures], n.parent.attr[ParseFeatures])
      def unroll2(child:ParseFeatures) = { val edge = child.token.attr[ParseEdge]; Factor(edge, child, edge.parent.attr[ParseFeatures]) }
      def unroll3(parent:ParseFeatures) = Nil
      def statistics(values:Values) = Stat(values._2, values._3)
    },
    new Template4[ParseEdge,ParseLabel,ParseFeatures,ParseFeatures] with DotStatistics2[DiscreteTensorValue,DiscreteTensorValue] with SparseWeights {
      def statisticsDomains = Tuple(ParseLabelDomain, ParseFeaturesDomain)
      def unroll1(n:ParseEdge) = Factor(n, n.label, n.child.attr[ParseFeatures], n.parent.attr[ParseFeatures])
      def unroll2(label:ParseLabel) = { val edge = label.edge; Factor(edge, label, edge.child.attr[ParseFeatures], edge.parent.attr[ParseFeatures]) }
      def unroll3(child:ParseFeatures) = { val edge = child.token.attr[ParseEdge]; Factor(edge, edge.label, child, edge.parent.attr[ParseFeatures]) }
      def unroll4(parent:ParseFeatures) = Nil
      def statistics(values:Values) = Stat(values._2, values._3)
    }
  )

  def parse(sentence:Sentence): Unit = {
    val stack = new scala.collection.mutable.Stack[Token]
    for (token <- sentence.tokens) {
      token.attr += new ParseEdge(token, if (token.sentenceHasPrev) token.prev else null, "SUBJ")
    }
  }
}