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



package cc.factorie.er
import cc.factorie._
import scala.collection.mutable.{ArrayStack,HashSet,HashMap,ListBuffer}


// Define function for scoring compatibility between getter targets with CategoricalValues
// Example usage:  Forany[Token] { t => Score(t, t.label) }


// TODO!!! Put this back for 2.8.0.Beta2
// See http://old.nabble.com/Re:--scala-internals--RC8-candidate-for-the-first-2.8.0-beta-td27262766.html
class Score[X<:Variable](val sns:ScoreNeighbor0[X]*) {
  def manifests : Seq[Manifest[_<:Variable]] = sns.flatMap(_.manifests)
  //def getters : Seq[GetterHead[X,ScorableValues0]] = sns.flatMap(_.getters)
  def getters : Seq[Getter[ScorableValues0] {type A = X}] = sns.flatMap(_.getters)
}
object Score {
  def apply[X<:Variable](sns:ScoreNeighbor0[X]*) = new Score[X](sns:_*)
}
//  case class Score[X<:Variable](sns:ScoreNeighbor0[X]) {
//    def manifests : Seq[Manifest[_<:Variable]] = List(sns).flatMap(_.manifests)
//    //def getters : Seq[GetterHead[X,ScorableValues0]] = sns.flatMap(_.getters)
//    def getters : Seq[Getter[ScorableValues0] {type A = X}] = List(sns).flatMap(_.getters)
//  }
//  case class Score2[X<:Variable](sn1:ScoreNeighbor0[X], sn2:ScoreNeighbor0[X]) {
//    def manifests : Seq[Manifest[_<:Variable]] = List(sn1,sn2).flatMap(_.manifests)
//    //def getters : Seq[GetterHead[X,ScorableValues0]] = sns.flatMap(_.getters)
//    def getters : Seq[Getter[ScorableValues0] {type A = X}] = List(sn1,sn2).flatMap(_.getters)
//  }
case class SparseScore[X<:Variable](override val sns:ScoreNeighbor0[X]*) extends Score(sns:_*)
trait ScoreNeighbor0[X<:Variable] {
  def manifests : Iterable[Manifest[ScorableValues0]];
  //def getters : Seq[GetterHead[X,ScorableValues0]]
  def getters : Seq[Getter[ScorableValues0] {type A = X}]
}
//class ScoreNeighbor[X<:Variable,A<:ScorableValues[A]](a1:GetterHead[X,A])(implicit ma:Manifest[A]) extends ScoreNeighbor0[X]
class ScoreNeighbor[X<:Variable,A<:ScorableValues[A]](a1:Getter[A] /*with GetterHead[X,A]*/)(implicit ma:Manifest[A]) extends ScoreNeighbor0[X] {
  def manifests = List(ma.asInstanceOf[Manifest[ScorableValues0]])
  //def getters = List(a1.asInstanceOf[GetterHead[X,ScorableValues0]])
  def getters = List(a1.asInstanceOf[Getter[ScorableValues0] {type A = X}])
}

object Foreach {
  def apply[X<:Variable {type GetterType <: Getter[X]}](x2c:X#GetterType { type A = X}=>Score[X])(implicit m:Manifest[X]) = {
    val score = x2c(newGetterUnit[X](m))
    val manifests = score.manifests.toList.asInstanceOf[List[Manifest[ScorableValues0]]];
    val getters = score.getters
    val size = manifests.length
    type I = ScorableValues0
    if (classOf[SparseScore[X]].isAssignableFrom(score.getClass)) {
      size match {
        case 1 => new TemplateWithDotStatistics1[I]()(manifests(0)) with SparseWeights {
          override def unroll1(n1:I) = { val roots = getters(0).reverse(n1); if (!roots.isEmpty) Factor(n1) else Nil }
        }
        case 2 => new TemplateWithDotStatistics2[I,I]()(manifests(0), manifests(1)) with SparseWeights {
          def unroll1(n1:I) = { val roots = getters(0).reverse(n1); for (root <- roots; n2 <- getters(1).forward(root)) yield Factor(n1,n2) }
          def unroll2(n2:I) = { val roots = getters(1).reverse(n2); for (root <- roots; n1 <- getters(0).forward(root)) yield Factor(n1,n2) }
        }
        case 3 => new TemplateWithDotStatistics3[I,I,I]()(manifests(0), manifests(1), manifests(2)) with SparseWeights {
          def unroll1(n1:I) = { val roots = getters(0).reverse(n1); for (root <- roots; n2 <- getters(1).forward(root); n3 <- getters(2).forward(root)) yield Factor(n1,n2,n3) } 
          def unroll2(n2:I) = { val roots = getters(1).reverse(n2); for (root <- roots; n1 <- getters(0).forward(root); n3 <- getters(2).forward(root)) yield Factor(n1,n2,n3) } 
          def unroll3(n3:I) = { val roots = getters(2).reverse(n3); for (root <- roots; n1 <- getters(0).forward(root); n2 <- getters(1).forward(root)) yield Factor(n1,n2,n3) } 
        }
        case 4 => new TemplateWithDotStatistics4[I,I,I,I]()(manifests(0), manifests(1), manifests(2), manifests(3)) with SparseWeights {
          def unroll1(n1:I) = { val roots = getters(0).reverse(n1); for (root <- roots; n2 <- getters(1).forward(root); n3 <- getters(2).forward(root); n4 <- getters(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
          def unroll2(n2:I) = { val roots = getters(1).reverse(n2); for (root <- roots; n1 <- getters(0).forward(root); n3 <- getters(2).forward(root); n4 <- getters(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
          def unroll3(n3:I) = { val roots = getters(2).reverse(n3); for (root <- roots; n1 <- getters(0).forward(root); n2 <- getters(1).forward(root); n4 <- getters(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
          def unroll4(n4:I) = { val roots = getters(3).reverse(n4); for (root <- roots; n1 <- getters(0).forward(root); n2 <- getters(1).forward(root); n3 <- getters(2).forward(root)) yield Factor(n1,n2,n3,n4) } 
        }
      }
    } else {
      size match {
        case 1 => new TemplateWithDotStatistics1[I]()(manifests(0)) {
          override def unroll1(n1:I) = { val roots = getters(0).reverse(n1); if (!roots.isEmpty) Factor(n1) else Nil }
        }
        case 2 => new TemplateWithDotStatistics2[I,I]()(manifests(0), manifests(1)) {
          def unroll1(n1:I) = { val roots = getters(0).reverse(n1); for (root <- roots; n2 <- getters(1).forward(root)) yield Factor(n1,n2) }
          def unroll2(n2:I) = { val roots = getters(1).reverse(n2); for (root <- roots; n1 <- getters(0).forward(root)) yield Factor(n1,n2) }
        }
        case 3 => new TemplateWithDotStatistics3[I,I,I]()(manifests(0), manifests(1), manifests(2)) {
          def unroll1(n1:I) = { val roots = getters(0).reverse(n1); for (root <- roots; n2 <- getters(1).forward(root); n3 <- getters(2).forward(root)) yield Factor(n1,n2,n3) } 
          def unroll2(n2:I) = { val roots = getters(1).reverse(n2); for (root <- roots; n1 <- getters(0).forward(root); n3 <- getters(2).forward(root)) yield Factor(n1,n2,n3) } 
          def unroll3(n3:I) = { val roots = getters(2).reverse(n3); for (root <- roots; n1 <- getters(0).forward(root); n2 <- getters(1).forward(root)) yield Factor(n1,n2,n3) } 
        }
        case 4 => new TemplateWithDotStatistics4[I,I,I,I]()(manifests(0), manifests(1), manifests(2), manifests(3)) {
          def unroll1(n1:I) = { val roots = getters(0).reverse(n1); for (root <- roots; n2 <- getters(1).forward(root); n3 <- getters(2).forward(root); n4 <- getters(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
          def unroll2(n2:I) = { val roots = getters(1).reverse(n2); for (root <- roots; n1 <- getters(0).forward(root); n3 <- getters(2).forward(root); n4 <- getters(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
          def unroll3(n3:I) = { val roots = getters(2).reverse(n3); for (root <- roots; n1 <- getters(0).forward(root); n2 <- getters(1).forward(root); n4 <- getters(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
          def unroll4(n4:I) = { val roots = getters(3).reverse(n4); for (root <- roots; n1 <- getters(0).forward(root); n2 <- getters(1).forward(root); n3 <- getters(2).forward(root)) yield Factor(n1,n2,n3,n4) } 
        }
      }
    }
  }
}

