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

package cc
import java.io.{PrintStream, ByteArrayOutputStream, BufferedReader}

import cc.factorie.util._
import cc.factorie.variable.BagOfWords

import scala.language.implicitConversions
import scala.reflect.runtime.universe._
import scala.util.{Success, Failure, Random}

package object factorie extends CubbieConversions {
  var random = new Random(0)
  def setRandomSeed(seed: Long): Unit = {
    random = new Random(seed)
  }
  def repeat(n:Int)(f: =>Unit) : Unit = for (i <- 0 until n) f

  implicit class AnyExtras[T](val a: T) extends AnyVal {
    def cast[U](implicit u : TypeTag[U], t:TypeTag[T]): Option[U] = if (typeOf[U] <:< typeOf[T]) Some(a.asInstanceOf[U]) else None
    def toNotNull: Option[T] = Option(a)
  }

  def when[A](cond:Boolean, a: => A):Option[A] = if(cond) Some(a) else None

  implicit def traversableExtras[A](t: Traversable[A]) = new cc.factorie.util.TraversableExtras[A](t)
  implicit def stringExtras(x:String) = new cc.factorie.util.StringExtras(x)
  implicit class IntPairExtras(val x:(Int, Int)) {
    def overlapsWith(y:(Int, Int)):Boolean = (x._1 >= y._1 && x._1 <= y._2) || (x._2 >= y._1 && x._2 <= y._2)
  }

  implicit class StringListExtras(s:Iterable[String]) {
    def toCountBag:Map[String, Double] = s.groupBy(identity).mapValues(_.size.toDouble)
  }

  implicit class WordBagExtras(m:Map[String, Double]) {
    def longest = m.keysIterator.toSeq.sortBy(_.length).lastOption.getOrElse("")
    def topWord = m.toSeq.sortBy(_._2).lastOption.map(_._1).getOrElse("")

    def topBag(w:Int) = m.toSeq.sortBy(-_._2).take(w)
    def topWords(w:Int) = topBag(w).map(_._1)

    def toBagOfWords = new BagOfWords(null, m)

  }

  implicit class TryExtras[A](t:scala.util.Try[A]) {
    def logError() = t match {
      case Success(_) => None
      case Failure(e) =>
        val bs = new ByteArrayOutputStream()
        val ps = new PrintStream(bs, true, "utf-8")
        e.printStackTrace(ps)
        Some(bs.toString("utf-8"))
    }
  }

  implicit class BufferedReaderExtras(rdr:BufferedReader) {

    /** Returns an iterator over the lines of the buffered reader's contents.
      * Consumes the reader and auto-closes. */
    def toIterator:Iterator[String] = new Iterator[String] {
      private var nextLine = rdr.readLine()

      def next() = {
        val res = nextLine
        nextLine = rdr.readLine()
        if(nextLine == null) {
          rdr.close()
        }
        res
      }

      def hasNext = nextLine != null
    }
  }

  object sDouble {
    def unapply(s:String) = s.toDoubleSafe
  }

  def assertStringEquals(expr:Any, str:String) = assert(expr.toString == str, "The string representation '" + expr.toString + "' does not match the expected value: '" + str +"'")
  def assertMinimalAccuracy(got:Double, goal:Double): Unit = assert(got >= goal, s"Accuracy ${got} is less than expected ${goal}.")
  def assertDoubleEquals(got:Double, goal:Double, epsilon:Double): Unit = assert(Math.abs(got-goal) < epsilon, s"Got: $got, expected: $goal")

  type DenseTensor1 = cc.factorie.la.DenseTensor1
  type DenseTensor2 = cc.factorie.la.DenseTensor2
  type DenseTensor3 = cc.factorie.la.DenseTensor3
  type DenseTensor4 = cc.factorie.la.DenseTensor4

  type Tensor1 = cc.factorie.la.Tensor1
  type Tensor2 = cc.factorie.la.Tensor2
  type Tensor3 = cc.factorie.la.Tensor3
  type Tensor4 = cc.factorie.la.Tensor4

  type Var = variable.Var

  type Assignment = variable.Assignment
  type HashMapAssignment = variable.HashMapAssignment

  type BooleanVariable = variable.BooleanVariable

  type Diff = variable.Diff
  type DiffList = variable.DiffList

  type DiscreteDomain = variable.DiscreteDomain

  type DiscreteVariable = variable.DiscreteVariable

  type HashFeatureVectorVariable = variable.HashFeatureVectorVariable

  type RealVariable = variable.RealVariable

  type Cubbie = util.Cubbie

  type GlobalLogging = util.GlobalLogging

  type Example = optimize.Example

  type Factor = model.Factor

  type Model = model.Model
  type CombinedModel = model.CombinedModel
  type TemplateModel = model.TemplateModel
  type Parameters = model.Parameters

  type GibbsSampler = infer.GibbsSampler
}
