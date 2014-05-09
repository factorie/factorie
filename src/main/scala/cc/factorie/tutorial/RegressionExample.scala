/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
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
package cc.factorie.tutorial

import java.io.File

import collection.mutable
import io.Source

import cc.factorie.app.regress.LinearRegressionTrainer
import cc.factorie.la.{DenseTensor1, Tensor1}
import cc.factorie.variable.{TensorVariable, BinaryFeatureVectorVariable, DiffList, CategoricalVectorDomain}

/**
 * An example of Linear Regression.  Tries to predict the hash value
 */
object RegressionExample {

  // input features
  object InputDomain extends CategoricalVectorDomain[String]
  class Input(file: File) extends BinaryFeatureVectorVariable[String] {
    def domain = InputDomain

    { // add all words in document to vector
      val text = Source.fromFile(file, "ISO-8859-1").mkString
      val words = """\w+""".r.findAllIn(text.trim)
      words.foreach{ word => this += word }
    }
  }

  class Output(val input: Input, val label: Double)(implicit d: DiffList = null) extends TensorVariable[Tensor1] {
    set(new DenseTensor1(1))
    value(0) = label
  }

  def main(args: Array[String]): Unit = {
    require(args.length == 2, "Usage: scala cc.factorie.tutorial.RegressionExample folder1/ folder2/")

    /** Load documents **/
    var outputs = mutable.ArrayBuffer[Output]()
    for ((directory, i) <- args.zipWithIndex) {
      for (file <- new File(directory).listFiles; if file.isFile) {
        outputs += new Output(new Input(file), (2 * i - 1) + math.random * 0.001)
      }
    }

    /** Run regression **/
    val regressor = LinearRegressionTrainer.train[Input, Output](outputs, {f => f.input}, 0.0)
  }

}
