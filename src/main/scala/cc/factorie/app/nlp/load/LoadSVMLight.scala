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

package cc.factorie.app.nlp.load

import java.io._
import java.util.zip.GZIPInputStream

import cc.factorie.la.SparseIndexedTensor1
import cc.factorie.variable.HashFeatureVectorVariable

import scala.collection.mutable.ArrayBuffer

object LoadSVMLight {
    private val svmLightSeparatorRegex = """\s|:""".r
    def loadSVMLight[T](file: String, hashDomainSize: Int): Seq[(SparseIndexedTensor1, Int)] = {
      val source = readFile(new java.io.File(file), gzip = true)
      val instances = ArrayBuffer[(SparseIndexedTensor1, Int)]()
      var lineNum = 0
      var line = null: String
      while ( {line = source.readLine(); line != null /*&& lineNum < 1000*/}) {
        lineNum += 1
        val fields = svmLightSeparatorRegex.split(line)
        val label = fields(0)
        val instance = new SparseIndexedTensor1(hashDomainSize)
        var i = 1
        val len = fields.length
        while (i < len) {
          val idx = fields(i).toInt
          val value = fields(i + 1).toDouble
          val bits = idx
          val hashIdx = HashFeatureVectorVariable.index(bits, hashDomainSize)
          val hashSign = HashFeatureVectorVariable.sign(bits)
          instance +=(hashIdx, value * hashSign)
          i += 2
        }
        instance._makeReadable()
        instances += ((instance, if (label.toInt == -1) 0 else 1))
      }
      instances
    }
    def readFile(file: File, gzip: Boolean = false): BufferedReader = {
      val fileStream = new BufferedInputStream(new FileInputStream(file))
      new BufferedReader(new InputStreamReader(if (gzip) new GZIPInputStream(fileStream) else fileStream))
    }
}