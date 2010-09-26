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

package cc.factorie.app
import java.io.InputStream

package object strings {

  def inputStreamToString(is:InputStream, encoding:String = "UTF-8"): String = {
    import java.io.InputStreamReader
    val buffer = new Array[Char](0x10000)
    val out = new StringBuilder()
    val in = new InputStreamReader(is, encoding)
    var read = 0
    do {
      read = in.read(buffer, 0, buffer.length)
      if (read > 0)
        out.appendAll(buffer, 0, read)
    } while (read>=0)
    out.toString
  }

}
