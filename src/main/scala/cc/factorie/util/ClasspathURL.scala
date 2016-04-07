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
package cc.factorie.util
import scala.language.existentials
import scala.reflect.ClassTag

@deprecated("See cc.factorie.util.ModelProvider", "10/05/15")
object ClasspathURL {

  def fromDirectory[C](suffix:String)(implicit m: ClassTag[C]): java.net.URL = {
    Option(System.getProperty(m.runtimeClass.getName)) match {
      case Some(url) =>
        try { new java.net.URL(url + "/"+ suffix) }
        catch {
          case t: java.net.MalformedURLException => throw new Error(s"System property ${m.runtimeClass.getName} contains malformed url ${url+suffix}. Either fix the URL or unset the system property to open a file from the classpath.", t)
        }
      case None =>
        m.runtimeClass.getResource(suffix) match {
          case null => throw new Error(s"No file named $suffix found in classpath for class ${m.runtimeClass.getName}, and no value found in system property ${m.runtimeClass.getName}. To fix this either add a file with the right name to the classpath or set the system property to point to a directory containing the file.")
          case a: java.net.URL => a
        }

    }
  }
  def apply[C](suffix:String)(implicit m: ClassTag[C]): java.net.URL = {
    Option(System.getProperty(m.runtimeClass.getName)) match {
      case Some(url) => try { new java.net.URL(url) }
        catch {
          case t: java.net.MalformedURLException => throw new Error(s"System property ${m.runtimeClass.getName} contains malformed url ${url+suffix}. Either fix the URL or unset the system property to open a file from the classpath.", t)
        }
      case None => m.runtimeClass.getResource(m.runtimeClass.getSimpleName+suffix) match {
        case null => throw new Error(s"No file named ${m.runtimeClass.getSimpleName + suffix} found in classpath for class ${m.runtimeClass.getName}, and no value found in system property ${m.runtimeClass.getName}. To fix this either add a file with the right name to the classpath or set the system property to point to a directory containing the file.")
        case a: java.net.URL => a
      }
    }
  }
}
