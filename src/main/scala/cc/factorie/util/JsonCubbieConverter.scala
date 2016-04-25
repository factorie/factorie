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

import org.json4s.JsonDSL._
import org.json4s._

import scala.collection.mutable

/**
 * Methods for serializing and de-serializing cubbies to and from json. 
 * Important Note: (As of May 28, 2015) Serialization/de-serialization of general list structures of ints and doubles
 * (Seq[Int], Seq[Double], List[Int], List[Double], Array[Int], Array[Double], etc) is not supported. Instead, the
 * factorie optimized versions of these data structures, IntSeq and DoubleSeq must be used. *
 * @author John Sullivan
 */
object JsonCubbieConverter {

  def toJson(c:Cubbie):JObject = {
    def toJsonImpl(a:Any):JValue = a match {
      case null => JNull
      case is:IntSeq => is._rawArray.slice(0,is.length).map(toJsonImpl).toIterable  // we can do something cleverer here if we want
      case ds:DoubleSeq => ds._rawArray.slice(0,ds.length).map(toJsonImpl).toIterable
      case m:mutable.Map[_,_] =>
        m.toMap.map{case (k,v) => k.toString -> toJsonImpl(v)}
      case it:Iterable[_] =>
        it.map(toJsonImpl)
      case i:Int => i
      case l:Long => l
      case d:Double => d
      case f:Float => f
      case b:Boolean => b
      case s:String => s
      case d:java.util.Date => d.toString
      case Some(an) => toJsonImpl(an)
      case None => JNothing
    }
    toJsonImpl(c._map).asInstanceOf[JObject]
  }

  def toCubbie[C <: Cubbie](jObj:JObject, con:() => C):C = {
    val c = con()
    def toCubbieImpl(j:JValue):Any = j match {
      case JObject(fields) =>
        val m = new mutable.HashMap[String,Any]()
        fields foreach { case JField(name, value) =>
          m += name -> toCubbieImpl(value)
        }
        m
      case JArray(vals) if vals.isEmpty => List.empty[Any] // Handle empty list
      case JArray(vals) => if(vals.forall(_.isInstanceOf[JInt])) { // we assume this was an IntSeq
        new ArrayIntSeq(vals.collect{case JInt(i) => i.toInt}.toArray)
      } else if(vals.forall(_.isInstanceOf[JDouble])) {
        new ArrayDoubleSeq(vals.collect{case JDouble(d) => d.toDouble}.toArray)
      } else {
        vals map toCubbieImpl
      }
      case JInt(i) => if(i.isValidInt) {
        i.toInt
      } else if(i.isValidLong) {
        i.toLong
      } else {
        i
      }
      case JDouble(d) => d
      case JBool(b) => b
      case JString(s) => s
      case JDecimal(d) => if(d.isDecimalDouble){
        d.toDouble
      } else {
        d
      }
      case JNothing => None
      case JNull => null
    }
    c._map = toCubbieImpl(jObj).asInstanceOf[mutable.Map[String, Any]]
    c
  }
}
