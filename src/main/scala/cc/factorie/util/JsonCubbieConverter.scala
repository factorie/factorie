package cc.factorie.util

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import scala.collection.mutable

/**
 * @author John Sullivan
 */
class JsonCubbieConverter {

  def toJson(c:Cubbie):JObject = {
    def toJsonImpl(a:Any):JValue = a match {
      case m:mutable.Map[String,_] =>
        m.toMap.map{case (k,v) => k -> toJsonImpl(v)}
      case it:Iterable[_] =>
        it.map(toJsonImpl)
      case i:Int => i
      case l:Long => l
      case d:Double => d
      case f:Float => f
      case b:Boolean => b
      case s:String => s
    }
    toJsonImpl(c._map).asInstanceOf[JObject]
  }

  def toCubbie[C <: Cubbie](jObj:JObject, con:() => C):C = {
    val c = con()
    def toCubbieImpl(j:JValue):Any = j match {
      case JObject(fields) =>
        val m = new mutable.HashMap[String,Any]()
        fields foreach { case JField(name, value) =>
          m += (name, toCubbieImpl(value))
        }
        m
      case JArray(vals) => vals map toCubbieImpl
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
    }
    c._map = toCubbieImpl(jObj).asInstanceOf[mutable.Map[String, Any]]
    c
  }
}
