package cc.factorie.util

import org.json4s._
import org.json4s.JsonDSL._
import scala.collection.mutable

/**
 * @author John Sullivan
 */
object JsonCubbieConverter {

  def toJson(c:Cubbie):JObject = {
    def toJsonImpl(a:Any):JValue = a match {
      case null => JNull
      case is:IntSeq => is._rawArray.map(toJsonImpl).toIterable  // we can do something cleverer here if we want
      case ds:DoubleSeq => ds._rawArray.map(toJsonImpl).toIterable
      case m:mutable.Map[_,_] =>
        m.toMap.map{case (k,v) => println((k,v));k.toString -> toJsonImpl(v)}
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
      case JArray(vals) => if(vals.forall(_.isInstanceOf[JInt])) { // we assume this was an IntSeq
        new ArrayIntSeq(vals.map{case JInt(i) => i.toInt}.toArray)
      } else if(vals.forall(_.isInstanceOf[JDouble])) {
        new ArrayDoubleSeq(vals.map{case JDouble(d) => d.toDouble}.toArray)
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
