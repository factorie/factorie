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
package cc.factorie.app.nlp.hcoref

import cc.factorie.db.mongo.MongoCubbieImplicits._
import cc.factorie.db.mongo.{LazyCubbieConverter, MongoCubbieCollection, MutableCubbieCollection}
import cc.factorie.util.{ArrayDoubleSeq, Cubbie}
import cc.factorie.variable.{BagOfWordsVariable, DenseDoubleBagVariable, Var}
import com.mongodb.DB

import scala.collection.mutable
import scala.reflect.ClassTag

/**
 * User: escher
 * Date: 11/2/13
 */
trait NodeCollection[Vars <: NodeVariables[Vars]] {
  type N = Node[Vars]
  def +=(n :N)                       :Unit
  def ++=(es:Iterable[N])            :Unit
  def drop()                         :Unit
  def store(nodesToStore:Iterable[N]):Unit
  def nextBatch(n:Int=10)            :Seq[N]
  def loadAll                        :Seq[N]
}

class BOWCubbie extends Cubbie{
  val nodeId = RefSlot[NodeCubbie[_]]("nid",()=>null.asInstanceOf[NodeCubbie[_]])  //todo understand how this null works
  val word   = StringSlot("w")
  val count  = DoubleSlot("c")
  def fetch = this.word -> this.count
  def store(id:String, w:String, c:Double) = {
    nodeId := id
    word   := w
    count  := c
    this
  }
}

class DenseArrayCubbie extends Cubbie {
  val nodeId = RefSlot[NodeCubbie[_]]("nid",()=>null.asInstanceOf[NodeCubbie[_]])  //todo understand how this null works
  val arr = DoubleSeqSlot("a")
  def fetch = this.arr
  def store(id:String, arr:Array[Double]) = {
    nodeId := id
    this.arr := new ArrayDoubleSeq(arr)
    this
  }
}

trait DBNodeCollection[Vars <: NodeVariables[Vars], NC <: NodeCubbie[Vars]] extends NodeCollection[Vars]{
  protected val _id2cubbie = mutable.HashMap[String,NC]()
  protected def newNodeCubbie :NC
  protected def newNode(v:Vars, nc:NC) = if(nc.isMention.value) {
    nc.newMention(v, nc.id.toString)
  } else {
    nc.newNode(v, nc.id.toString)
  }
  protected def cubbify(n:N) = {val nc = newNodeCubbie; nc.store(n); nc}
  protected def nodeCubbieColl :MutableCubbieCollection[NC]
  def += (n:N){ insert(n) }
  def ++=(ns:Iterable[N]){ insert(ns) }
  def insert(c:NC) { nodeCubbieColl += c }
  def insert(n:N) {  nodeCubbieColl += cubbify(n) }
  def insert(ns:Iterable[N]) { nodeCubbieColl ++= ns.map(cubbify) }
  def drop:Unit
  def store(nodesToStore:Iterable[N]) {
    val (created, others) = nodesToStore.partition(n => n.exists && !n.loadedFromDb)
    nodeCubbieColl ++= created.map(cubbify)
    for(node <- others){
      if(!node.exists){
        nodeCubbieColl.remove(_.idIs(node.uniqueId))
      }
      else {
        nodeCubbieColl.updateDelta(_id2cubbie(node.uniqueId),cubbify(node))
      }
    }
  }

  def assembleNodes(toAssemble:Seq[N], node2ParentId: Map[N, String], id2Node:Map[String, N]) {

    def assembleHelper(n:N) {
      if(!n.getParent.isDefined && node2ParentId.isDefinedAt(n)) {
        val parent = id2Node(node2ParentId(n))
        n.alterParent(Some(parent))(null)
        assembleHelper(parent)
      }
    }

    val mentions = toAssemble.filter(_.isMention)
    for(m <- mentions){
      assembleHelper(m)
    }
  }

}

abstract class MongoNodeCollection[Vars <: NodeVariables[Vars], NC <: NodeCubbie[Vars]](val bagNames:Seq[String], val arrayNames:Seq[String], mongoDB:DB)(implicit ct: ClassTag[Vars]) extends DBNodeCollection[Vars, NC]{
  val numBags = ct.runtimeClass.getDeclaredFields.count(_.getType.getName.endsWith("BagOfWordsVariable")) -1
  //assert(bagNames.size == numBags+1, "Insufficient bag of words collection names : "+numBags+1+"<"+bagNames.size)
  val numArrays = ct.runtimeClass.getDeclaredFields.count(_.getType.getName.endsWith("DenseDoubleBagVarable"))
  //assert(arrayNames.size == numArrays, "Insufficient dense collection names : "+numArrays+"<"+bagNames.size)
  protected val colls = bagNames.map(mongoDB.getCollection)
  val nodeCubbieColl = new MongoCubbieCollection[NC](colls(0),() => newNodeCubbie,(a:NC) => Seq(Seq(a.parentRef))) with LazyCubbieConverter[NC]
  val varsCubbieColls = colls.tail.map(coll => new MongoCubbieCollection(coll,() => newBOWCubbie,(a:BOWCubbie) => Seq(Seq(a.nodeId))) with LazyCubbieConverter[BOWCubbie])
  val denseCubbieColls = arrayNames.map{ arrName =>
    new MongoCubbieCollection(mongoDB.getCollection(arrName),() => newDenseCubbie, (a:DenseArrayCubbie) => Seq(Seq(a.nodeId))) with LazyCubbieConverter[DenseArrayCubbie]
  }

  def drop: Unit = ???

  def nextBatch(n: Int): Seq[N] = ???

  def getTruth(nc:NC):String

  override def += (n:N){
    var bowIdx = 1
    var arrIdx = 0
    for(v <- n.variables.getVariables){
      v match {
        case bow:BagOfWordsVariable =>
          varsCubbieColls(bowIdx) ++= cubbifyBOW(n.uniqueId, bow)
          bowIdx+=1
        case arr:DenseDoubleBagVariable =>
          denseCubbieColls(arrIdx) += new DenseArrayCubbie().store(n.uniqueId, arr.value)
          arrIdx += 1
        case _ => println("can't cubbify this type")
      }
    }
    insert(n)
  }

  def cubbifyBOW(nodeId:String, bow:BagOfWordsVariable) = bow.value.asHashMap.map{
    case (w,d) => newBOWCubbie.store(nodeId, w, d)
  }

  def loadAll: Seq[N] = {
    val node2ParentId = mutable.HashMap[N, String]()
    val id2Node       = mutable.HashMap[String, N]()
    val nodes =
      for(nc <- nodeCubbieColl.toSeq) yield {
        _id2cubbie += nc.id.toString -> nc
        val bowVars = varsCubbieColls.map(coll => {
          val it = coll.findByAttribute("bid",Seq(nc.id))
          val bag = new BagOfWordsVariable
          for(b <- it){
            bag += (b.word.value, b.count.value)
          }
          bag
        })
        val arrVars = denseCubbieColls.flatMap{ coll => //todo check that this is correct
           coll.findByAttribute("bid", Seq(nc.id)).map{cub =>
            val arr = cub.arr.value.asArray
            val denseVar = new DenseDoubleBagVariable(arr.length)
            denseVar.set(arr)(null)
            denseVar
          }
        }
        val v = newNodeVars(getTruth(nc), (bowVars ++ arrVars):_*)
        val n = newNode(v,nc)
        id2Node += nc.id.toString -> n
        if(nc.parentRef.isDefined){
          node2ParentId += n -> nc.parentRef.value.toString
        }
        n
      }
    assembleNodes(nodes, node2ParentId.toMap, id2Node.toMap)
    nodes
  }

  //  def loadByIds(ids: Seq[String]): Seq[N] = {
  //    for(nc <- nodeCubbieColl.findByIds(ids)) yield {
  //      val vars = mutable.ArrayBuffer[Var]()
  //      for(coll <- varsCubbieColls){
  //        val it = coll.findByAttribute("nid",Seq(nc.id))
  //        //todo create a var with the cubbies
  //      }
  //      val v = newNodeVars(vars:_*)
  //      val n = newNode(v,n)
  //      n
  //    }
  //  }

  protected def newBOWCubbie  = new BOWCubbie()
  protected def newDenseCubbie = new DenseArrayCubbie()

  protected def newNodeVars[V <: Var](truth:String, vars: V*) : Vars
}

