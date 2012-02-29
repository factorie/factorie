package cc.factorie.example

import cc.factorie._
import cc.factorie.app.nlp.coref._


object Coref3 {
  
  class Bow(val entity:Entity) extends SetVariable[String] {
    def this(e:Entity, ss:Iterable[String]) = { this(e); ss.foreach(this.add(_)(null)) }
  }
  
  class MyMention(n:String, b:Iterable[String]) extends StringMention(n) {
    attr += new Bow(this, b)
  }
  class MyMentionCubbie extends StringEntityCubbie {
    val bow = StringListSlot("bow")
  }
  
  var entityCount = 0
  class MyEntityVariable(s:String) extends EntityVariable(s) {
    override val id = { entityCount += 1; entityCount }
    attr += new Bow(this)
  }
  
  object HierCorefModel extends TemplateModel(
    // compatibility between string names, entityRef, 
    new TemplateWithStatistics3[EntityRef,EntityName,EntityName] {
      def unroll1(er:EntityRef) = Factor(er, er.src.attr[EntityName], er.dst.attr[EntityName])
      def unroll2(childName:EntityName) = Factor(childName.entity.superEntityRef, childName, childName.entity.superEntity.attr[EntityName])
      def unroll3(parentName:EntityName) = for (e <- parentName.entity.subEntities) yield Factor(e.superEntityRef, e.attr[EntityName], parentName)
      def score(s:Stat): Double = {
        val childString = s._2
        val parentString = s._3
        - cc.factorie.app.strings.editDistance(childString, parentString)
      }
    },
    // compatibility between bows
    new TemplateWithStatistics3[EntityRef,Bow,Bow] {
      def unroll1(er:EntityRef) = Factor(er, er.src.attr[Bow], er.dst.attr[Bow])
      def unroll2(childBow:Bow) = Factor(childBow.entity.superEntityRef, childBow, childBow.entity.superEntity.attr[Bow])
      def unroll3(parentBow:Bow) = for (e <- parentBow.entity.subEntities) yield Factor(e.superEntityRef, e.attr[Bow], parentBow)
      def score(s:Stat): Double = {
        val childBow = s._2
        val parentBow = s._3
        childBow.intersect(parentBow).size
      }
    }
  )
  
  object HierarchicalCorefSampler extends SettingsSampler[Null](HierCorefModel, null) {
    val entityVar:MyEntityVariable = null
    var entities:Seq[MyEntityVariable] = null
    def infer(numSamples:Int):Unit ={
	  
    }
    def nextEntity= entities.sampleUniformly(random)
    
    def settings(c:Null) : SettingIterator = new SettingIterator {
      def hasNext: Boolean = throw new Error
      def reset: Unit = throw new Error
      def next(d:DiffList): DiffList = throw new Error
      val changes = new scala.collection.mutable.ArrayBuffer[(DiffList)=>Unit];
      changes += {(d:DiffList) => {}}

      val entity1 = nextEntity
      val entity2 = nextEntity
      if (entity1.entityRoot.id != entity2.entityRoot.id) {
        changes += {(d:DiffList) => entity1.setSuperEntity(entity2)(d)} //what if entity2 is a mention?
        changes += {(d:DiffList) => entity1.setSuperEntity(entity2.entityRoot)(d)}
        changes += {(d:DiffList) => entity2.setSuperEntity(entity1.entityRoot)(d)}
        changes += {(d:DiffList) => entity2.setSuperEntity(entity1)(d)}
        changes += {(d:DiffList) => }
      } else {
        changes += {(d:DiffList) => entity1.setSuperEntity(null)(d)}
        changes += {(d:DiffList) => entity2.setSuperEntity(null)(d)}
      }
    }
  }
  
  
  
  def main(args:Array[String]): Unit = {
    val mentions = for (t <- data) yield new MyMention(t._1, t._2)
    // priority queue
    // get next n entities from db, and their canopy
    // how much of tree substructure to retrieve
  }
  
  val data = List(
    ("Andrew McCallum", List("nips", "icml", "acl")),
    ("Andrew MacCallum", List("acl", "emnlp")),
    ("Angrew McCallum", List("emnlp", "kdd")),
    ("McCallum", List("kdd")),
    ("A. McCallum", List("uai")),
    ("Michael Wick", List("kdd", "uai")),
    ("Mike Wick", List("kdd", "nips")),
    ("Michael Andrew Wick", List("icml", "nips")),
    ("Wick", List("siam", "kdd")),
    ("Wick", List("uai"))
  )
  

}