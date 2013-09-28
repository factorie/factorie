package cc.factorie.app.nlp.hcoref
import cc.factorie._
import collection.mutable.{HashMap, ArrayBuffer}
import cc.factorie.variable.DiffList

object HierEntityUtils{
  val shortDecimal = new java.text.DecimalFormat("0.0#")
  def makeSingletons[E<:HierEntity](entities:Seq[E]):Seq[E] ={
    for(e <- entities)
      e.setParentEntity(null)(null)
    entities.filter(_.isObserved).toSeq
  }
  def collapseOn[E<:HierEntity](entities:Seq[E], collapser:E=>Option[String], newEntity:()=>E, propagater:E=>Unit):Seq[E] ={
    val result = new ArrayBuffer[E]
    result ++= makeSingletons(entities)
    val key2entities = new HashMap[String,ArrayBuffer[E]]
    for(e <- entities.filter(collapser(_) != None)){
      val rep = collapser(e).get
      key2entities.getOrElse(rep,{val r = new ArrayBuffer[E];key2entities(rep)=r;r}) += e
    }
    for((label,trueCluster) <- key2entities){
      if(trueCluster.size>1){
        val root = newEntity()
        result += root
        for(e<-trueCluster){
          e.setParentEntity(root)(null)
          for(bag <- e.attr.all[BagOfWordsVariable])root.attr(bag.getClass).add(bag.value)(null)
        }
        propagater(root)
      }
    }
    result
  }
  def createBagsForMergeUp(e1:Entity,e2:Entity,parent:Entity)(implicit d:DiffList):Unit ={
    for(bag <- e1.attr.all[BagOfWordsVariable])parent.attr(bag.getClass).add(bag.value)(d)
    for(bag <- e2.attr.all[BagOfWordsVariable])parent.attr(bag.getClass).add(bag.value)(d)
    parent.attr[MentionCountVariable].set(parent.attr[MentionCountVariable].value + e1.attr[MentionCountVariable].value)(d)
    parent.attr[MentionCountVariable].set(parent.attr[MentionCountVariable].value + e2.attr[MentionCountVariable].value)(d)
    //val evar = parent.attr[EditSetVariable]
    //e1.attr[EditSetVariable].value.foreach(evar.add(_)(d))
    //e2.attr[EditSetVariable].value.foreach(evar.add(_)(d))
  }
  def linkChildToParent(child:Entity,parent:Entity)(implicit d:DiffList):Unit ={
    if(child.parentEntity!=null)propagateRemoveBag(child,child.parentEntity)
    child.setParentEntity(parent)(d)
    if(parent!=null)propagateBagUp(child)(d)
  }
  def propagateBagUp(entity:Entity)(implicit d:DiffList):Unit ={
    var e = entity.parentEntity
    while(e!=null){
      //val evar = e.attr[EditSetVariable]
      //for(edit <- entity.attr[EditSetVariable])evar.add(edit)(d)
      e.attr[MentionCountVariable].set(e.attr[MentionCountVariable].value + entity.attr[MentionCountVariable].value)(d)
      for(bag <- entity.attr.all[BagOfWordsVariable])
        e.attr(bag.getClass).add(bag.value)(d)
      e=e.parentEntity
    }
  }
  def propagateRemoveBag(parting:Entity,formerParent:Entity)(implicit d:DiffList):Unit ={
    var e = formerParent
    while(e!=null){
      //val evar = e.attr[EditSetVariable]
      //parting.attr[EditSetVariable].value.foreach(evar.remove(_)(d))
      e.attr[MentionCountVariable].set(e.attr[MentionCountVariable].value - parting.attr[MentionCountVariable].value)(d)
      for(bag <- parting.attr.all[BagOfWordsVariable])
        e.attr(bag.getClass).remove(bag.value)(d)
      e=e.parentEntity
    }
  }
  def prettyPrint(entities:Seq[Entity],printSingletons:Boolean=false):Unit = {
    var count = 0
    var numSingletons = 0
    var singletons = new ArrayBuffer[Entity]
    var sizeDist = new HashMap[Int,Int]
    for(e <- entities.filter((e:Entity) => {e.isRoot && e.isConnected})){
      if(!e.isObserved)prettyPrint(e) else singletons += e
      var size = e.numLeaves
      sizeDist(size) = sizeDist.getOrElse(size,0) + 1
      count += 1
    }
    if(printSingletons){
      println("\n\n------SINGLETONS-----")
      println("Printing singletons")
      for(e <- singletons)prettyPrint(e)
    }
    println("\nEntity size distribution")
    val sorted = sizeDist.toList.sortBy(_._2).reverse
    println(sorted)
    println("\nPrinted " + count + " entities  ("+singletons.size+ " singletons).")
  }
  def prettyPrint(e:Entity):Unit ={
    val authorString = entityStringPretty(e,
      (e:Entity)=>{
        var result:String ="num-leaves:"+e.numLeaves+" id:"+e.id.toString+";name: "+e.string
        //if(e.asInstanceOf[Entity].groundTruth!=None)result="truth:"+e.asInstanceOf[Entity].groundTruth.get+";"+result
        //if(e.childEntitiesSize>1)result = "purity="+shortDecimal.format(purity(e))+";"+result
        result},
      Some(defaultFeaturesToPrint(_))
    )
    println(authorString)
  }
  def printEntities(entities:Seq[Entity],includeSingletons:Boolean=true,represent:Entity=>String=(e:Entity)=>"",context:Entity=>String=(e:Entity)=>""):Unit = {
    var count = 0
    for(e <- entities.filter((e:Entity) => {e.isRoot && e.isConnected})){
      if(!e.isObserved || includeSingletons)
        println(entityString(e,represent,context))
      count += 1
    }
    println("Printed " + count + " entities.")
  }
  def entityStringPretty(e:Entity,flatRepresent:Entity=>String,featuresToPrint:Option[Entity=>Seq[String]],perLevelIndent:String="   ",result:StringBuffer=new StringBuffer,depth:Int=0):String={
    val levelIndent = {var r="";for(i<-0 until depth)r+=perLevelIndent;r}
    result.append("\n"+levelIndent)
    if(e.isRoot){
      result.append("EntityRoot["+flatRepresent(e)+"]")
      if(featuresToPrint!=None)result.append("\n"+levelIndent+"| Features\n"+levelIndent+"|   ")
    }else if(e.isObserved){
      result.append("-Mention["+flatRepresent(e)+"]")
      if(featuresToPrint!=None)result.append("\n"+levelIndent+"|   ")
    }else{
      result.append("*SubEntity["+flatRepresent(e)+"]")
      if(e.childEntitiesSize==0)result.append("-SUBENTITY ERROR")//throw new Exception("ERROR SUB ENTITY IS EMPTY")
      if(featuresToPrint!=None)result.append("\n"+levelIndent+"| Features\n"+levelIndent+"|   ")
    }
    for(featuresToPrintFunc<-featuresToPrint)result.append(featuresToPrintFunc(e).mkString("\n"+levelIndent+"|   "))
    if(e.childEntitiesSize>0)result.append("\n"+levelIndent+" \\Children ("+e.childEntitiesSize+")")
    for(childEntity <- e.childEntitiesIterator)entityStringPretty(childEntity,flatRepresent,featuresToPrint,perLevelIndent,result,depth+1)
    result.toString
  }
  def printClusteringStats[T<:HierEntity](entities:Seq[T]):Unit = {
    var count = 0
    var numSingletons = 0
    var sizeDist = new HashMap[Int,Int]
    for(e <- entities.filter((e:T) => {e.isRoot && e.isConnected})){
      var size = e.numLeaves
      sizeDist(size) = sizeDist.getOrElse(size,0) + 1
      count += 1
    }
    println("\nEntity size distribution")
    val sorted = sizeDist.toList.sortBy(_._2).reverse
    println(sorted)
  }
  def entityString(e:Entity,represent:Entity=>String,context:Entity=>String):String = {
    if(e==null)return "null"
    val result = new StringBuffer
    entityString(e,result,0,represent,context)
    result.toString
  }
  protected def entityString(e:Entity, result:StringBuffer, depth:Int=0,represent:Entity=>String=(e:Entity)=>"", context:Entity=>String=(e:Entity)=>""):Unit = {
    for(i<-0 until depth)result.append("   ")
    //result.append(e.id+"-")
    if(e.isRoot){
      result.append("EntityRoot["+represent(e)+"]")
      //result.append("(exists?="+e.isConnected+" mention?="+e.isObserved+" #children:"+e.subEntitiesSize+")")
    }else if(e.isObserved){
      result.append("Mention["+represent(e)+"]")
      //result.append(" Title="+e.asInstanceOf[AuthorEntity].paper.title)
    }else{
      result.append("SubEntity["+represent(e)+"]")
      if(e.childEntitiesSize==0)result.append("-SUBENTITY ERROR")//throw new Exception("ERROR SUB ENTITY IS EMPTY")
      //if(e.subEntitySize==1)throw new Exception("ERROR SUB ENTITY HAS ONE CHILD)
    }
    result.append(context(e))
    //result.append("{"+bagToString(e.attr[BagOfCoAuthors].value)+"}")
    result.append("\n")
    for(childEntity <- e.childEntitiesIterator)entityString(childEntity,result,depth+1,represent,context)
  }
  def bagToString(bag:BagOfWords,k:Int=8):String = {
    val map = new HashMap[String,Double]
    for((k,v) <- bag.iterator)map += k -> v
    topk(map,k)
  }
  def topk(bag:HashMap[String,Double], k:Int=18) : String ={
    val result = new StringBuffer
    val sorted = bag.toList.sortBy(_._2).reverse.take(k)
    for(i<-0 until sorted.length){
      result.append(sorted(i)._1+"->"+shortDecimal.format(sorted(i)._2))
      if(i<sorted.length-1)
        result.append(", ")
    }
    result.toString
  }
  def defaultEntityContext(e:Entity):String ={
    val result = new StringBuffer
    for(bagVar <- e.attr.all[BagOfWordsVariable]){
      val bag = bagVar.value
      if(bag.size>0){
        val name = bag.getClass.toString.split("\\.").toSeq.takeRight(1)
        result.append("\n  "+name+"("+bag.size+")=["+this.bagToString(bag,8)+"]")
      }
    }
    result.toString
  }
  def defaultFeaturesToPrint(e:Entity):Seq[String] ={
    val result = new ArrayBuffer[String]
    for(bagVar <- e.attr.all[BagOfWordsVariable]){
      val bag = bagVar.value
      if(bag.size>0){
        val name = bagVar.getClass.getName.split("\\.").toSeq.takeRight(1)(0).replaceAll("[A-Za-z]+\\(","").replaceAll("\\)","")
        result += name+"("+bag.size+"): ["+this.bagToString(bag,8)+"]"
      }
    }
    result
  }
}