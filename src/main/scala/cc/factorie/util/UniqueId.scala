package cc.factorie.util


/** An object that has a unique identifier that is consistent across serialization. 
    @author Andrew McCallum */
trait UniqueId[+A] {
  def uniqueId: A
}

//case class UniqueId[A](string:String)
