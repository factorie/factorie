package cc.factorie.util


/** An object that has a unique identifier of type A that is consistent across serialization. 
    @author Andrew McCallum */
trait TypedUniqueId[+A] {
  def uniqueId: A
}

/** An object that has a unique identifier of type String that is consistent across serialization. 
    @author Andrew McCallum */
trait UniqueId extends TypedUniqueId[String]


