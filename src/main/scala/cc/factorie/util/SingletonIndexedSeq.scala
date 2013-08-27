package cc.factorie.util

/** An immutable indexed sequence containing a single element, with efficient implementations of head, last, foreach, iterator and map.
    @author Andrew McCallum */
case class SingletonIndexedSeq[A](elt:A) extends IndexedSeq[A] {
  def apply(i:Int): A = if (i == 0) elt else throw throw new IndexOutOfBoundsException(i.toString)
  def length: Int = 1
  override def head: A = elt
  override def last: A = elt
  override def foreach[U](f: (A)=>U): Unit = f(elt)
  override def iterator: Iterator[A] = Iterator.single(elt)
  def map[B](f: (A) => B): SingletonIndexedSeq[B] = new SingletonIndexedSeq(f(elt))
  // TODO Figure out how to override this properly. 
  //override def map[B, That](f: A => B)(implicit bf: scala.collection.generic.CanBuildFrom[IndexedSeq[A],B,That]): That = new SingletonIndexedSeq(f(elt))
}

//import scala.collection.generic.SeqFactory
//import scala.collection.generic.CanBuildFrom
//import scala.collection.mutable.ArrayBuffer
//object SingletonIndexedSeq extends SeqFactory[SingletonIndexedSeq] {
//  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, SingletonIndexedSeq[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
//  //def newBuilder[A]: Builder[A, ArrayBuffer[A]] = new ArrayBuffer[A]
//}
