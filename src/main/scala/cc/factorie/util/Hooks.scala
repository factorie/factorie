package cc.factorie.util
import scala.collection.mutable.ArrayBuffer

class Hooks0 extends ArrayBuffer[()=>Unit] {
  def apply : Unit = this.foreach(_.apply)
}
class Hooks1[A] extends ArrayBuffer[A=>Unit] {
  def apply(a:A) : Unit = this.foreach(_.apply(a))
}
class Hooks2[A,B] extends ArrayBuffer[(A,B)=>Unit] {
  def apply(a:A,b:B) : Unit = this.foreach(_.apply(a,b))
}
class Hooks3[A,B,C] extends ArrayBuffer[(A,B,C)=>Unit] {
  def apply(a:A,b:B,c:C) : Unit = this.foreach(_.apply(a,b,c))
}


