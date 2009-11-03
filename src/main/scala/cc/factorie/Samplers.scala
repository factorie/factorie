package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.ArrayBuffer


class GenericSampler[C](sampler:Sampler[C])(implicit mc:Manifest[C]) extends Sampler[C] {
	val contextClass = mc.erasure
  /** If argument is the right type, then call process method. */
  def process0[T<:AnyRef](context:T): DiffList = if (contextClass.isAssignableFrom(context.getClass)) process(context.asInstanceOf[C]) else null
  def process1(context:C) = sampler.process1(context)
}

// TODO Consider using a similar trick to avoid the need for .init in Template with Statistics!!!
object Samplers {
  implicit def sampler2GenericSampler[C](s:Sampler[C])(implicit mc:Manifest[C]) = new GenericSampler[C](s)(mc)
}

class Samplers(ss:Iterable[Sampler[_]]) extends ArrayBuffer[Sampler[_]] {
  def this() = this(Nil)
  def this(ss:Sampler[_]*) = this(ss)
  this ++= ss
  
  def process(context:AnyRef) : DiffList = {
    val samplers = this.elements
    while (samplers.hasNext) {
      val sampler = samplers.next
      throw new Error("Not yet implemented")
      val d = new DiffList // sampler.process0(context)
      if (d != null) return d
    }
    return null
  }
  
  def process(vs:Iterable[AnyRef]) : Unit = vs.foreach(process(_))
  
}
