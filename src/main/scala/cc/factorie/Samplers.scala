package cc.factorie
import scala.collection.mutable.ArrayBuffer

class Samplers extends ArrayBuffer[Sampler[_]] {
  
  def process(context:AnyRef) : DiffList = {
    val samplers = this.elements
    while (samplers.hasNext) {
      val sampler = samplers.next
      val d = sampler.process0(context)
      if (d != null) return d
    }
    return null
  }
  
  def process(vs:Iterable[AnyRef]) : Unit = vs.foreach(process(_))
  
}
