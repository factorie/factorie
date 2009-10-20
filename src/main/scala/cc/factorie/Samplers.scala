package cc.factorie
import scala.collection.mutable.ArrayBuffer

class Samplers extends ArrayBuffer[VariableSampler] {
  
  def process(v:Variable) : DiffList = {
    val samplers = this.elements
    while (samplers.hasNext) {
      val sampler = samplers.next
      val d = sampler.process(v)
      if (d != null) return d
    }
    return null
  }
  
  def process(vs:Iterable[Variable]) : Unit = vs.foreach(process(_))
  
}
