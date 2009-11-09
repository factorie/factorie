package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.ArrayBuffer


class GenericSampler[C](val sampler:Sampler[C])(implicit mc:Manifest[C]) extends Sampler[C] {
  //println("GenericSampler m="+mc)
	val contextClass = mc.erasure
	val contextManifest = mc
  /** If argument is the right type, then call process method. */
  def process0[T<:AnyRef](context:T): DiffList = 
    if (contextManifest >:> Manifest.classType(context.getClass)) 
      process(context.asInstanceOf[C]) 
    else 
      null
  def process1(context:C) = sampler.process1(context)
}


class SamplerSuite extends ArrayBuffer[GenericSampler[_]] with Sampler[AnyRef] {
  /*def this() = this(Nil)
  def this(ss:Sampler[_]*) = this(ss)
  this ++= ss*/
  
  def process1(context:AnyRef) : DiffList = {
    val samplers = this.elements
    while (samplers.hasNext) {
      val sampler = samplers.next
      //println("SamplerSuite context "+context+" sampler "+sampler.sampler)
      val d:DiffList = sampler.process0(context)
      if (d != null) {
      	//println("SamplerSuite sampler "+sampler.sampler+" diff "+d)
        return d
      }
    }
    return null
  }
  
  override def noDiffList: this.type = {
    this.foreach(_.sampler.noDiffList)
    super.noDiffList
  }
  
}
