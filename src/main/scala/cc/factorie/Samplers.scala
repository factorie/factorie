package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.{ArrayBuffer,HashMap}


class GenericSampler[C](val sampler:Sampler[C])(implicit mc:Manifest[C]) extends Sampler[C] with cc.factorie.util.Trackable {
  //println("GenericSampler m="+mc)
	val contextClass = mc.erasure
	val contextManifest = mc
  /** If argument is the right type, then call process method. */
  val contextClassCache = new HashMap[Class[_],Boolean]
  def compatible(c:Class[_]): Boolean = {
    //mc.erasure.isAssignableFrom(c)  // This takes 44.4 seconds for LDADemo
    contextClassCache.getOrElseUpdate(c, contextManifest >:> Manifest.classType(c)) // This takes 42.8 seconds for LDADemo
    // No caching Manifest comparison with >:> took 468 seconds.  Wow!
  }
  def process0[T<:AnyRef](context:T): DiffList = 
    if (compatible(context.getClass)) {
      val c:C = context.asInstanceOf[C] // TODO How slow is this check?
      //|**("GenericSampler.process")
      val d = process(c) 
      //**|
      d
    } else 
      null
  def process1(context:C) = sampler.process1(context)
}


class SamplerSuite extends ArrayBuffer[GenericSampler[_]] with Sampler[AnyRef] with cc.factorie.util.Trackable {
  /*def this() = this(Nil)
  def this(ss:Sampler[_]*) = this(ss)
  this ++= ss*/
  
  def process1(context:AnyRef) : DiffList = {
    val samplers = this.elements
    while (samplers.hasNext) {
    	//|**("SamplerSuite")
      val sampler = samplers.next
      //println("SamplerSuite context "+context+" sampler "+sampler.sampler)
      val d:DiffList = sampler.process0(context)
      //**|
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
