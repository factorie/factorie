package cc.factorie

import java.io._
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import la.Tensor
import collection.mutable
import java.nio.channels.{ReadableByteChannel, WritableByteChannel, Channels}
import java.nio.ByteBuffer

class StringMapCubbie[T](val m: mutable.Map[String,T]) extends Cubbie {
  var akeys : Seq[String] = null
  var avalues: Seq[T] = null
  setMap(new mutable.Map[String, Any] {
    override def update(key: String, value: Any): Unit = {
      if (key == "keys") {
        akeys = value.asInstanceOf[Traversable[String]].toSeq
      } else if (key == "values") {
        assert(akeys != null)
        avalues = value.asInstanceOf[Traversable[T]].toSeq
        for (i <- 0 until akeys.size) {
          m(akeys(i)) = avalues(i)
        }
      }
    }
    def += (kv: (String, Any)): this.type = { update(kv._1, kv._2); this }
    def -= (key: String): this.type = sys.error("Can't remove slots from cubbie map!")
    def get(key: String): Option[Any] = if (key == "keys") Some(m.keys.toTraversable) else if (key == "values") Some(m.values.toTraversable) else None
    def iterator: Iterator[(String, Any)] = Seq(("keys", get("keys").get), ("values", get("values").get)).iterator
  })
}

// Thought we needed these funky manifest matches to avoid erasure issues
// unfortunately we actually need to fall back to type testing because of bugs in 2.9's
// Manifest.<:< - once we switch to 2.10 we can do the more safe thing
class CubbieMaker[-C](make: C => Cubbie, typeTester: Any => Boolean)(implicit m1: Manifest[C]) {
  CubbieMaker.makers += this
//  def tryMake[T](toConvert: T)(implicit m2: Manifest[T]): Option[Cubbie] =
//    if (m2 <:< m1) Some(make(toConvert.asInstanceOf[C])) else None
  def tryMake[T](toConvert: T): Option[Cubbie] =
    if (typeTester(toConvert)) Some(make(toConvert.asInstanceOf[C])) else None
}

// WARNING: when defining new CubbieMakers, make sure to put more specific ones earlier in the list
object CubbieMaker {
  val makers = new mutable.ArrayBuffer[CubbieMaker[_]]
  // this trick lets us give compile errors if we don't have a CubbieMaker defined, but still use the most specific overriding
  // CubbieMaker instead of the least specific one (contravariance!!)
  // just define overriding CubbieMakers here earlier than the less specific versions and they will be found earlier in search through the "makers" list
  implicit val modm = new CubbieMaker[Model](
    new ModelCubbie(_), x => x.isInstanceOf[Model])
  implicit val cdm = new CubbieMaker[CategoricalDomain[String]](
    new CategoricalDomainCubbie(_), x => x.isInstanceOf[CategoricalDomain[String]])
  implicit val smm = new CubbieMaker[mutable.HashMap[String, String]](
    new StringMapCubbie(_), x => x.isInstanceOf[mutable.HashMap[String, String]])
  implicit val csdm = new CubbieMaker[CategoricalSeqDomain[String]](
    new CategoricalSeqDomainCubbie(_), x => x.isInstanceOf[CategoricalSeqDomain[String]])
  implicit val cdtdm = new CubbieMaker[CategoricalDimensionTensorDomain[_]](
    new CategoricalDimensionTensorDomainCubbie(_), x => x.isInstanceOf[CategoricalDimensionTensorDomain[_]])

  def cubbieForType[T](toSerialize: T)(implicit m: Manifest[T], evidenceThatWeHaveCubbieMaker: CubbieMaker[T]): Cubbie =
    makers.foldLeft(None: Option[Cubbie])((resOpt, maker) => if (resOpt.isDefined) resOpt else maker.tryMake(toSerialize)).get
}

object BinaryFileSerializer {
  import CubbieMaker._
  def serializeModel[A, B, C](model: A, labelDomain: B, featuresDomain: C, fileName: String, gzip: Boolean = false)
    (implicit m1: Manifest[A], ev1: CubbieMaker[A], m2: Manifest[B], ev2: CubbieMaker[B], m3: Manifest[C], ev3: CubbieMaker[C]): Unit = {
    BinaryCubbieFileSerializer.serialize(cubbieForType(model), new File(fileName + "-model"), gzip)
    BinaryCubbieFileSerializer.serialize(cubbieForType(labelDomain), new File(fileName + "-labelDomain"), gzip)
    BinaryCubbieFileSerializer.serialize(cubbieForType(featuresDomain), new File(fileName + "-featuresDomain"), gzip)
  }
  def deserializeModel[A, B, C](model: A, labelDomain: B, featuresDomain: C, fileName: String, gzip: Boolean = false)
    (implicit m1: Manifest[A], ev1: CubbieMaker[A], m2: Manifest[B], ev2: CubbieMaker[B], m3: Manifest[C], ev3: CubbieMaker[C]): Unit = {
    BinaryCubbieFileSerializer.deserialize(cubbieForType(labelDomain), new File(fileName + "-labelDomain"), gzip)
    BinaryCubbieFileSerializer.deserialize(cubbieForType(featuresDomain), new File(fileName + "-featuresDomain"), gzip)
    BinaryCubbieFileSerializer.deserialize(cubbieForType(model), new File(fileName + "-model"), gzip)
  }
  def serialize[A](toSerialize: A, fileName: String)(implicit m: Manifest[A], ev: CubbieMaker[A]) =
    BinaryCubbieFileSerializer.serialize(cubbieForType(toSerialize), new File(fileName), gzip = false)
  def deserialize[A](deserializeTo: A, fileName: String)(implicit m: Manifest[A], ev: CubbieMaker[A]) =
    BinaryCubbieFileSerializer.deserialize(cubbieForType(deserializeTo), new File(fileName), gzip = false)
  def serialize[A](toSerialize: A, file: File)(implicit m: Manifest[A], ev: CubbieMaker[A]) =
    BinaryCubbieFileSerializer.serialize(cubbieForType(toSerialize), file, gzip = false)
  def deserialize[A](deserializeTo: A, file: File)(implicit m: Manifest[A], ev: CubbieMaker[A]) =
    BinaryCubbieFileSerializer.deserialize(cubbieForType(deserializeTo), file, gzip = false)
  def serialize[A](toSerialize: A, fileName: String, gzip: Boolean)(implicit m: Manifest[A], ev: CubbieMaker[A]) =
    BinaryCubbieFileSerializer.serialize(cubbieForType(toSerialize), new File(fileName), gzip)
  def deserialize[A](deserializeTo: A, fileName: String, gzip: Boolean)(implicit m: Manifest[A], ev: CubbieMaker[A]) =
    BinaryCubbieFileSerializer.deserialize(cubbieForType(deserializeTo), new File(fileName), gzip)
  def serialize[A](toSerialize: A, file: File, gzip: Boolean)(implicit m: Manifest[A], ev: CubbieMaker[A]) =
    BinaryCubbieFileSerializer.serialize(cubbieForType(toSerialize), file, gzip)
  def deserialize[A](deserializeTo: A, file: File, gzip: Boolean)(implicit m: Manifest[A], ev: CubbieMaker[A]) =
    BinaryCubbieFileSerializer.deserialize(cubbieForType(deserializeTo), file, gzip)
  def serialize[A](toSerialize: A, s: DataOutputStream)(implicit m: Manifest[A], ev: CubbieMaker[A]) =
    BinaryCubbieFileSerializer.serialize(cubbieForType(toSerialize), s)
  def deserialize[A](toSerialize: A, s: DataInputStream)(implicit m: Manifest[A], ev: CubbieMaker[A]) =
    BinaryCubbieFileSerializer.deserialize(cubbieForType(toSerialize), s)
  def writeFile(fileName: String, gzip: Boolean = false): DataOutputStream = {
    val file = new File(fileName)
    file.createNewFile()
    val fileStream = new BufferedOutputStream(new FileOutputStream(file))
    new DataOutputStream(if (gzip) new BufferedOutputStream(new GZIPOutputStream(fileStream)) else fileStream)
  }
  def readFile(fileName: String, gzip: Boolean = false): DataInputStream = {
    val file = new File(fileName)
    val fileStream = new BufferedInputStream(new FileInputStream(file))
    new DataInputStream(if (gzip) new BufferedInputStream(new GZIPInputStream(fileStream)) else fileStream)
  }
}

object BinaryCubbieFileSerializer {
  def serialize(toSerialize: Cubbie, file: File, gzip: Boolean = false): Unit = {
    file.createNewFile()
    val fileStream = new BufferedOutputStream(new FileOutputStream(file))
    val s = new DataOutputStream(if (gzip) new BufferedOutputStream(new GZIPOutputStream(fileStream)) else fileStream)
    serialize(toSerialize, s)
    s.close()
  }

  def deserialize(deserializeTo: Cubbie, file: File, gzip: Boolean = false): Unit = {
    val fileStream = new BufferedInputStream(new FileInputStream(file))
    val s = new DataInputStream(if (gzip) new BufferedInputStream(new GZIPInputStream(fileStream)) else fileStream)
    deserialize(deserializeTo, s)
    s.close()
  }

  def serialize(c: Cubbie, s: DataOutputStream): Unit = {
    for ((k, v) <- c._map.toSeq) serialize(Some(k), v, s)
  }
  def deserialize(c: Cubbie, s: DataInputStream): Unit = {
    for ((k, v) <- c._map.toSeq) {
      val key = readString(s)
      assert(k == key, "Cubbie keys don't match with serialized data! (got \"%s\", expected \"%s\")" format (key, k))
      c._map(key) = deserializeInner(v, s.readByte(), s)
    }
  }

  private val INT: Byte = 0x01
  private val DOUBLE: Byte = 0x02
  private val BOOLEAN: Byte = 0x03
  private val STRING: Byte = 0x04
  private val TENSOR: Byte = 0x05
  private val LIST: Byte = 0x07
  private val MAP: Byte = 0x06

  private def deserializeInner(preexisting: Any, tag: Byte, s: DataInputStream): Any = tag match {
    case DOUBLE => s.readDouble()
    case INT => s.readInt()
    case BOOLEAN => s.readShort() != 0
    case STRING => readString(s)
    case TENSOR =>
      if (preexisting == null) sys.error("Require pre-existing tensor value in cubbie for general \"TENSOR\" slot.")
      val tensor = preexisting.asInstanceOf[Tensor]
//      def dump[T](x: T, title: String): T = { println(title + ": " + x); x }
//      repeat(dump(s.readInt(), "tensor length"))(tensor(dump(s.readInt(), "idx")) = dump(s.readDouble(), "value"))
      repeat(s.readInt())(tensor(s.readInt()) = s.readDouble())
      tensor
    case MAP =>
      val m = if (preexisting == null) new mutable.HashMap[String, Any] else preexisting.asInstanceOf[mutable.Map[String, Any]]
      repeat(s.readInt()) {
        val key = readString(s)
        m(key) = deserializeInner(if (m.contains(key)) m(key) else null, s.readByte(), s)
      }
      m
    case LIST =>
      val innerTag = s.readByte()
      val len = s.readInt()
      val buff =
        (if (innerTag == INT) new mutable.ArrayBuffer[Int]
        else if (innerTag == DOUBLE) new mutable.ArrayBuffer[Double]
        else new mutable.ArrayBuffer[Any]).asInstanceOf[mutable.ArrayBuffer[Any]]
      val iter = (if (preexisting == null) Seq[Any]() else preexisting.asInstanceOf[Traversable[Any]]).toIterator
      repeat(len) {
        val pre = if (iter.hasNext) iter.next() else null
        if (!isPrimitiveTag(innerTag)) s.readByte() // read and ignore the type tag
        buff += deserializeInner(pre, innerTag, s)
      }
      buff
  }
  private def readString(s: DataInputStream): String = {
    val bldr = new StringBuilder
    repeat(s.readInt())(bldr += s.readChar())
    bldr.mkString
  }
  private def writeString(str: String, s: DataOutputStream): Unit = {
    s.writeInt(str.length)
    str.foreach(s.writeChar(_))
  }
  private def tagForType(value: Any): Byte = value match {
    case _: Int => INT
    case _: Double => DOUBLE
    case _: Boolean => BOOLEAN
    case _: String => STRING
    case _: Tensor => TENSOR
    case _: mutable.Map[String, Any] => MAP
    case _: Traversable[_] => LIST
  }
  private def isPrimitiveTag(tag: Byte): Boolean = tag match {
    case DOUBLE | BOOLEAN | INT => true
    case _ => false
  }
  private def isPrimitive(value: Any): Boolean = isPrimitiveTag(tagForType(value))
  private def serialize(key: Option[String], value: Any, s: DataOutputStream): Unit = {
    key.foreach(writeString(_, s))
    if (key.isDefined || !isPrimitive(value)) s.writeByte(tagForType(value))
    value match {
      case i: Int => s.writeInt(i)
      case bl: Boolean => s.writeShort(if (bl) 0x01 else 0x00)
      case d: Double => s.writeDouble(d)
      case str: String => writeString(str, s)
      case t: Tensor =>
        s.writeInt(t.activeDomainSize)
        for ((i, v) <- t.activeElements) {
          s.writeInt(i)
          s.writeDouble(v)
        }
      case m: mutable.Map[String, Any] =>
        s.writeInt(m.size)
        for ((k, v) <- m) serialize(Some(k), v, s)
      case t: Traversable[Any] =>
        val tag = t.headOption.map(tagForType(_)).getOrElse(INT)
        s.writeByte(tag)
        s.writeInt(t.size)
        t.foreach(serialize(None, _, s))
    }
    s.flush()
  }
}