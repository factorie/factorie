package cc.factorie

import java.io._
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import la.Tensor
import collection.mutable
import java.nio.channels.{ReadableByteChannel, WritableByteChannel, Channels}
import java.nio.ByteBuffer

// We have these in a trait so we can mix them into the package object and make them available by default
trait CubbieConversions {
  implicit def modm(m: Model): Cubbie = new ModelCubbie(m)
  implicit def cdm(m: CategoricalDomain[_]): Cubbie = new CategoricalDomainCubbie(m)
  implicit def smm(m: mutable.HashMap[String, String]): Cubbie = new StringMapCubbie(m)
  implicit def csdm(m: CategoricalSeqDomain[_]): Cubbie = new CategoricalSeqDomainCubbie(m)
  implicit def cdtdm(m: CategoricalDimensionTensorDomain[_]): Cubbie = new CategoricalDimensionTensorDomainCubbie(m)
}

// You can import this object to gain access to the default cubbie conversions
object CubbieConversions extends CubbieConversions

object BinarySerializer {
  private def getLazyCubbieSeq(vals: Seq[() => Cubbie]): Seq[Cubbie] = vals.view.map(_())
  // We lazily create the cubbies because, for example, model cubbies will force their model's weights lazy vals
  // so we need to control the order of cubbie creation and serialization (domains are deserialized before model cubbies are even created)
  def serialize(c1: => Cubbie, file: File, gzip: Boolean): Unit =
    serialize(getLazyCubbieSeq(Seq(() => c1)), file, gzip)
  def serialize(c1: => Cubbie, c2: => Cubbie, file: File, gzip: Boolean): Unit =
    serialize(getLazyCubbieSeq(Seq(() => c1, () => c2)), file, gzip)
  def serialize(c1: => Cubbie, c2: => Cubbie, c3: => Cubbie, file: File, gzip: Boolean): Unit =
    serialize(getLazyCubbieSeq(Seq(() => c1, () => c2, () => c3)), file, gzip)
  def serialize(c1: => Cubbie, c2: => Cubbie, c3: => Cubbie, c4: => Cubbie, file: File, gzip: Boolean): Unit =
    serialize(getLazyCubbieSeq(Seq(() => c1, () => c2, () => c3, () => c4)), file, gzip)

  def deserialize(c1: => Cubbie, file: File, gzip: Boolean): Unit =
    deserialize(getLazyCubbieSeq(Seq(() => c1)), file, gzip)
  def deserialize(c1: => Cubbie, c2: => Cubbie, file: File, gzip: Boolean): Unit =
    deserialize(getLazyCubbieSeq(Seq(() => c1, () => c2)), file, gzip)
  def deserialize(c1: => Cubbie, c2: => Cubbie, c3: => Cubbie, file: File, gzip: Boolean): Unit =
    deserialize(getLazyCubbieSeq(Seq(() => c1, () => c2, () => c3)), file, gzip)
  def deserialize(c1: => Cubbie, c2: => Cubbie, c3: => Cubbie, c4: => Cubbie, file: File, gzip: Boolean): Unit =
    deserialize(getLazyCubbieSeq(Seq(() => c1, () => c2, () => c3, () => c4)), file, gzip)

  def serialize(c1: => Cubbie, filename: String): Unit = serialize(c1, new File(filename))
  def serialize(c1: => Cubbie, file: File): Unit =
    serialize(getLazyCubbieSeq(Seq(() => c1)), file, gzip = false)
  def serialize(c1: => Cubbie, c2: => Cubbie, file: File): Unit =
    serialize(getLazyCubbieSeq(Seq(() => c1, () => c2)), file, gzip = false)
  def serialize(c1: => Cubbie, c2: => Cubbie, c3: => Cubbie, file: File): Unit =
    serialize(getLazyCubbieSeq(Seq(() => c1, () => c2, () => c3)), file, gzip = false)
  def serialize(c1: => Cubbie, c2: => Cubbie, c3: => Cubbie, c4: => Cubbie, file: File): Unit =
    serialize(getLazyCubbieSeq(Seq(() => c1, () => c2, () => c3, () => c4)), file, gzip = false)

  def deserialize(c1: => Cubbie, filename: String): Unit = deserialize(c1, new File(filename))
  def deserialize(c1: => Cubbie, file: File): Unit =
    deserialize(getLazyCubbieSeq(Seq(() => c1)), file, gzip = false)
  def deserialize(c1: => Cubbie, c2: => Cubbie, file: File): Unit =
    deserialize(getLazyCubbieSeq(Seq(() => c1, () => c2)), file, gzip = false)
  def deserialize(c1: => Cubbie, c2: => Cubbie, c3: => Cubbie, file: File): Unit =
    deserialize(getLazyCubbieSeq(Seq(() => c1, () => c2, () => c3)), file, gzip = false)
  def deserialize(c1: => Cubbie, c2: => Cubbie, c3: => Cubbie, c4: => Cubbie, file: File): Unit =
    deserialize(getLazyCubbieSeq(Seq(() => c1, () => c2, () => c3, () => c4)), file, gzip = false)

  def serialize(cs: Seq[Cubbie], file: File, gzip: Boolean = false): Unit = {
    val stream = writeFile(file, gzip)
    for (c <- cs) serialize(c, stream)
    stream.close()
  }
  def deserialize(cs: Seq[Cubbie], file: File, gzip: Boolean = false): Unit = {
    val stream = readFile(file, gzip)
    for (c <- cs) deserialize(c, stream)
    stream.close()
  }

  def writeFile(file: File, gzip: Boolean = false): DataOutputStream = {
    file.createNewFile()
    val fileStream = new BufferedOutputStream(new FileOutputStream(file))
    new DataOutputStream(if (gzip) new BufferedOutputStream(new GZIPOutputStream(fileStream)) else fileStream)
  }
  def readFile(file: File, gzip: Boolean = false): DataInputStream = {
    val fileStream = new BufferedInputStream(new FileInputStream(file))
    new DataInputStream(if (gzip) new BufferedInputStream(new GZIPInputStream(fileStream)) else fileStream)
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
  private val NULL: Byte = 0x08

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
    case NULL =>
      s.readByte()
      null
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
    case null => NULL
  }
  private def isPrimitiveTag(tag: Byte): Boolean = tag match {
    case DOUBLE | BOOLEAN | INT | NULL => true
    case _ => false
  }
  private def isPrimitive(value: Any): Boolean = isPrimitiveTag(tagForType(value))
  private def serialize(key: Option[String], value: Any, s: DataOutputStream): Unit = {
    //println("Serialize.serialize key="+key+" value="+(if (value != null) value.toString.take(20) else null))
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
        val tag = t.headOption.map(tagForType).getOrElse(INT)
        s.writeByte(tag)
        s.writeInt(t.size)
        t.foreach(serialize(None, _, s))
      case null =>
        s.writeByte(0x0)
    }
    s.flush()
  }
}

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