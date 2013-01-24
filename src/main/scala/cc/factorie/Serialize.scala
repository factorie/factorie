package cc.factorie

import java.io._
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import la.Tensor
import collection.mutable
import java.nio.channels.{ReadableByteChannel, WritableByteChannel, Channels}
import java.nio.ByteBuffer

final class CubbieMaker[-A](make: A => Cubbie) { def apply(a: A) = make(a) }

object CubbieMaker {
  implicit val modelCubbieMaker = new CubbieMaker[Model](new ModelCubbie(_))
  implicit val categoricalDomainCubbieMaker = new CubbieMaker[CategoricalDomain[String]](new CategoricalDomainCubbie(_))
}

object BinaryFileSerializer {
  import CubbieMaker._
  def serialize[A](toSerialize: A, fileName: String)(implicit maker: CubbieMaker[A]) =
    BinaryCubbieFileSerializer.serialize(maker(toSerialize), new File(fileName), gzip = false)
  def deserialize[A](deserializeTo: A, fileName: String)(implicit maker: CubbieMaker[A]) =
    BinaryCubbieFileSerializer.serialize(maker(deserializeTo), new File(fileName), gzip = false)
  def serialize[A](toSerialize: A, file: File)(implicit maker: CubbieMaker[A]) =
    BinaryCubbieFileSerializer.serialize(maker(toSerialize), file, gzip = false)
  def deserialize[A](deserializeTo: A, file: File)(implicit maker: CubbieMaker[A]) =
    BinaryCubbieFileSerializer.serialize(maker(deserializeTo), file, gzip = false)
  def serialize[A](toSerialize: A, fileName: String, gzip: Boolean)(implicit maker: CubbieMaker[A]) =
    BinaryCubbieFileSerializer.serialize(maker(toSerialize), new File(fileName), gzip)
  def deserialize[A](deserializeTo: A, fileName: String, gzip: Boolean)(implicit maker: CubbieMaker[A]) =
    BinaryCubbieFileSerializer.serialize(maker(deserializeTo), new File(fileName), gzip)
  def serialize[A](toSerialize: A, file: File, gzip: Boolean)(implicit maker: CubbieMaker[A]) =
    BinaryCubbieFileSerializer.serialize(maker(toSerialize), file, gzip)
  def deserialize[A](deserializeTo: A, file: File, gzip: Boolean)(implicit maker: CubbieMaker[A]) =
    BinaryCubbieFileSerializer.serialize(maker(deserializeTo), file, gzip)
  def serialize[A](toSerialize: A, s: DataOutputStream)(implicit maker: CubbieMaker[A]) =
    BinaryCubbieFileSerializer.serialize(maker(toSerialize), s)
  def deserialize[A](toSerialize: A, s: DataInputStream)(implicit maker: CubbieMaker[A]) =
    BinaryCubbieFileSerializer.deserialize(maker(toSerialize), s)
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