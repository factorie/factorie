package cc.factorie

import java.io._
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import la.Tensor
import collection.mutable
import java.nio.channels.{ReadableByteChannel, WritableByteChannel, Channels}
import java.nio.ByteBuffer

trait Serializer[-T] {
  def serialize(toSerialize: T, str: PrintStream): Unit
  def deserialize(deserializeTo: T, str: BufferedReader): Unit
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
    for ((k, v) <- c._map) serialize(Some(k), v, s)
  }
  def deserialize(c: Cubbie, s: DataInputStream): Unit = {
    for ((k, v) <- c._map.toSeq) {
      val key = readString(s)
      assert(k == key, "Cubbie keys don't match with serialized data!")
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
      val tensor = preexisting.asInstanceOf[Tensor]
//      def dump[T](x: T, title: String): T = { println(title + ": " + x); x }
//      repeat(dump(s.readInt(), "tensor length"))(tensor(dump(s.readInt(), "idx")) = dump(s.readDouble(), "value"))
      repeat(s.readInt())(tensor(s.readInt()) = s.readDouble())
      tensor
    case MAP =>
      val m = preexisting.asInstanceOf[mutable.HashMap[String, Any]]
      repeat(s.readInt()) {
        val key = readString(s)
        m(key) = deserializeInner(m(key), s.readByte(), s)
      }
      m
    case LIST =>
      val innerTag = s.readByte()
      val len = s.readInt()
      val buff = new mutable.ArrayBuffer[Any]()
      val iter = preexisting.asInstanceOf[Traversable[Any]].toIterator
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
    case _: mutable.HashMap[String, Any] => MAP
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
      case m: mutable.HashMap[String, Any] =>
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

//object StringCubbieFileSerializer {
//
//  def serialize(toSerialize: Cubbie, file: File, gzip: Boolean = false): Unit = {
//    file.createNewFile()
//    val fileStream = new BufferedOutputStream(new FileOutputStream(file))
//    val writer = new PrintStream(if (gzip) new BufferedOutputStream(new GZIPOutputStream(fileStream)) else fileStream)
//    serialize(toSerialize, writer)
//    writer.close()
//  }
//
//  def deserialize(deserializeTo: Cubbie, file: File, gzip: Boolean = false): Unit = {
//    val fileStream = new FileInputStream(file)
//    val str = new BufferedReader(new InputStreamReader(if (gzip) new GZIPInputStream(new BufferedInputStream(fileStream)) else fileStream))
//    deserialize(deserializeTo, str)
//    str.close()
//  }
//
//  def deserialize(c: Cubbie, s: BufferedReader): Unit = {
//    for ((_, v) <- c._map.toSeq) {
//      val key = s.readLine()
//      c._map(key) = deserializeInner(v, s.readLine(), s)
//    }
//  }
//  def serialize(c: Cubbie, p: PrintStream): Unit = {
//    for ((k, v) <- c._map) serialize(k, v, new PrintWriter(p))
//  }
//  private def deserializeInner(preexisting: Any, tag: String, s: BufferedReader, hasEndMarker: Boolean = true): Any = {
//    tag match {
//      case "#double" =>
//        val v = s.readLine().toDouble; if (hasEndMarker) s.readLine(); v
//      case "#int" =>
//        val v = s.readLine().toInt; if (hasEndMarker) s.readLine(); v
//      case "#boolean" =>
//        val v = s.readLine().toBoolean; if (hasEndMarker) s.readLine(); v
//      case "#tensor" =>
//        val tensor = preexisting.asInstanceOf[Tensor]
//        var line = ""
//        while ({line = s.readLine(); line != "#end"}) {
//          val fields = line.split(" +")
//          tensor(fields(0).toInt) = fields(1).toDouble
//        }
//        tensor
//      case "#string" =>
//        val b = new StringBuilder
//        var line = ""
//        while ({line = s.readLine(); line != "#end"}) b.append(line)
//        b.mkString
//      case "#map" =>
//        val m = preexisting.asInstanceOf[mutable.HashMap[String, Any]]
//        for ((_, v) <- m.toSeq) {
//          val key = s.readLine()
//          m(key) = deserializeInner(v, s.readLine(), s)
//        }
//        if (hasEndMarker) s.readLine()
//        m
//      case "#list" =>
//        val innerTag = s.readLine()
//        val len = s.readLine().toInt
//        val buff = new mutable.ArrayBuffer[Any]()
//        val iter = preexisting.asInstanceOf[Traversable[Any]].toIterator
//        for (_ <- 0 until len) {
//          val pre = if (iter.hasNext) iter.next() else null
//          if (!isPrimitiveTag(innerTag)) s.readLine()
//          buff += deserializeInner(pre, innerTag, s, hasEndMarker = !isPrimitiveTag(innerTag))
//        }
//        if (hasEndMarker) s.readLine()
//        buff
//    }
//  }
//  private def tagForType(value: Any): String = value match {
//    case _: Int => "#int"
//    case _: Double => "#double"
//    case _: Tensor => "#tensor"
//    case _: Boolean => "#boolean"
//    case _: String => "#string"
//    case _: mutable.HashMap[String, Any] => "#map"
//    case _: Traversable[_] => "#list"
//  }
//  private def isPrimitiveTag(value: String): Boolean = value match {
//    case "#double" | "#boolean" | "#int" => true
//    case _ => false
//  }
//  private def isPrimitive(value: Any): Boolean = isPrimitiveTag(tagForType(value))
//  private def serialize(key: String, value: Any, writer: PrintWriter): Unit = {
//    if (key != "") writer.println(key)
//    if (!(key == "" && isPrimitive(value))) writer.println(tagForType(value))
//    value match {
//      case _: String =>
//        writer.println(value) // TODO: escape these strings so no "#"'s get in there and mess up the format -luke
//      case _: Int | _: Boolean | _: Double =>
//        writer.println(value)
//      case t: Tensor =>
//        for ((i, v) <- t.activeElements; if (v != 0.0))
//          writer.println(i + " " + v)
//      case m: mutable.HashMap[String, Any] =>
//        for ((k, v) <- m) serialize(k, v, writer)
//      case t: Traversable[_] =>
//        writer.println(tagForType(t.head))
//        writer.println(t.size)
//        t.foreach(x => serialize("", x, writer))
//    }
//    if (!(key == "" && isPrimitive(value))) writer.println("#end")
//    writer.flush()
//  }
//}

object Serializer {

  def serialize[T](toSerialize: T, file: File, gzip: Boolean = false)(implicit serializer: Serializer[T]): Unit = {
    file.createNewFile()
    val fileStream = new BufferedOutputStream(new FileOutputStream(file))
    val writer = new PrintStream(if (gzip) new BufferedOutputStream(new GZIPOutputStream(fileStream)) else fileStream)
    serializer.serialize(toSerialize, writer)
    writer.close()
  }

  def deserialize[T](deserializeTo: T, file: File, gzip: Boolean = false)(implicit serializer: Serializer[T]): Unit = {
    val fileStream = new FileInputStream(file)
    val str = new BufferedReader(new InputStreamReader(if (gzip) new GZIPInputStream(new BufferedInputStream(fileStream)) else fileStream))
    serializer.deserialize(deserializeTo, str)
    str.close()
  }

  def serialize[T](toSerialize: T, str: PrintStream)(implicit serializer: Serializer[T]): Unit = {
    serializer.serialize(toSerialize, str)
  }

  def deserialize[T](deserializeTo: T, str: BufferedReader)(implicit serializer: Serializer[T]): Unit = {
    serializer.deserialize(deserializeTo, str)
  }

  implicit object CategoricalDomainSerializer extends Serializer[CategoricalDomain[String]] {

    def serialize(domain: CategoricalDomain[String], str: PrintStream): Unit = {
      val writer = new PrintWriter(str)
      if (domain.frozen) writer.println("#frozen = true") else writer.println("#frozen = false")
      for (e <- domain.iterator) {
        if (e.toString.contains("\n")) throw new Error("Cannot save Domain with category String containing newline.")
        writer.println(e.toString)
      }
      writer.println("#end")
      writer.flush()
    }

    def deserialize(domain: CategoricalDomain[String], reader: BufferedReader): Unit = {
      var line = reader.readLine
      var willFreeze = false
      if (line.split("\\s+").apply(2) == "true") willFreeze = true // Parse '#frozen = true'
      if (domain.string2T eq null)
        while ({line = reader.readLine; line != null && line != "#end"})
          domain.index(line)
      else
        while ({line = reader.readLine; line != null && line != "#end"})
          domain.index(domain.string2T(line))
      if (willFreeze) {domain.freeze(); domain._frozenByLoader = true}
    }
  }

  object DotFamilySerializer extends Serializer[DotFamily] {

    def serialize(dotFamily: DotFamily, str: PrintStream): Unit = {
      val writer = new PrintWriter(str)
      for (weight <- dotFamily.weights.activeElements; if (weight._2 != 0.0)) {
        //      println("writing: " + weight)
        writer.print(weight._1)
        writer.print(" ")
        writer.println(weight._2)
      }
      writer.println("#end")
      writer.flush()
    }

    def deserialize(dotFamily: DotFamily, reader: BufferedReader): Unit = {
      var line = ""
      while ( {line = reader.readLine; line != null && line != "#end"}) {
        //      println("reading: " + line)
        val fields = line.split(" +")
        assert(fields.length == 2)
        val index = fields(0).toInt
        val value = fields(1).toDouble
        dotFamily.weights(index) = value
      }
    }
  }

  implicit object FamilySerializer extends Serializer[Family] {
    def serialize(family: Family, str: PrintStream): Unit = {
      family match {
        case df: DotFamily => DotFamilySerializer.serialize(df, str)
      }
    }
    def deserialize(family: Family, reader: BufferedReader): Unit = {
      family match {
        case df: DotFamily => DotFamilySerializer.deserialize(df, reader)
      }
    }
  }

  implicit object ModelSerializer extends Serializer[Model] {
    def serialize(m: Model, str: PrintStream): Unit = {
      m.families.foreach(Serializer.serialize(_, str))
    }
    def deserialize(model: Model, reader: BufferedReader): Unit = {
      model.families.foreach(Serializer.deserialize(_, reader))
    }
  }
}