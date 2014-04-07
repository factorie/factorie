package cc.factorie.util

import java.io._
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import collection.mutable
import cc.factorie.la._
import scala.language.implicitConversions
import cc.factorie.variable._
import scala.Some
import cc.factorie.model.{WeightsSetCubbie, Parameters}
import cc.factorie.app.classify.backend.{BoostedTreeCubbie, DecisionTreeCubbie, RandomForestCubbie}

// We have these in a trait so we can mix them into the package object and make them available by default
trait CubbieConversions {
  implicit def m2cc[T](m: T)(implicit cc: StoreFetchCubbie[T]): Cubbie = { cc.store(m); cc }
  implicit def ccts[T <: Seq[Tensor]]: StoreFetchCubbie[T] = new TensorListCubbie[T]
  implicit def cct[T <: Tensor]: StoreFetchCubbie[T] = new TensorCubbie[T]

  implicit def btc: RandomForestCubbie = new RandomForestCubbie
  implicit def rfc: DecisionTreeCubbie = new DecisionTreeCubbie
  implicit def dtc: BoostedTreeCubbie = new BoostedTreeCubbie

  implicit def modm(m: Parameters): Cubbie = new WeightsSetCubbie(m.parameters)
  implicit def cdm(m: CategoricalDomain[_]): Cubbie = new CategoricalDomainCubbie(m)
  implicit def csdm(m: CategoricalSeqDomain[_]): Cubbie = new CategoricalSeqDomainCubbie(m)
  implicit def cdtdm(m: CategoricalVectorDomain[_]): Cubbie = new CategoricalVectorDomainCubbie(m)
  implicit def simm(m: mutable.Map[String,Int]): Cubbie = new StringMapCubbie(m) //StringMapCubbie is parametrized by T, as String -> T, so this knows that it's an Int
  implicit def smm(m: mutable.Map[String,String]): Cubbie = new StringMapCubbie(m)
}

// You can import this object to gain access to the default cubbie conversions
object CubbieConversions extends CubbieConversions

object BinarySerializer extends GlobalLogging {
  import cc.factorie._
  private def getLazyCubbieSeq(vals: Seq[() => Cubbie]): Seq[Cubbie] = vals.view.map(_())
  // We lazily create the cubbies because, for example, model cubbies will force their model's weights' lazy vals
  // so we need to control the order of cubbie creation and serialization (domains are deserialized before model cubbies are even created)
  // Is this still an issue with the Weights/Parameters stuff?? -luke
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

  def deserialize[T](filename: String)(implicit cc: StoreFetchCubbie[T]): T = { deserialize(cc, filename); cc.fetch() }
  def deserialize[T](file: File)(implicit cc: StoreFetchCubbie[T]): T = { deserialize(cc, file); cc.fetch() }
  def deserialize[T](file: File, gzip: Boolean)(implicit cc: StoreFetchCubbie[T]): T = { deserialize(cc, file, gzip); cc.fetch() }

  def serialize(cs: Seq[Cubbie], file: File, gzip: Boolean = false): Unit = {
    val stream = writeFile(file, gzip)
    for (c <- cs) serialize(c, stream)
    stream.close()
  }
  def deserialize(cs: Seq[Cubbie], file: File, gzip: Boolean = false): Unit = {
    val stream = readFile(file, gzip)
    for (c <- cs) {
      if (logger.level == Logger.DEBUG) {
        println("Before deserializing cubbie " + c.getClass.getName)
        System.gc()
        val runtime = Runtime.getRuntime
        println("Used memory: " + ((runtime.totalMemory() - runtime.freeMemory())/(1024*1024.0*1025)) + " GB")
      }
      deserialize(c, stream)
    }
    stream.close()
    if (logger.level == Logger.DEBUG) {
      println("After deserialization.")
      System.gc()
      val runtime = Runtime.getRuntime
      println("Used memory: " + ((runtime.totalMemory() - runtime.freeMemory())/(1024*1024.0*1025)) + " GB")
    }
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

  val currentVersion = "1.0"
  def serialize(c: Cubbie, s: DataOutputStream): Unit = {
    serialize(Some("version"), currentVersion, s)
    serialize(Some("cubbieVersion"), c.version, s)
    serialize(Some("cubbieName"), c.cubbieName, s)
    for ((k, v) <- c._map.toSeq) serialize(Some(k), v, s)
  }
  def deserialize(c: Cubbie, s: DataInputStream): Unit = {
    def readField(fieldName: String): String = {
      assert(readString(s) == fieldName, "Cubbie must have \"%s\" field serialized." format fieldName)
      s.readByte() // throw out tag
      deserializeInner(null, STRING, s).asInstanceOf[String]
    }
    def assertSame(fieldName: String, written: String, expected: String): Unit =
      assert(written == expected, "Expected written cubbie %s to be \"%s\", instead got \"%s\"" format (fieldName, written, expected))

    assertSame("version", readField("version"), currentVersion)
    assertSame("cubbieVersion", readField("cubbieVersion"), c.version)
    assertSame("cubbieName", readField("cubbieName"), c.cubbieName)

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
  private val MAP: Byte = 0x06
  private val LIST: Byte = 0x07
  private val NULL: Byte = 0x08
  private val SPARSE_INDEXED_TENSOR: Byte = 0x09
  private val SPARSE_BINARY_TENSOR: Byte = 0x10
  private val DENSE_TENSOR: Byte = 0x11

  private def deserializeInner(preexisting: Any, tag: Byte, s: DataInputStream): Any = tag match {
    case DOUBLE => s.readDouble()
    case INT => s.readInt()
    case BOOLEAN => s.readShort() != 0
    case STRING => readString(s)
    case SPARSE_INDEXED_TENSOR | SPARSE_BINARY_TENSOR | DENSE_TENSOR =>
      val activeDomainSize = s.readInt()
      val dims = readIntArray(s)
      val order = dims.length
      // use pre-existing if there is one, and it has the right dimensions
      val preexistingTensor = preexisting.toNotNull.flatMap(_.cast[Tensor])
      val newBlank =
        // Deserialization can substitute a different sparsity/density than was allocated.
        // Useful for training Dense, sparsifying, serializing, and deserializing without changing the Model's Dense initialization. -akm
        if (preexistingTensor.exists(_.dimensions.sameElements(dims)))
          preexistingTensor.get
        else (tag, order) match {
          case (SPARSE_INDEXED_TENSOR, 1) => new SparseIndexedTensor1(dims(0))
          case (SPARSE_INDEXED_TENSOR, 2) => new SparseIndexedTensor2(dims(0), dims(1))
          case (SPARSE_INDEXED_TENSOR, 3) => new SparseIndexedTensor3(dims(0), dims(1), dims(2))
          case (SPARSE_BINARY_TENSOR, 1) => new SparseBinaryTensor1(dims(0))
          case (SPARSE_BINARY_TENSOR, 2) => new SparseBinaryTensor2(dims(0), dims(1))
          case (SPARSE_BINARY_TENSOR, 3) => new SparseBinaryTensor3(dims(0), dims(1), dims(2))
          case (DENSE_TENSOR, 1) => new DenseTensor1(dims(0))
          case (DENSE_TENSOR, 2) => new DenseTensor2(dims(0), dims(1))
          case (DENSE_TENSOR, 3) => new DenseTensor3(dims(0), dims(1), dims(2))
        }
      newBlank match {
        case nb: ArraySparseIndexedTensor =>
          nb.sizeHint(activeDomainSize)
          val idxArr = readIntArray(s)
          val valArr = readDoubleArray(s)
          for ((i, v) <- idxArr.zip(valArr)) nb += (i, v)
        case nb: ArraySparseBinaryTensor =>
          nb.sizeHint(activeDomainSize)
          nb ++= readIntArray(s)
        case nb: DenseTensor =>
          readDoubleArray(s, nb.asArray)
      }
      newBlank
    case TENSOR =>
      val tensor = preexisting.toNotNull.flatMap(_.cast[Tensor])
        .getOrElse(sys.error("Require pre-existing tensor value in cubbie for general \"TENSOR\" slot."))
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
        // if it's not primitive, we need to read the inner type tag because we could have heterogeneous tensor list, etc
        val nextTag = if (isPrimitiveTag(innerTag)) innerTag else s.readByte() // read and ignore the type tag
        buff += deserializeInner(pre, nextTag, s)
      }
      buff
    case NULL =>
      s.readByte()
      null
  }
  // TODO come back and make these fast and read/write whole blocks of bytes at once
  private def readDoubleArray(s: DataInputStream, arr: Array[Double]): Array[Double] = { val length = s.readInt(); var i = 0; while (i < length) { arr(i) = s.readDouble(); i += 1 }; arr }
  private def readDoubleArray(s: DataInputStream): Array[Double] = { val arr = new Array[Double](s.readInt()); var i = 0; while (i < arr.length) { arr(i) = s.readDouble(); i += 1 }; arr }
  private def writeDoubleArray(s: DataOutputStream, arr: Array[Double]): Unit = writeDoubleArray(s, arr, arr.length)
  private def writeDoubleArray(s: DataOutputStream, arr: Array[Double], length: Int): Unit = { s.writeInt(length); var i = 0; while (i < length) { s.writeDouble(arr(i)); i += 1 } }
  private def readIntArray(s: DataInputStream, arr: Array[Int]): Array[Int] = { val length = readInt(); var i = 0; while (i < length) { arr(i) = s.readInt(); i += 1 }; arr }
  private def readIntArray(s: DataInputStream): Array[Int] = { val arr = new Array[Int](s.readInt()); var i = 0; while (i < arr.length) { arr(i) = s.readInt(); i += 1 }; arr }
  private def writeIntArray(s: DataOutputStream, arr: Array[Int]): Unit = writeIntArray(s, arr, arr.length)
  private def writeIntArray(s: DataOutputStream, arr: Array[Int], length: Int): Unit = { s.writeInt(length); var i = 0; while (i < length) { s.writeInt(arr(i)); i += 1 } }
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
    case _: SparseIndexedTensor => SPARSE_INDEXED_TENSOR
    case _: SparseBinaryTensor => SPARSE_BINARY_TENSOR
    case _: DenseTensor => DENSE_TENSOR
    case _: Tensor => TENSOR
    case _: mutable.Map[String @unchecked, Any @unchecked] => MAP
    case _: Traversable[_] => LIST
    case null => NULL
  }
  private def isPrimitiveTag(tag: Byte): Boolean = tag match {
    case DOUBLE | BOOLEAN | INT | NULL => true
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
      case t: Tensor if t.isInstanceOf[SparseIndexedTensor] || t.isInstanceOf[SparseBinaryTensor] || t.isInstanceOf[DenseTensor] =>
        val activeDomainSize = t.activeDomainSize
        s.writeInt(activeDomainSize)
        writeIntArray(s, t.dimensions)
        t match {
          case nb: SparseIndexedTensor =>
            writeIntArray(s, nb._indices, activeDomainSize)
            writeDoubleArray(s, nb._values, activeDomainSize)
          case nb: SparseBinaryTensor =>
            writeIntArray(s, nb._indices, activeDomainSize)
          case nb: DenseTensor => writeDoubleArray(s, nb.asArray, activeDomainSize)
        }
      case t: Tensor =>
        s.writeInt(t.activeDomainSize)
        t.foreachActiveElement((i, v) => {s.writeInt(i); s.writeDouble(v)})
      case m: mutable.Map[String @unchecked, Any @unchecked] =>
        s.writeInt(m.size)
        for ((k, v) <- m) serialize(Some(k), v, s)
      case t: Traversable[Any @unchecked] =>
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

abstract class StoreFetchCubbie[T] extends Cubbie {
  type StoredType = T
  def store(t: T): Unit
  def fetch(): T
}

class TensorCubbie[T <: Tensor] extends StoreFetchCubbie[T] {
  val tensor = new TensorSlot("tensor")
  // Hit this nasty behavior again - should not have to specify a default value in order to get a slot to serialize into
  tensor := (null: Tensor)
  def store(t: T): Unit = tensor := t
  def fetch(): T = tensor.value.asInstanceOf[T]
}

class TensorListCubbie[T <: Seq[Tensor]] extends StoreFetchCubbie[T] {
  val tensors = new TensorListSlot("tensors")
  // Hit this nasty behavior again - should not have to specify a default value in order to get a slot to serialize into
  tensors := (null: Seq[Tensor])
  def store(t: T): Unit = tensors := t
  def fetch(): T = tensors.value.asInstanceOf[T]
}