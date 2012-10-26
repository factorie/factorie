package cc.factorie

import java.io._
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

trait Serializer[-T] {
  def serialize(toSerialize: T, str: PrintStream): Unit
  def deserialize(deserializeTo: T, str: BufferedReader): Unit
}

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