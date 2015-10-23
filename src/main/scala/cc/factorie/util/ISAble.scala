package cc.factorie.util

import java.io._
import java.net.URL
import java.nio.file.{Files, Path}

/**
 * @author johnsullivan
 * Typeclass for things that can become [[java.io.InputStream]] */
trait ISAble[-A] extends (A => InputStream)

object ISAble {
  implicit object IsIs extends ISAble[InputStream] {
    def apply(is:InputStream) = is
  }

  implicit object UrlIs extends ISAble[URL] {
    def apply(u:URL) = u.openStream()
  }

  implicit object FileIs extends ISAble[File] {
    def apply(f:File) = new FileInputStream(f)
  }

  implicit object PathIs extends ISAble[Path] {
    def apply(p:Path) = Files.newInputStream(p)
  }

  implicit object BytesIs extends ISAble[Array[Byte]] {
    def apply(arr:Array[Byte]) = new ByteArrayInputStream(arr)
  }

  def inputStreamOf[A](a:A)(implicit conv:ISAble[A]):InputStream = conv(a)
  def buffered[A](a:A, buf:Int = 8192)(implicit conv:ISAble[A]):BufferedInputStream = conv(a) match {
    case b:BufferedInputStream => b
    case is => new BufferedInputStream(is, buf)
  }
  def lines[A](a:A, charSet:String="UTF-8")(implicit conv:ISAble[A]):Iterator[String] = new Iterator[String] {
    private val rdr = new BufferedReader(new InputStreamReader(conv(a), charSet))
    private var nextLine = rdr.readLine()

    def next() = {
      val res = nextLine
      nextLine = rdr.readLine()
      if(nextLine == null) {
        rdr.close()
      }
      res
    }

    def hasNext = nextLine != null
  }
}


