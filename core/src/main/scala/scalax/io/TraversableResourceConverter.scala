package scalax.io

import java.net.URL
import java.io.{File, RandomAccessFile}

/**
 * Used by the [[scalax.io.Input]] object for converting an arbitrary object to an Input Object
 *
 * Note: this is a classic use of the type class pattern
 */
trait TraversableResourceConverter[-A] {
  def toInput(t:A) : Input
}

/**
 * contains several implementations of [[scalax.io.TraversableResourceConverter]].  They will be implicitely resolved allowing
 * a user of the library to simple call A.asInput and the converter will be found without the user needing to look up these classes
 */
object TraversableResourceConverter {

  /**
   * Converts a File to an Input object
   */
  implicit object FileConverter extends TraversableResourceConverter[File]{
    def toInput(file: File) = Resource.fromFile(file)
  }
  /**
   * Converts a URL to an Input object
   */
  implicit object URLConverter extends TraversableResourceConverter[URL]{
    def toInput(url: URL) = Resource.fromURL(url)
  }
  /**
   * Converts a Traversable of Ints to an Input object.  Each Int is treated as a byte
   */
  implicit object TraversableIntsAsBytesConverter extends TraversableResourceConverter[Traversable[Int]]{
    def toInput(t: Traversable[Int]) = new Input {
      def chars(implicit codec: Codec) = new LongTraversable[Char] {
        val maxChars = codec.encoder.maxBytesPerChar

        lazy val chars = codec.decode(t.view.map{_.toByte}.toArray)
        def foreach[U](f: (Char) => U) = chars.foreach(f)
      }.view

      def bytesAsInts = new LongTraversable[Int]{
        def foreach[U](f: (Int) => U) = t.foreach(f)
      }.view

      def size = Some(t.size)
    }
  }
  /**
   * Converts a Traversable[Byte] to an Input object
   */
  implicit object TraversableByteConverter extends TraversableResourceConverter[Traversable[Byte]]{
    def toInput(t: Traversable[Byte]) = new Input {
      def chars(implicit codec: Codec) = new LongTraversable[Char] {
        val maxChars = codec.encoder.maxBytesPerChar

        lazy val chars = codec.decode(t.toArray)
        def foreach[U](f: (Char) => U) = chars.foreach(f)
      }.view

      def bytesAsInts = new LongTraversable[Int]{
        def foreach[U](f: (Int) => U) = t.foreach(b => f(b.toInt))
      }.view


      override def bytes = new LongTraversable[Byte]{
        def foreach[U](f: (Byte) => U) = t.foreach(b => f(b))
      }.view

      def size = Some(t.size)
    }
  }
}

