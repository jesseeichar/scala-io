/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2011, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import scala.collection.Traversable
import Line._
import java.net.URL
import java.io.{InputStream, File}

/**
 * An trait for objects that viewed as a sequence of bytes. For example InputStream
 * and ReadableByteChannel could be an Input object.
 *
 * Note: All collections returned are non-strict collections and each
 * invocation of a method will typically open a new stream or channel.
 * That behaviour can be overriden by the implementation but
 * it is the default behaviour.
 *
 * Default implementation is based on providing an implementation for
 * bytesAsInts and all other methods are implemented using
 * that method.
 *
 * @author Jesse Eichar
 * @since 1.0
 *
 * @see [[scalax.io.Output]]
 * @see [[scalax.io.ReadChars]]
 * @see [[scalax.io.WriteChars]]
 */
trait Input {

    /**
    * The number of bytes that can be read from the underlying resource.
    *
    * if length == None then it is not possible to determine the
    * number of bytes in advance.
    */
    def size: Option[Long]

    /**
    * Obtains a Traversable for conveniently processing the resource as bytes.
    *
    * @return an non-strict traversable over all the bytes
    */
    def bytes : ResourceView[Byte] = (bytesAsInts map {_.toByte}).asInstanceOf[ResourceView[Byte]]    // TODO this is broken

    /**
    * Obtains a Traversable for conveniently processing the file as Ints.
    *
    * @return an non-strict traversable over all the bytes with the bytes being represented as Ints
    */
    def bytesAsInts: ResourceView[Int]

    /**
    * This method aspires to be the fastest way to read
    * a stream of known length into memory.
    */
    def byteArray: Array[Byte] = bytes.toArray

    /**
     * The characters in the object.$
     *
     * If the codec is not the same as the source codec (the codec of
     * the underlying data) then the characters will converted to the
     * desired codec.
     *
     * @param codec
     *          The codec representing the desired encoding of the characters
     * @return
     *          an traversable of all the characters
     */
    def chars(implicit codec: Codec): ResourceView[Char]

    /**
     * Obtain an non-strict traversable for iterating through the lines in the object
     *
     * If the codec is not the same as the source codec (the codec of
     * the underlying data) then the characters will converted to the
     * desired codec.
     *
     * @param codec
     *          The codec representing the desired encoding of the characters
     * @param terminator
     *          The strategy for determining the end of line
     *          Default is to auto-detect the EOL
     * @param includeTerminator
     *          if true then the line will end with the line terminator
     *          Default is false
     *
     * @return
     *          a non-strict traversable for iterating through all the lines
     */
    def lines(terminator: Terminators.Terminator = new Terminators.Auto(),
              includeTerminator: Boolean = false)(implicit codec: Codec): ResourceView[String] = {
                  new LineTraversable(chars(codec), terminator, includeTerminator).view
    }
    /**
     * Loads all the characters into memory. There is no protection against
     * loading very large files/amounts of data.
     *
     * If the codec is not the same as the source codec (the codec of
     * the underlying data) then the characters will converted to the
     * desired codec.
     *
     * @param codec
     *          The codec representing the desired encoding of the characters
     */
    def slurpString(implicit codec: Codec) = chars(codec).mkString
}

object Input {
  class AsInput(op: => Input) {
    /** An object to an input object */
    def asInput: Input = op
  }

  /**
   * Wrap an arbitraty object as and AsInput object allowing the object to be converted to an Input object.
   *
   * The possible types of src are the subclasses of [[scalax.io.AsInputConverter]]
   */
  implicit def asInputConverter[B](src:B)(implicit converter:AsInputConverter[B]) =
    new AsInput(converter.toInput(src))

  /**
   * Used by the [[scalax.io.Input]] object for converting an arbitrary object to an Input Object
   *
   * Note: this is a classic use of the type class pattern
   */
  trait AsInputConverter[-A] {
    def toInput(t:A) : Input
  }

  /**
   * contains several implementations of [[scalax.io.AsInputConverter]].  They will be implicitely resolved allowing
   * a user of the library to simple call A.asInput and the converter will be found without the user needing to look up these classes
   */
  object AsInputConverter {

    /**
     * Converts a File to an Input object
     */
    implicit object FileConverter extends AsInputConverter[File]{
      def toInput(file: File) = Resource.fromFile(file)
    }
    /**
     * Converts a URL to an Input object
     */
    implicit object URLConverter extends AsInputConverter[URL]{
      def toInput(url: URL) = Resource.fromURL(url)
    }
    /**
     * Converts a InputStream to an Input object
     */
    implicit object InputStreamConverter extends AsInputConverter[InputStream]{
      def toInput(is: InputStream) = Resource.fromInputStream(is)
    }
    /**
     * Converts a Traversable of Ints to an Input object.  Each Int is treated as a byte
     */
    implicit object TraversableIntsAsBytesConverter extends AsInputConverter[Traversable[Int]]{
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
    implicit object TraversableByteConverter extends AsInputConverter[Traversable[Byte]]{
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


}
