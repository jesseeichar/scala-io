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
   * Get the Resource context that configures how the underlying resource is accessed
   *
   * @return the associated ResourceContext
   */
  def context:ResourceContext
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
  def bytes : LongTraversable[Byte]

  /**
   * Read the input as blocks of bytes.  This method should be avoided unless the maximum performance is
   * absolutely required because bytes provides very good performance and is a better API for most applications.
   *
   * However since it better reflects how data is read with most input sources (like InputStreams and ReadableByteChannels);
   * blocks is slightly more performance than bytes and therefore
   * can be used when performance is the most important consideration.
   *
   * @param blockSize block size can optionally be specified but the default is normally acceptable.
   */
  def blocks(blockSize: Option[Int] = None): LongTraversable[ByteBlock]

  /**
  * Obtains a Traversable for conveniently processing the file as Ints.
  *
  * @return an non-strict traversable over all the bytes with the bytes being represented as Ints
  */
  def bytesAsInts: LongTraversable[Int] = bytes.map(_.toInt).asInstanceOf[LongTraversable[Int]]

  /**
  * This method aspires to be the fastest way to read
  * a stream of known length into memory.
  */
  def byteArray: Array[Byte] = bytes.toArray

  /**
   * Copy all data from this Input object to the Output object
   * as efficiently as possible.
   *
   *
   * @param output output sink to copy the data to
   */
  def copyDataTo(output:Output): Unit = output.copyDataFrom(this)

  /**
   * The characters in the object.
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
  def chars(implicit codec: Codec = Codec.default): LongTraversable[Char]

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
  def lines(terminator: Terminators.Terminator = Terminators.Auto,
            includeTerminator: Boolean = false)(implicit codec: Codec = Codec.default): LongTraversable[String] = {
    new LineTraversable(chars(codec).iterator, terminator, includeTerminator, context)
  }
  /**
   * Loads all the characters into memory. There is no protection against
   * loading very large files/amounts of data.
   *
   * @param codec
   *          The codec representing the desired encoding of the characters
   */
  def slurpString(implicit codec: Codec = Codec.default) = new String(byteArray, codec.name)

}
