/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import scalax.io.resource._
import scala.collection.Traversable
import OpenOption._
import Line._
import java.io.{ 
  InputStream, PrintStream, File => JFile,
  InputStreamReader, OutputStream, Writer, Reader
}
import java.nio.channels.{
  ByteChannel, WritableByteChannel, FileChannel, ReadableByteChannel,
  Channels
}
import java.net.{ URI, URL }

import collection.mutable.ArrayBuffer
import Path.fail

/**
 * An trait for objects that viewed as a sequence of bytes. For example InputStream
 * and ReadableByteChannel could be an Input object (or be converted
 * to a ReadBytes object).
 * <p>
 * Note: All collections returned are non-strict collections and each
 * invocation of a method will typically open a new stream or channel.
 * That behaviour can be overrided by the implementation but
 * it is the default behaviour.
 * </p>
 * <p>
 * Default implementation is based on providing an implementation for 
 * bytesAsInts and all other methods are implemented using
 * that method.  
 * @author Jesse Eichar
 * @since 1.0
 *
 * @see Output
 */
trait Input {

    /**
    * The number of bytes available for reading
    * <p>
    * if length == None then it is not possible to determine the
    * number of bytes in advance.
    * </p>
    */
    def size: Option[Long] = None

    /**
    * Obtains a Traversable for conveniently processing the resource as bytes.
    * <p>
    * Depending on the underlying resource this may be slower than
    * {@link #bytesAsInts}
    * </p>
    * <p>
    * Note: The iterable returned is a non-strict collection
    * </p><p>
    * In some object the bytes of underlying iterable can be cast to an Seq
    * and elements can be randomly accessed. Random access must be used
    * carefully as each access will open a new stream unless that behavior
    * is modified by the implementation.
    * </p><p>
    * For example on some filesystems using random access within a
    * {@link FileOperations#open} will perform all accesses using the same
    * Channel improving the performance
    * </p>
    *
    * @return an non-strict iterable over all the bytes
    */
    def bytes: Traversable[Byte] = bytesAsInts map (_.toByte)

    /**
    * Obtains a Traversable for conveniently processing the file as Ints.
    * <p>
    * Depending on the underlying resource this may be slower than
    * {@link #bytes}
    * </p>
    * <p>
    * This is a View so remember to treat it as a view and not as a Stream or
    * a strict collection
    * </p>
    * <p>
    * In some object the bytes of underlying iterable can be cast to an Seq
    * and elements can be randomly accessed. Random access must be used
    * carefully as each access will open a new stream unless that behavior
    * is modified by the implementation.
    * </p><p>
    * For example on some filesystems using random access within a
    * {@link FileOperations#open} will perform all accesses using the same
    * Channel improving the performance
    * </p>
    * <p>
    * Note: The iterable returned is a non-strict collection
    * </p>
    *
    * @return an non-strict iterable over all the bytes with the bytes being represented as Ints
    */
    def bytesAsInts: Traversable[Int]

    /**
    * This method aspires to be the fastest way to read
    * a stream of known length into memory.
    */
    def slurpBytes: Array[Byte] = bytes.toArray

    /**
     * The characters in the object.
     * <p>
     * If the codec is not the same as the source codec (the codec of
     * the underlying data) then the characters will converted to the
     * desired codec.
     * </p><p>
     * In some object the bytes of underlying iterable can be cast to a Seq
     * and elements can be randomly accessed. Random access must be used
     * carefully as each access will open a new stream unless that behavior
     * is modified by the implementation.
     * </p><p>
     * For example on some filesystems using random access within a
     * {@link FileOperations#open} will perform all accesses using
     * the same Channel improving the performance.
     * </p>
     * 
     * @param codec
     *          The codec representing the desired encoding of the characters
     * @return
     *          an iterable of all the characters
     */
    def chars(implicit codec: Codec): Traversable[Char]
    
    /**
     * Obtain an non-strict iterable for iterating through the lines in the object
     * <p>
     * If the codec is not the same as the source codec (the codec of
     * the underlying data) then the characters will converted to the
     * desired codec.
     * </p><p>
     * In some object the bytes of underlying iterable can be cast to a Seq
     * and elements can be randomly accessed. Random access must be used
     * carefully as each access will open a new stream unless that behavior
     * is modified by the implementation.
     * </p><p>
     * For example on some filesystems using random access within a
     * {@link FileOperations#open} will perform all accesses using
     * the same Channel improving the performance.
     * </p>       
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
     *          a non-strict iterable for iterating through all the lines
     */
    def lines(terminator: Terminators.Terminator = Terminators.Auto,
              includeTerminator: Boolean = false)(implicit codec: Codec): Traversable[String] = {
               /* require(terminator.length == 1 || terminator.length == 2, "Line terminator may be 1 or 2 characters only.")
                new Traversable[String] {
                  def iterator = new LineIterator(chars(codec), terminator, includeTerminator)
                }*/
                null //TODO
              }
    /**
     * Loads all the characters into memory. There is no protection against
     * loading very large files/amounts of data.
     * <p>
     * If the codec is not the same as the source codec (the codec of
     * the underlying data) then the characters will converted to the
     * desired codec.
     * </p>
     * @param codec
     *          The codec representing the desired encoding of the characters  
     */
    def slurpString(implicit codec: Codec) = chars(codec).mkString
}
