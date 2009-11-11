/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import scalax.io.resource._
import scala.resource.ManagedResource
import scala.collection.Traversable
import OpenOption._

import java.io.{
  InputStream, PrintStream, File => JFile,
  InputStreamReader, OutputStream
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
 * and ReadableByteChannel could both be a ReadBytes object (or be converted
 * to a ReadBytes object).
 * <p>
 * Note: All collections returned are non-strict collections and each
 * invocation of a method will typically open a new stream or channel.
 * That behaviour can be overrided by the implementation but
 * it is the default behaviour.
 * </p>
 *
 * @author Jesse Eichar
 * @since 1.0
 *
 * @see WriteBytes
 * @see Bytes
 * @see Chars
 * @see ReadChars
 * @see WriteChars
 */
trait ReadBytes {
  /**
   * The number of bytes available for reading
   * <p>
   * if length == None then it is not possible to determine the
   * number of bytes in advance.
   * </p>
   */
  def length: Option[Long] = None

  /**
   * Obtains a Iterable for conveniently processing the resource as bytes.
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
  def bytes(): Iterable[Byte] = bytesAsInts() map (_.toByte)

  /**
   * Obtains a Iterable for conveniently processing the file as Ints.
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
  def bytesAsInts(): Iterable[Int]

  /**
   * This method aspires to be the fastest way to read
   * a stream of known length into memory.
   */
  def slurpBytes(): Array[Byte] = bytes.toArray
}


/**
 * A trait for objects that have bytes written to them. For example an
 * OutputStream and File can both be WriteBytes (or be converted to one).
 * Depending on the implementation and the underlying object the
 * {@link OpenOptions} may be restricted to a subset of the
 * {@link OpenOption}.
 * <p>
 * Note: Each invocation of a method will typically open a new stream or
 * channel.  That behaviour can be overrided by the implementation but
 * it is the default behaviour.
 * </p>
 *
 * @author Jesse Eichar
 * @since 1.0
 *
 * @see ReadBytes
 * @see Bytes
 * @see Chars
 * @see ReadChars
 * @see WriteChars
 */
trait WriteBytes {

  /**
   * Write bytes to the file
   *
   * <strong>Important:</strong> The use of an Array is highly recommended
   * because normally arrays can be more efficiently written using
   * the underlying APIs
   * </p><p>
   * The bytes are either appended to the file or replace the contents of the
   * file depending on the openOptions. By default the conents of the file
   * will be replaced.
   * </p>
   *
   * @param bytes
   *          The bytes to write to the file
   * @param openOptions
   *          The options declaring how the file will be opened
   *          Default is WRITE/CREATE/TRUNCATE
   */
  def writeBytes(bytes: Traversable[Byte],
                 openOptions: Iterable[OpenOption] = WRITE_TRUNCATE): Unit = {
    // TODO
    ()
  }

}


/*
trait ReadChannelBytes extends ReadBytes {
  private def withBufferedInputStream[R]( in: InputStream => R): R = {
    readableByteChannel.acquireAndGet[R] (
      channel => {
        val buffered = Channels.newInputStream(channel)
        in(buffered)
      })

  def length: Long = -1

  def bytes(): Iterable[Byte] = bytesAsInts() map (_.toByte)
  def bytesAsInts(): Iterable[Int] = {
    withBufferedInputStream {
      in => Iterator continually in.read() takeWhile (_ != -1)
    }.toStream.view
  }
  def slurpBytes(): Array[Byte] = {
    // if we don't know the length, fall back on relative inefficiency
    if (length == -1L)
      return (new ArrayBuffer[Byte]() ++= bytes()).toArray

    val arr = new Array[Byte](length.toInt)
    val len = arr.length
    var offset = 0

    def loop(in:InputStream) {
      if (offset < len) {
        val read = in.read(arr, offset, len - offset)
        if (read >= 0) {
          offset += read
          loop(in)
        }
      }
    }
    withBufferedInputStream (loop _)

    if (offset == arr.length) arr
    else fail("Could not read entire source (%d of %d bytes)".format(offset, len))
  }
  }
*/
