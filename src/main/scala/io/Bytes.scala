/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import scala.resource.ManagedResource
import scala.collection.Traversable
import StandardOpenOptions._

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
 * Factory for creating {@link ReadBytes} and {@link WriteBytes} from
 * streams and channels.
 * <p>
 * Note: It is highly recommended to pass the stream/channel creation code to the factory method
 * so that the Read/WriteBytes object can be reused.  For example:
 * <pre><code>
 * val url = new URL("www.scala-lang.org")
 * Bytes.fromInputStream(url.openStream)
 * </code></pre>
 * is preferable to
 * <pre><code>
 * val stream = new URL("www.scala-lang.org")
 * Bytes.fromInputStream(stream)
 * </code></pre>
 * The ReadBytes obtained from latter example can only be
 * used once before the stream is used.  But the former
 * can be re-used because the parameter is the function required
 * to create the input stream
 */
object Bytes {
  /**
   * Create a ReadBytes from an {@link InputStream}
   *
   * @param creator
   *          the function used to create an {@link InputStream}
   */
  def fromInputStream(creator: => InputStream): ReadBytes = fromReadableByteChannel(Channels.newChannel(creator)) 
  /**
   * Create a ReadBytes from an {@link ReadableByteChannel}
   *
   * @param creator
   *          the function used to create an {@link ReadableByteChannel}
   */
  def fromReadableByteChannel(creator: => ReadableByteChannel): ReadBytes =  new Read(creator)
  /**
   * Create a ReadBytes from an {@link OutputStream}
   *
   * @param creator
   *          the function used to create an {@link OutputStream}
   */
  def fromOutputStream(creator: => OutputStream): WriteBytes = fromWritableByteChannel(Channels.newChannel(creator)) 
  /**
   * Create a WriteBytes from an {@link WritableByteChannel}
   *
   * @param creator
   *          the function used to create an {@link WritableByteChannel}
   */
  def fromWritableByteChannel(creator: => WritableByteChannel): WriteBytes =  new Write(creator)
  /**
   * Create a ReadBytes with WriteBytes from an {@link ByteChannel}
   *
   * @param creator
   *          the function used to create an {@link ByteChannel}
   */
  def fromByteChannel(creator: => ByteChannel): ReadBytes with WriteBytes =  new ReadWrite(creator)
  /**
   * Create a ReadBytes with WriteBytes from an {@link FileChannel}
   *
   * @param creator
   *          the function used to create an {@link FileChannel}
   */
  def fromFileChannel(creator: => FileChannel): ReadBytes with WriteBytes =  new ReadWrite(creator)

  private class Read (creator: => ReadableByteChannel) extends ReadBytes {
    protected lazy val obtainReadableByteChannel = IoResource.fromReadableByteChannel(creator)
  }
  private class Write (creator: => WritableByteChannel) extends WriteBytes {
    protected lazy val obtainWritableByteChannel = IoResource.fromWritableByteChannel(creator)
  }
  private class ReadWrite (creator: => ByteChannel) extends ReadBytes with WriteBytes {
    lazy val resource = IoResource.fromByteChannel(creator)
    protected lazy val obtainWritableByteChannel = resource
    protected lazy val obtainReadableByteChannel = resource
  }
}
 
/**
 * Traits which can be viewed as a sequence of bytes.  Source types
 * which know their length should override def length: Long for more
 * efficient method implementations.
 * <p>
 * The default implementation uses the inputStream 
 * basis for reading the data.
 * </p>
 */
trait ReadBytes {
  /**
   * Obtains a {@ReadableByteResource} for input
   * operations
   */
  protected def obtainReadableByteChannel: ManagedResource[ReadableByteChannel]
  private def withBufferedInputStream[R]( in: InputStream => R): R = {
    obtainReadableByteChannel.acquireAndGet[R] (
      channel => {
        val buffered = Channels.newInputStream(channel)
        in(buffered)
      })
  }
  
  /**
   * The number of bytes available for reading
   * <p>
   * if length == -1 then it is not possible to determine the
   * number of bytes in advance.
   * </p>
   */
  def length: Long = -1

  /**
   * Obtains a Iterable for conveniently processing the resource as bytes.
   * <p>
   * Depending on the underlying resource this may be slower than
   * {@link #bytesAsInts}
   * </p>
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
   */
  def bytesAsInts(): Iterable[Int] = {
    withBufferedInputStream {
      in => Iterator continually in.read() takeWhile (_ != -1)
    }.toStream.view
  }

  /**
   * This method aspires to be the fastest way to read
   * a stream of known length into memory.
   */
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

trait WriteBytes {
  /**
   * Obtains a {@ReadableByteResource} for input
   * operations
   */
  protected def obtainWritableByteChannel: ManagedResource[WritableByteChannel]

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
                 openOptions: Iterable[OpenOptions] = WRITE_TRUNCATE): Unit = {
    // TODO
    ()
  }
  
}
