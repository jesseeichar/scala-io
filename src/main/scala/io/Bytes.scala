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

object Bytes {
  def fromInputStream(inputStream: => InputStream): ReadBytes = fromReadableByteChannel(Channels.newChannel(inputStream)) 
  def fromReadableByteChannel(channel: => ReadableByteChannel): ReadBytes =  new Read(channel)

  def fromOutputStream(outputStream: => OutputStream): WriteBytes = fromWritableByteChannel(Channels.newChannel(outputStream)) 
  def fromWritableByteChannel(channel: => WritableByteChannel): WriteBytes =  new Write(channel)

  def fromByteChannel(channel: => ByteChannel): ReadBytes with WriteBytes =  new ReadWrite(channel)
  def fromFileChannel(channel: => FileChannel): ReadBytes with WriteBytes =  new ReadWrite(channel)

  private class Read (channel: => ReadableByteChannel) extends ReadBytes {
    protected lazy val obtainReadableByteChannel = IoResource.fromReadableByteChannel(channel)
  }
  private class Write (channel: => WritableByteChannel) extends WriteBytes {
    protected lazy val obtainWritableByteChannel = IoResource.fromWritableByteChannel(channel)
  }
  private class ReadWrite (channel: => ByteChannel) extends ReadBytes with WriteBytes {
    lazy val resource = IoResource.fromByteChannel(channel)
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

  def writeBytes(bytes: Traversable[Byte],
                 openOptions: Iterable[OpenOptions] = WRITE_TRUNCATE): Unit = {
    // TODO
    ()
  }
  
}
