/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import scala.resource.{
  ManagedResource, AbstractUntranslatedManagedResource
}
import java.io.{
  BufferedReader, BufferedWriter, InputStream, OutputStream,
  BufferedInputStream, BufferedOutputStream, Reader, Writer,
  Closeable, InputStreamReader, OutputStreamWriter
}
import java.nio.channels.{
  ByteChannel, ReadableByteChannel, WritableByteChannel,
  Channels, FileChannel
}

/**
 * A Resource that can be used to do IO.  It wraps objects from the java.io package
 *
 * @param R
 *          The type of the resource that will be managed by the ManagedResource
 */
protected[io] trait IoResource[R <: Closeable] extends AbstractUntranslatedManagedResource[R] {
  /**
   * Creates a new InputStream (provided the code block used to create the resource is
   * re-usable).  This method should only be used with care in cases when Automatic
   * Resource Management cannot be used because the 
   * {@link InputStream} must be closed manually.
   * <p>
   * This is public only to permit interoperability with certain Java APIs.
   * A better pattern of use should be:
   * <code>
   * resource.acquireFor {
   *   // call java API
   * }
   * </code>
   * or
   * <code>
   * val calculatedResult = resource.acquireAndGet {
   *   // cal java API that returns a result
   * }
   * </code>
   */
  def open(): R
  protected final def unsafeClose(handle : R) = try{ handle.close } catch { case _ => () }
}

/**
 * An IoResource that can be used to also create a Buffered version of the resource
 *
 * @param O
 *          The ubuffered type of resource to be managed and can be converted to type B
 * @param B
 *          The buffered type of resource to be managed
 * @param BufferedRepr
 *          The type of Buffered*Resource that can be created from this type of resource
 */
protected[io] trait BufferableResource[O <: Closeable,B,BufferedRepr <: ManagedResource[O]] 
extends IoResource[O] {
  /**
   * Create a {@link BufferedInputStreamResource} that will use the same opener
   * for creating {@link InputStream}s
   */
  def buffered: BufferedRepr
}

object IoResource {
  // InputStream factory methods
  def inputStream(opener: => InputStream): InputStreamResource = new InputStreamResource(opener)
  def bufferedInputStream(opener: => BufferedInputStream): BufferedInputStreamResource = new BufferedInputStreamResource(opener)

  // OutputStream factory methods
  def outputStream(opener: => OutputStream): OutputStreamResource = new OutputStreamResource(opener)
  def bufferedOutputStream(opener: => BufferedOutputStream): BufferedOutputStreamResource = new BufferedOutputStreamResource(opener)

  // Reader factory methods
  def reader(opener: => Reader): ReaderResource = new ReaderResource(opener)
  def bufferedReader(opener: => BufferedReader): BufferedReaderResource = new BufferedReaderResource(opener)

  // Writer factory methods
  def writer(opener: => Writer): WriterResource = new WriterResource(opener)
  def bufferedWriter(opener: => BufferedWriter): BufferedWriterResource = new BufferedWriterResource(opener)

  // Channel factory methods
  def readableChannel(opener: => ReadableByteChannel): ReadableByteChannelResource = new ReadableByteChannelResource(opener)
  def writableChannel(opener: => WritableByteChannel): WritableByteChannelResource = new WritableByteChannelResource(opener)
  def byteChannel(opener: => ByteChannel): ByteChannelResource = new ByteChannelResource(opener)
  def fileChannel(opener: => FileChannel): FileChannelResource = new FileChannelResource(opener)

}
/***************************** InputStreamResource ************************************/
/**
 * A ManagedResource for accessing and using InputStreams.
 *
 * @see ManagedResource 
 */
class InputStreamResource(opener: => InputStream) 
extends BufferableResource[InputStream, BufferedInputStream, BufferedInputStreamResource] {
  def open() = opener
  val buffered = IoResource.bufferedInputStream(new BufferedInputStream(opener))
  def reader(implicit codec: Codec = Codec.default) = {
    IoResource.reader(new InputStreamReader(opener, codec.charSet))
  }
  lazy val channel: ReadableByteChannelResource = {
    IoResource.readableChannel(Channels.newChannel(open()))
  }
}

/**
 * A ManagedResource for accessing and using BufferedInputStreams.
 *
 * @see ManagedResource 
 */
class BufferedInputStreamResource(opener: => BufferedInputStream) 
extends IoResource[BufferedInputStream] {
  def open() = opener
  def reader(implicit codec:Codec = Codec.default) = {
    IoResource.reader(new InputStreamReader(opener, codec.charSet))
  }
}


/***************************** OutputStreamResource ************************************/

/**
 * A ManagedResource for accessing and using OutputStreams.
 *
 * @see ManagedResource 
 */
class OutputStreamResource(opener: => OutputStream) 
extends BufferableResource[OutputStream, BufferedOutputStream, BufferedOutputStreamResource] {
  def open() = opener
  val buffered = IoResource.bufferedOutputStream(new BufferedOutputStream(opener))
  def writer(implicit codec:Codec = 
    Codec.default) = IoResource.writer(new OutputStreamWriter(opener, codec.charSet))
  lazy val channel: WritableByteChannelResource = 
    IoResource.writableChannel(Channels.newChannel(open()))
}

/**
 * A ManagedResource for accessing and using BufferedOutputStreams.
 *
 * @see ManagedResource 
 */
class BufferedOutputStreamResource(opener: => BufferedOutputStream) 
extends IoResource[BufferedOutputStream] {
  def open() = opener
}


/***************************** ReaderResource ************************************/

/**
 * A ManagedResource for accessing and using Readers.
 *
 * @see ManagedResource 
 */
class ReaderResource(opener: => Reader) 
extends BufferableResource[Reader, BufferedReader, BufferedReaderResource] {
  def open() = opener
  val buffered = IoResource.bufferedReader(new BufferedReader(opener))
}

/**
 * A ManagedResource for accessing and using BufferedReaders.
 *
 * @see ManagedResource 
 */
class BufferedReaderResource(opener: => BufferedReader) extends IoResource[BufferedReader] {
  def open() = opener
}


/***************************** WriterResource ************************************/
/**
 * A ManagedResource for accessing and using Writers.
 *
 * @see ManagedResource 
 */
class WriterResource(opener: => Writer) extends BufferableResource[Writer, BufferedWriter, BufferedWriterResource] {
  def open() = opener
  val buffered = IoResource.bufferedWriter(new BufferedWriter(opener))
}

/**
 * A ManagedResource for accessing and using BufferedWriters.
 *
 * @see ManagedResource 
 */
class BufferedWriterResource(opener: => BufferedWriter) extends IoResource[BufferedWriter] {
  def open() = opener
}


/***************************** ByteChannelResource ************************************/
/**
 * A ManagedResource for accessing and using ByteChannels.
 *
 * @see ManagedResource 
 */
class ByteChannelResource(opener: => ByteChannel) extends IoResource[ByteChannel] {  
  def open() = opener
  lazy val inputStream = IoResource.inputStream(Channels.newInputStream(opener))
  lazy val outputStream = IoResource.outputStream(Channels.newOutputStream(opener))
}


/***************************** ReadableByteChannelResource ************************************/

/**
 * A ManagedResource for accessing and using ByteChannels.
 *
 * @see ManagedResource 
 */
class ReadableByteChannelResource(opener: => ReadableByteChannel) extends IoResource[ReadableByteChannel] {  
  def open() = opener
  lazy val inputStream = IoResource.inputStream(Channels.newInputStream(opener))
}


/***************************** WritableByteChannelResource ************************************/

/**
 * A ManagedResource for accessing and using ByteChannels.
 *
 * @see ManagedResource 
 */
class WritableByteChannelResource(opener: => WritableByteChannel) extends IoResource[WritableByteChannel] {  
  def open() = opener
  lazy val outputStream = IoResource.outputStream(Channels.newOutputStream(opener))
}


/***************************** FileChannelResource ************************************/
/**
 * A ManagedResource for accessing and using ByteChannels.
 *
 * @see ManagedResource 
 */
class FileChannelResource(opener: => FileChannel) extends IoResource[FileChannel] {  
  def open() = opener
}
