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
 * Factory class/object for creating Resource objects
 *
 * @param T
 *          The type of object that the ManagedResource will wrap
 * @param R
 *          The type of ManagedResource that will be returned
 */
protected[io] trait IoResourceFactory[I,R <: ManagedResource[I]] {
  /**
   * Create an InputStreamResource from a function that creates an inputStream
   * <p>
   * It is important to remember that if the resource is to be reusable the function should
   * return a new instance each time
   * </p>
   * Discouraged code:
   * <pre><code>
   * val stream = new FileInputStream("filename")
   * InputStreamResource(stream)
   * </code></pre>
   * This is discouraged because the resulting InputStreamResource can only be used once
   * before the stream is finished.
   * <p>
   * The recommended Example:
   * <pre><code>
   * InputStreamResource(new FileInputStream("filename")
   * </code></pre>
   * Using this code snippet the resource can be reused several times because the resource
   * has the ability to open a new stream.
   */
  def apply(opener: => I): R 
}

/**
 * Factory class/object for creating both a non-buffered and buffered instances of Resource objects
 * @param T
 *          The type of object that the ManagedResource will wrap
 * @param R
 *          The type of ManagedResource that will be returned
 */
protected[io] trait BufferedIoResourceFactory[T,R <: ManagedResource[T]] {
  /**
   * Creates a buffered resource from the opener.
   *
   * @see IoResourceFactory#apply
   */
  def buffered(opener: => T): R
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
   * The factory for creating the BufferedVersion
   */
  protected val companion: BufferedIoResourceFactory[O,BufferedRepr]
  /**
   * Create a {@link BufferedInputStreamResource} that will use the same opener
   * for creating {@link InputStream}s
   */
  lazy val buffered: BufferedRepr = companion.buffered(open())
}


/***************************** InputStreamResource ************************************/

/**
 * Contains factory methods for creating InputStreamResources
 */
object InputStreamResource 
extends BufferedIoResourceFactory[InputStream, BufferedInputStreamResource] 
with IoResourceFactory[InputStream, InputStreamResource]{
  def apply(opener: => InputStream): InputStreamResource = new InputStreamResource(opener)
  def buffered(opener: => InputStream): BufferedInputStreamResource = {
    new BufferedInputStreamResource(new BufferedInputStream(opener))
  }
}

/**
 * A ManagedResource for accessing and using InputStreams.
 *
 * @see ManagedResource 
 */
class InputStreamResource(opener: => InputStream) 
extends BufferableResource[InputStream, BufferedInputStream, BufferedInputStreamResource] {
  def open() = opener
  val companion = InputStreamResource
  def reader(implicit codec: Codec = Codec.default) = {
    ReaderResource(new InputStreamReader(opener, codec.charSet))
  }
  lazy val channel: ReadableByteChannelResource = {
    ReadableByteChannelResource(Channels.newChannel(open()))
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
    ReaderResource.buffered(new InputStreamReader(opener, codec.charSet))
  }
}


/***************************** OutputStreamResource ************************************/

/**
 * Contains factory methods for creating OutputStreamResources
 */
object OutputStreamResource 
extends BufferedIoResourceFactory[OutputStream, BufferedOutputStreamResource] 
with IoResourceFactory[OutputStream, OutputStreamResource]{
  def apply(opener: => OutputStream): OutputStreamResource = new OutputStreamResource(opener)
  def buffered(opener: => OutputStream): BufferedOutputStreamResource = new BufferedOutputStreamResource(new BufferedOutputStream(opener))
}

/**
 * A ManagedResource for accessing and using OutputStreams.
 *
 * @see ManagedResource 
 */
class OutputStreamResource(opener: => OutputStream) 
extends BufferableResource[OutputStream, BufferedOutputStream, BufferedOutputStreamResource] {
  def open() = opener
  val companion = OutputStreamResource
  def writer(implicit codec:Codec = 
    Codec.default) = WriterResource(new OutputStreamWriter(opener, codec.charSet))
  lazy val channel: WritableByteChannelResource = 
    WritableByteChannelResource(Channels.newChannel(open()))
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
 * Contains factory methods for creating ReaderResources
 */
object ReaderResource extends BufferedIoResourceFactory[Reader, BufferedReaderResource] 
                      with IoResourceFactory[Reader, ReaderResource]{
  def apply(opener: => Reader): ReaderResource = new ReaderResource(opener)
  def buffered(opener: => Reader): BufferedReaderResource = new BufferedReaderResource(new BufferedReader(opener))
}

/**
 * A ManagedResource for accessing and using Readers.
 *
 * @see ManagedResource 
 */
class ReaderResource(opener: => Reader) 
extends BufferableResource[Reader, BufferedReader, BufferedReaderResource] {
  def open() = opener
  val companion = ReaderResource
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
 * Contains factory methods for creating WriterResources
 */
object WriterResource extends BufferedIoResourceFactory[Writer, BufferedWriterResource] 
                      with IoResourceFactory[Writer, WriterResource]{
  def apply(opener: => Writer): WriterResource = new WriterResource(opener)
  def buffered(opener: => Writer): BufferedWriterResource = 
    new BufferedWriterResource(new BufferedWriter(opener))
}

/**
 * A ManagedResource for accessing and using Writers.
 *
 * @see ManagedResource 
 */
class WriterResource(opener: => Writer) extends BufferableResource[Writer, BufferedWriter, BufferedWriterResource] {
  def open() = opener
  val companion = WriterResource
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
 * Contains factory methods for creating ByteChannelResources
 */
object ByteChannelResource extends IoResourceFactory[ByteChannel, ByteChannelResource]{
  def apply(opener: => ByteChannel): ByteChannelResource = new ByteChannelResource(opener)
}

/**
 * A ManagedResource for accessing and using ByteChannels.
 *
 * @see ManagedResource 
 */
class ByteChannelResource(opener: => ByteChannel) extends IoResource[ByteChannel] {  
  def open() = opener
}


/***************************** ReadableByteChannelResource ************************************/

/**
 * Contains factory methods for creating ReadableByteChannelResources
 */
object ReadableByteChannelResource extends IoResourceFactory[ReadableByteChannel, ReadableByteChannelResource]{
  def apply(opener: => ReadableByteChannel): ReadableByteChannelResource = new ReadableByteChannelResource(opener)
}

/**
 * A ManagedResource for accessing and using ByteChannels.
 *
 * @see ManagedResource 
 */
class ReadableByteChannelResource(opener: => ReadableByteChannel) extends IoResource[ReadableByteChannel] {  
  def open() = opener
}


/***************************** WritableByteChannelResource ************************************/

/**
 * Contains factory methods for creating WritableByteChannelResources
 */
object WritableByteChannelResource extends IoResourceFactory[WritableByteChannel, WritableByteChannelResource]{
  def apply(opener: => WritableByteChannel): WritableByteChannelResource = new WritableByteChannelResource(opener)
}

/**
 * A ManagedResource for accessing and using ByteChannels.
 *
 * @see ManagedResource 
 */
class WritableByteChannelResource(opener: => WritableByteChannel) extends IoResource[WritableByteChannel] {  
  def open() = opener
}


/***************************** FileChannelResource ************************************/

/**
 * Contains factory methods for creating FileChannelResources
 */
object FileChannelResource extends IoResourceFactory[FileChannel, FileChannelResource]{
  def apply(opener: => FileChannel): FileChannelResource = new FileChannelResource(opener)
}

/**
 * A ManagedResource for accessing and using ByteChannels.
 *
 * @see ManagedResource 
 */
class FileChannelResource(opener: => FileChannel) extends IoResource[FileChannel] {  
  def open() = opener
}

