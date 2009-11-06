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
trait IoResource[R <: Closeable] extends AbstractUntranslatedManagedResource[R] {
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

trait Bufferable[B <: Closeable] {
  def buffered: IoResource[B]
}

trait InputStreamable[S <: InputStream, B <: Closeable] {
  def inputStream: IoResource[S] with Bufferable[B]
}

trait OutputStreamable[S <: OutputStream, B <: Closeable] {
  def outputStream: IoResource[S] with Bufferable[B]
}

trait Readable[S <: Reader, B <: Closeable] {
  def reader(implicit codec: Codec = Codec.default): IoResource[S] with Bufferable[B]
}

trait Writable[S <: Writer, B <: Closeable] {
  def writer(implicit codec: Codec = Codec.default): IoResource[S] with Bufferable[B]
}

trait WritableByteChannelable[S <: WritableByteChannel] {
  def writableByteChannel: IoResource[S]
}

trait ReadableByteChannelable[S <: ReadableByteChannel] {
  def readableByteChannel: IoResource[S]
}

object IoResource {
  type InputStreamResource = IoResource[InputStream] with Bufferable[BufferedInputStream]
      with Readable[Reader, BufferedReader] with ReadableByteChannelable[ReadableByteChannel]
  type BufferedInputStreamResource = IoResource[BufferedInputStream]
      with Readable[Reader, BufferedReader] with ReadableByteChannelable[ReadableByteChannel]
  type OutputStreamResource = IoResource[OutputStream] with Bufferable[BufferedOutputStream]
      with Writable[Writer, BufferedWriter] with WritableByteChannelable[WritableByteChannel]
  type BufferedOutputStreamResource = IoResource[BufferedOutputStream]
      with Writable[Writer, BufferedWriter] with WritableByteChannelable[WritableByteChannel]
  type ReaderResource = IoResource[Reader] with Bufferable[BufferedReader]
  type BufferedReaderResource = IoResource[BufferedReader]
  type WriterResource = IoResource[Writer] with Bufferable[BufferedWriter]
  type BufferedWriterResource = IoResource[BufferedWriter]
  type ReadableByteChannelResource = IoResource[ReadableByteChannel]
      with InputStreamable[InputStream, BufferedInputStream] with Readable[Reader, BufferedReader]
  type WritableByteChannelResource = IoResource[WritableByteChannel]
      with OutputStreamable[OutputStream, BufferedOutputStream] with Writable[Writer, BufferedWriter]
  type ByteChannelResource = IoResource[ByteChannel]
      with InputStreamable[InputStream, BufferedInputStream] with Readable[Reader, BufferedReader]
      with OutputStreamable[OutputStream, BufferedOutputStream] with Writable[Writer, BufferedWriter]
  type FileChannelResource = IoResource[FileChannel]
      with InputStreamable[InputStream, BufferedInputStream] with Readable[Reader, BufferedReader]
      with OutputStreamable[OutputStream, BufferedOutputStream] with Writable[Writer, BufferedWriter]

  // InputStream factory methods
  def fromInputStream(opener: => InputStream)(implicit codec: Codec = Codec.default): InputStreamResource = new InputStreamIoResource(opener,codec)
  def fromBufferedInputStream(opener: => BufferedInputStream)(implicit codec: Codec = Codec.default): BufferedInputStreamResource  = new BufferedInputStreamIoResource(opener,codec)

  // OutputStream factory methods
  def fromOutputStream(opener: => OutputStream)(implicit codec: Codec = Codec.default): OutputStreamResource = new OutputStreamIoResource(opener,codec)
  def fromBufferedOutputStream(opener: => BufferedOutputStream)(implicit codec: Codec = Codec.default): BufferedOutputStreamResource = new BufferedOutputStreamIoResource(opener,codec)

  // Reader factory methods
  def fromReader(opener: => Reader)(implicit codec: Codec = Codec.default): ReaderResource = new ReaderIoResource(opener)
  def fromBufferedReader(opener: => BufferedReader)(implicit codec: Codec = Codec.default): BufferedReaderResource = new BufferedReaderIoResource(opener)

  // Writer factory methods
  def fromWriter(opener: => Writer)(implicit codec: Codec = Codec.default): WriterResource = new WriterIoResource(opener)
  def fromBufferedWriter(opener: => BufferedWriter)(implicit codec: Codec = Codec.default): BufferedWriterResource = new BufferedWriterIoResource(opener)

  // Channel factory methods
  def fromReadableByteChannel(opener: => ReadableByteChannel)(implicit codec: Codec = Codec.default): ReadableByteChannelResource = new ReadableByteChannelIoResource(opener,codec)
  def fromWritableByteChannel(opener: => WritableByteChannel)(implicit codec: Codec = Codec.default): WritableByteChannelResource = new WritableByteChannelIoResource(opener,codec)
  def fromByteChannel(opener: => ByteChannel)(implicit codec: Codec = Codec.default): ByteChannelResource = new ByteChannelIoResource(opener,codec)
  def fromFileChannel(opener: => FileChannel)(implicit codec: Codec = Codec.default): FileChannelResource = new FileChannelIoResource(opener,codec)
}

/***************************** InputStreamResource ************************************/
/**
 * A ManagedResource for accessing and using InputStreams.
 *
 * @see ManagedResource
 */
protected[io] class InputStreamIoResource(opener: => InputStream, codec: Codec) extends IoResource[InputStream]
      with Bufferable[BufferedInputStream] with Readable[Reader, BufferedReader]
      with ReadableByteChannelable[ReadableByteChannel] {
  def open() = opener
  val buffered = IoResource.fromBufferedInputStream(new BufferedInputStream(opener))
  def reader(implicit codec: Codec = codec) =
    IoResource.fromReader(new InputStreamReader(opener, codec.charSet))
  lazy val readableByteChannel =
    IoResource.fromReadableByteChannel(Channels.newChannel(open()))
}

/**
 * A ManagedResource for accessing and using BufferedInputStreams.
 *
 * @see ManagedResource
 */
protected[io] class BufferedInputStreamIoResource(opener: => BufferedInputStream, codec: Codec) extends IoResource[BufferedInputStream]
      with Readable[Reader, BufferedReader] with ReadableByteChannelable[ReadableByteChannel] {
  def open() = opener
  def reader(implicit codec:Codec = codec) =
    IoResource.fromReader(new InputStreamReader(opener, codec.charSet))
  lazy val readableByteChannel = 
    IoResource.fromReadableByteChannel(Channels.newChannel(open()))
}

/***************************** OutputStreamResource ************************************/

/**
 * A ManagedResource for accessing and using OutputStreams.
 *
 * @see ManagedResource
 */
protected[io] class OutputStreamIoResource(opener: => OutputStream, codec: Codec) extends IoResource[OutputStream]
      with Bufferable[BufferedOutputStream] with Writable[Writer, BufferedWriter]
      with WritableByteChannelable[WritableByteChannel] {
  def open() = opener
  val buffered = IoResource.fromBufferedOutputStream(new BufferedOutputStream(opener))
  def writer(implicit codec:Codec = codec) =
    IoResource.fromWriter(new OutputStreamWriter(opener, codec.charSet))
  lazy val writableByteChannel =
    IoResource.fromWritableByteChannel(Channels.newChannel(open()))
}

/**
 * A ManagedResource for accessing and using BufferedOutputStreams.
 *
 * @see ManagedResource
 */
protected[io] class BufferedOutputStreamIoResource(opener: => BufferedOutputStream, codec: Codec) extends IoResource[BufferedOutputStream]
      with Writable[Writer, BufferedWriter] with WritableByteChannelable[WritableByteChannel] {
  def open() = opener
  def writer(implicit codec:Codec = codec) =
    IoResource.fromWriter(new OutputStreamWriter(opener, codec.charSet))
  lazy val writableByteChannel =
    IoResource.fromWritableByteChannel(Channels.newChannel(open()))
}


/***************************** ReaderResource ************************************/

/**
 * A ManagedResource for accessing and using Readers.
 *
 * @see ManagedResource
 */
protected[io] class ReaderIoResource(opener: => Reader) extends IoResource[Reader]
      with Bufferable[BufferedReader] {
  def open() = opener
  val buffered = IoResource.fromBufferedReader(new BufferedReader(opener))
}

/**
 * A ManagedResource for accessing and using BufferedReaders.
 *
 * @see ManagedResource
 */
protected[io] class BufferedReaderIoResource(opener: => BufferedReader) extends IoResource[BufferedReader] {
  def open() = opener
}

/***************************** WriterResource ************************************/
/**
 * A ManagedResource for accessing and using Writers.
 *
 * @see ManagedResource
 */
protected[io] class WriterIoResource(opener: => Writer) extends IoResource[Writer]
      with Bufferable[BufferedWriter] {
  def open() = opener
  val buffered = IoResource.fromBufferedWriter(new BufferedWriter(opener))
}

/**
 * A ManagedResource for accessing and using BufferedWriters.
 *
 * @see ManagedResource
 */
protected[io] class BufferedWriterIoResource(opener: => BufferedWriter) extends IoResource[BufferedWriter] {
  def open() = opener
}


/***************************** ByteChannelResource ************************************/
/**
 * A ManagedResource for accessing and using ByteChannels.
 *
 * @see ManagedResource
 */
protected[io] class ByteChannelIoResource(opener: => ByteChannel, codec: Codec) extends IoResource[ByteChannel]
      with InputStreamable[InputStream, BufferedInputStream] with Readable[Reader, BufferedReader]
      with OutputStreamable[OutputStream, BufferedOutputStream] with Writable[Writer, BufferedWriter]{
  def open() = opener
  lazy val inputStream = IoResource.fromInputStream(Channels.newInputStream(opener))
  lazy val outputStream = IoResource.fromOutputStream(Channels.newOutputStream(opener))
  def reader(implicit codec: Codec = codec) = IoResource.fromReader(Channels.newReader(opener, codec.charSet.name()))
  def writer(implicit codec: Codec = codec) = IoResource.fromWriter(Channels.newWriter(opener, codec.charSet.name()))
}


/***************************** ReadableByteChannelResource ************************************/

/**
 * A ManagedResource for accessing and using ByteChannels.
 *
 * @see ManagedResource
 */
protected[io] class ReadableByteChannelIoResource(opener: => ReadableByteChannel, codec: Codec) extends IoResource[ReadableByteChannel]
      with InputStreamable[InputStream, BufferedInputStream] with Readable[Reader, BufferedReader] {
  def open() = opener
  lazy val inputStream = IoResource.fromInputStream(Channels.newInputStream(opener))
  def reader(implicit codec:Codec = codec) = IoResource.fromReader(Channels.newReader(opener, codec.charSet.name()))
}

/***************************** WritableByteChannelResource ************************************/

/**
 * A ManagedResource for accessing and using ByteChannels.
 *
 * @see ManagedResource
 */
protected[io] class WritableByteChannelIoResource(opener: => WritableByteChannel, codec: Codec) extends IoResource[WritableByteChannel]
      with OutputStreamable[OutputStream, BufferedOutputStream] with Writable[Writer, BufferedWriter] {
  def open() = opener
  lazy val outputStream = IoResource.fromOutputStream(Channels.newOutputStream(opener))
  def writer(implicit codec:Codec = codec) = IoResource.fromWriter(Channels.newWriter(opener, codec.charSet.name()))
}

/***************************** FileChannelResource ************************************/
/**
 * A ManagedResource for accessing and using ByteChannels.
 *
 * @see ManagedResource
 */
protected[io] class FileChannelIoResource(opener: => FileChannel, codec: Codec) extends IoResource[FileChannel]
      with InputStreamable[InputStream, BufferedInputStream] with Readable[Reader, BufferedReader]
      with OutputStreamable[OutputStream, BufferedOutputStream] with Writable[Writer, BufferedWriter] {
  def open() = opener
  lazy val inputStream = IoResource.fromInputStream(Channels.newInputStream(opener))
  lazy val outputStream = IoResource.fromOutputStream(Channels.newOutputStream(opener))
  def reader(implicit codec:Codec = codec) = IoResource.fromReader(Channels.newReader(opener, codec.charSet.name()))
  def writer(implicit codec:Codec = codec) = IoResource.fromWriter(Channels.newWriter(opener, codec.charSet.name()))
}
