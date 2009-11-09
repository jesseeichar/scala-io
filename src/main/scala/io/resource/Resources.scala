/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.resource

import scalax.io._
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
 * 
 * @author  Jesse Eichar
 * @since   1.0
 */
trait IoResource[R <: {def close()}] extends AbstractUntranslatedManagedResource[R] {
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
 * An Object that has an associates Buffered object. For example InputStream
 * has BufferedInputStream
 *
 * @param B
 *          they type of the Buffered object
 * 
 * @author  Jesse Eichar
 * @since   1.0
 */
trait Bufferable[B <: {def close()}] {
  /**
   * Obtain the buffered version of this object.
   *
   * @return the buffered version of this object
   */
  def buffered: IoResource[B]
}

/**
 * An object that can be converted to an input stream. For example
 * a ReadableByteChannel
 *
 * @param S
 *          the type of InputStream that is created
 * @param B
 *          the type of buffered input stream that can also be created
 * 
 * @author  Jesse Eichar
 * @since   1.0
 */
trait InputStreamable[S <: InputStream, B <: BufferedInputStream] {
  /**
   * Obtain the InputStream IoResource version of this object.
   *
   * @return the InputStream IoResource version of this object.
   */
  def inputStream: IoResource[S] with Bufferable[B]
}

/**
 * An object that can be converted to an output stream. For example
 * a WriteableByteChannel
 *
 * @param S
 *          the type of OutputStream that is created
 * @param B
 *          the type of buffered output stream that can also be created
 * 
 * @author  Jesse Eichar
 * @since   1.0
 */
trait OutputStreamable[S <: OutputStream, B <: BufferedOutputStream] {
  /**
   * Obtain the InputStream IoResource version of this object.
   *
   * @return the InputStream IoResource version of this object.
   */
  def outputStream: IoResource[S] with Bufferable[B]
}

/**
 * An object that can be converted to a Reader. For example
 * an InputStream
 *
 * @param S
 *          the type of Reader that is created
 * @param B
 *          the type of BufferedReader that can also be created
 * 
 * @author  Jesse Eichar
 * @since   1.0
 */
trait Readable[S <: Reader, B <: BufferedReader] {
  /**
   * Obtain the Reader IoResource version of this object.
   *
   * @return the Reader IoResource version of this object.
   */
  def reader(implicit codec: Codec = Codec.default): IoResource[S] with Bufferable[B]
}

/**
 * An object that can be converted to a Writer. For example
 * an OutputStream
 *
 * @param S
 *          the type of Writer that is created
 * @param B
 *          the type of BufferedWriter that can also be created
 * 
 * @author  Jesse Eichar
 * @since   1.0
 */
trait Writable[S <: Writer, B <: BufferedWriter] {
  /**
   * Obtain the Writer IoResource version of this object.
   *
   * @return the Writer IoResource version of this object.
   */
  def writer(implicit codec: Codec = Codec.default): IoResource[S] with Bufferable[B]
}

/**
 * An object that can be converted to a WritableByteChannel. For example
 * an OutputStream
 *
 * @param S
 *          the type of WritableByteChannel that is created
 * @param B
 *          the type of BufferedWritableByteChannel that can also be created
 * 
 * @author  Jesse Eichar
 * @since   1.0
 */
trait WritableByteChannelable[S <: WritableByteChannel] {
  /**
   * Obtain the Writer IoResource version of this object.
   *
   * @return the Writer IoResource version of this object.
   */
  def writableByteChannel: IoResource[S]
}

/**
 * An object that can be converted to a ReadableByteChannel. For example
 * an InputStream
 *
 * @param S
 *          the type of ReadableByteChannel that is created
 * @param B
 *          the type of BufferedReadableByteChannel that can also be created
 * 
 * @author  Jesse Eichar
 * @since   1.0
 */
trait ReadableByteChannelable[S <: ReadableByteChannel] {
  /**
   * Obtain the Reader IoResource version of this object.
   *
   * @return the Reader IoResource version of this object.
   */
  def readableByteChannel: IoResource[S]
}

/**
 * Defines several factory methods for creating instances of IoResource.
 * It also defines several useful IoResource types. For example
 * ResourceType which is a IoResource[Reader] with Bufferable[BufferedReader].
 * All the types that can be created with the factory methods can be created.
 * <p>
 * Example usage:
 * <pre><code>
 * val URL = new URL("http://scala-lang.org")
 * val resource: IoResource[InputStream] = IoResource.fromInputStream(url.openStream).buffered
 * </code></pre>
 * 
 * @author  Jesse Eichar
 * @since   1.0
 */
object IoResource {

  /** A IoResource with several conversion traits. */
  type InputStreamResource = IoResource[InputStream] with Bufferable[BufferedInputStream]
      with Readable[Reader, BufferedReader] with ReadableByteChannelable[ReadableByteChannel]
  /** A IoResource with several conversion traits. */
  type BufferedInputStreamResource = IoResource[BufferedInputStream]
      with Readable[Reader, BufferedReader] with ReadableByteChannelable[ReadableByteChannel]
  /** A IoResource with several conversion traits. */
  type OutputStreamResource = IoResource[OutputStream] with Bufferable[BufferedOutputStream]
      with Writable[Writer, BufferedWriter] with WritableByteChannelable[WritableByteChannel]
  /** A IoResource with several conversion traits. */
  type BufferedOutputStreamResource = IoResource[BufferedOutputStream]
      with Writable[Writer, BufferedWriter] with WritableByteChannelable[WritableByteChannel]
  /** A IoResource with several conversion traits. */
  type ReaderResource = IoResource[Reader] with Bufferable[BufferedReader]
  /** A IoResource with several conversion traits. */
  type BufferedReaderResource = IoResource[BufferedReader]
  /** A IoResource with several conversion traits. */
  type WriterResource = IoResource[Writer] with Bufferable[BufferedWriter]
  /** A IoResource with several conversion traits. */
  type BufferedWriterResource = IoResource[BufferedWriter]
  /** A IoResource with several conversion traits. */
  type ReadableByteChannelResource = IoResource[ReadableByteChannel]
      with InputStreamable[InputStream, BufferedInputStream] with Readable[Reader, BufferedReader]
  /** A IoResource with several conversion traits. */
  type WritableByteChannelResource = IoResource[WritableByteChannel]
      with OutputStreamable[OutputStream, BufferedOutputStream] with Writable[Writer, BufferedWriter]
  /** A IoResource with several conversion traits. */
  type ByteChannelResource = IoResource[ByteChannel]
      with InputStreamable[InputStream, BufferedInputStream] with Readable[Reader, BufferedReader]
      with OutputStreamable[OutputStream, BufferedOutputStream] with Writable[Writer, BufferedWriter]
  /** A IoResource with several conversion traits. */
  type FileChannelResource = IoResource[FileChannel]
      with InputStreamable[InputStream, BufferedInputStream] with Readable[Reader, BufferedReader]
      with OutputStreamable[OutputStream, BufferedOutputStream] with Writable[Writer, BufferedWriter]

  // InputStream factory methods
  /**
   * Create an IoResource instance with several conversion traits from an InputStream.
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * In other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * IoResource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new InputStream
   * @param codec
   *          the codec representing the encoding of the underlying data
   *          this is only important if the stream may be converted to a
   *          IoResource[Reader]
   *          Default is Codec.default
   *
   * @return
   *          an InputStreamResource
   */
  def fromInputStream(opener: => InputStream)(implicit codec: Codec = Codec.default): InputStreamResource = new InputStreamIoResource(opener,codec)
  /**
   * Create an IoResource instance from a BufferedInputStream
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * In other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * IoResource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new BufferedInputStream
   * @param codec
   *          the codec representing the encoding of the underlying data
   *          this is only important if the stream may be converted to a
   *          IoResource[Reader]
   *          Default is Codec.default
   *
   * @return
   *          a BufferedInputStreamResource
   */
  def fromBufferedInputStream(opener: => BufferedInputStream)(implicit codec: Codec = Codec.default): BufferedInputStreamResource  = new BufferedInputStreamIoResource(opener,codec)

  // OutputStream factory methods
  /**
   * Create an IoResource instance with several conversion traits from an OutputStream.
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * Out other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * IoResource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new OutputStream
   * @param codec
   *          the codec representing the encoding of the underlying data
   *          this is only important if the stream may be converted to a
   *          IoResource[Writer]
   *          Default is Codec.default
   *
   * @return
   *          an OutputStreamResource
   */
  def fromOutputStream(opener: => OutputStream)(implicit codec: Codec = Codec.default): OutputStreamResource = new OutputStreamIoResource(opener,codec)
  /**
   * Create an IoResource instance from a BufferedOutputStream
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * Out other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * IoResource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new BufferedOutputStream
   *
   * @param codec
   *          the codec representing the encoding of the underlying data
   *          this is only important if the stream may be converted to a
   *          IoResource[Writer]
   *          Default is Codec.default
   *
   * @return
   *          a BufferedOutputStreamResource
   */
  def fromBufferedOutputStream(opener: => BufferedOutputStream)(implicit codec: Codec = Codec.default): BufferedOutputStreamResource = new BufferedOutputStreamIoResource(opener,codec)

  // Reader factory methods
  /**
   * Create an IoResource instance with conversion traits from an Reader.
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * Out other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * IoResource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new Reader
   *          this is only important if the stream may be converted to a
   *
   * @return
   *          an ReaderResource
   */
  def fromReader(opener: => Reader): ReaderResource = new ReaderIoResource(opener)
  /**
   * Create an IoResource instance with conversion traits from an BufferedReader.
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * Out other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * IoResource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new BufferedReader
   *
   * @return
   *          an BufferedReaderResource
   */
  def fromBufferedReader(opener: => BufferedReader): BufferedReaderResource = new BufferedReaderIoResource(opener)

  // Writer factory methods
  /**
   * Create an IoResource instance with conversion traits from an Writer.
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * Out other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * IoResource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new Writer
   *
   * @return
   *          an WriterResource
   */
  def fromWriter(opener: => Writer): WriterResource = new WriterIoResource(opener)
  /**
   * Create an IoResource instance with conversion traits from an BufferedReader.
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * Out other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * IoResource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new BufferedReader
   *
   * @return
   *          an BufferedReaderResource
   */
  def fromBufferedWriter(opener: => BufferedWriter): BufferedWriterResource = new BufferedWriterIoResource(opener)

  // Channel factory methods
  /**
   * Create an IoResource instance with several conversion traits from an ReadableByteChannel.
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * In other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * IoResource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new ReadableByteChannel
   * @param codec
   *          the codec representing the encoding of the underlying data
   *          this is only important if the stream may be converted to a
   *          IoResource[Reader]
   *          Default is Codec.default
   *
   * @return
   *          an ReadableByteChannelResource
   */
  def fromReadableByteChannel(opener: => ReadableByteChannel)(implicit codec: Codec = Codec.default): ReadableByteChannelResource = new ReadableByteChannelIoResource(opener,codec)
  /**
   * Create an IoResource instance with several conversion traits from an WritableByteChannel.
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * In other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * IoResource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new WritableByteChannel
   * @param codec
   *          the codec representing the encoding of the underlying data
   *          this is only important if the stream may be converted to a
   *          IoResource[Writer]
   *          Default is Codec.default
   *
   * @return
   *          an WritableByteChannelResource
   */
  def fromWritableByteChannel(opener: => WritableByteChannel)(implicit codec: Codec = Codec.default): WritableByteChannelResource = new WritableByteChannelIoResource(opener,codec)
  /**
   * Create an IoResource instance with several conversion traits from an ByteChannel.
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * In other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * IoResource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new ByteChannel
   * @param codec
   *          the codec representing the encoding of the underlying data
   *          this is only important if the stream may be converted to a
   *          IoResource[Writer] or IoResource[Reader]
   *          Default is Codec.default
   *
   * @return
   *          an ByteChannelResource
   */
  def fromByteChannel(opener: => ByteChannel)(implicit codec: Codec = Codec.default): ByteChannelResource = new ByteChannelIoResource(opener,codec)
  /**
   * Create an IoResource instance with several conversion traits from an FileChannel.
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * In other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * IoResource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new FileChannel
   * @param codec
   *          the codec representing the encoding of the underlying data
   *          this is only important if the stream may be converted to a
   *          IoResource[Writer] or IoResource[Reader]
   *          Default is Codec.default
   *
   * @return
   *          an FileChannelResource
   */
  def fromFileChannel(opener: => FileChannel)(implicit codec: Codec = Codec.default): FileChannelResource = new FileChannelIoResource(opener,codec)
}

/***************************** InputStreamResource ************************************/
/**
 * A ManagedResource for accessing and using InputStreams.
 *
 * @see ManagedResource
 */
private[io] class InputStreamIoResource(opener: => InputStream, codec: Codec) extends IoResource[InputStream]
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
private[io] class BufferedInputStreamIoResource(opener: => BufferedInputStream, codec: Codec) extends IoResource[BufferedInputStream]
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
private[io] class OutputStreamIoResource(opener: => OutputStream, codec: Codec) extends IoResource[OutputStream]
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
private[io] class BufferedOutputStreamIoResource(opener: => BufferedOutputStream, codec: Codec) extends IoResource[BufferedOutputStream]
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
private[io] class ReaderIoResource(opener: => Reader) extends IoResource[Reader]
      with Bufferable[BufferedReader] {
  def open() = opener
  val buffered = IoResource.fromBufferedReader(new BufferedReader(opener))
}

/**
 * A ManagedResource for accessing and using BufferedReaders.
 *
 * @see ManagedResource
 */
private[io] class BufferedReaderIoResource(opener: => BufferedReader) extends IoResource[BufferedReader] {
  def open() = opener
}

/***************************** WriterResource ************************************/
/**
 * A ManagedResource for accessing and using Writers.
 *
 * @see ManagedResource
 */
private[io] class WriterIoResource(opener: => Writer) extends IoResource[Writer]
      with Bufferable[BufferedWriter] {
  def open() = opener
  val buffered = IoResource.fromBufferedWriter(new BufferedWriter(opener))
}

/**
 * A ManagedResource for accessing and using BufferedWriters.
 *
 * @see ManagedResource
 */
private[io] class BufferedWriterIoResource(opener: => BufferedWriter) extends IoResource[BufferedWriter] {
  def open() = opener
}


/***************************** ByteChannelResource ************************************/
/**
 * A ManagedResource for accessing and using ByteChannels.
 *
 * @see ManagedResource
 */
private[io] class ByteChannelIoResource(opener: => ByteChannel, codec: Codec) extends IoResource[ByteChannel]
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
private[io] class ReadableByteChannelIoResource(opener: => ReadableByteChannel, codec: Codec) extends IoResource[ReadableByteChannel]
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
private[io] class WritableByteChannelIoResource(opener: => WritableByteChannel, codec: Codec) extends IoResource[WritableByteChannel]
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
private[io] class FileChannelIoResource(opener: => FileChannel, codec: Codec) extends IoResource[FileChannel]
      with InputStreamable[InputStream, BufferedInputStream] with Readable[Reader, BufferedReader]
      with OutputStreamable[OutputStream, BufferedOutputStream] with Writable[Writer, BufferedWriter] {
  def open() = opener
  lazy val inputStream = IoResource.fromInputStream(Channels.newInputStream(opener))
  lazy val outputStream = IoResource.fromOutputStream(Channels.newOutputStream(opener))
  def reader(implicit codec:Codec = codec) = IoResource.fromReader(Channels.newReader(opener, codec.charSet.name()))
  def writer(implicit codec:Codec = codec) = IoResource.fromWriter(Channels.newWriter(opener, codec.charSet.name()))
}
