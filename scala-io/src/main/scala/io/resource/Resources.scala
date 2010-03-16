/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.resource

import scalax.io._
import scalax.resource.{
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
trait Resource[R <: Closeable] extends AbstractUntranslatedManagedResource[R] {
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
 * An Object that has an associated Buffered object. For example InputStream
 * has BufferedInputStream
 *
 * @param C
 *          The resource type
 * @param B
 *          they type of the buffered resource 
 * 
 * @author  Jesse Eichar
 * @since   1.0
 */
trait Bufferable[C <: Closeable, R <: Resource[C]] {
  /**
   * Obtain the buffered version of this object.
   *
   * @return the buffered version of this object
   */
  def buffered: R
}

/**
 * An object that can be converted to an input stream. For example
 * a ReadableByteChannel
 *
 * @param S
 *          the type of InputStream that is created
 * 
 * @author  Jesse Eichar
 * @since   1.0
 */
trait InputStreamable[S <: Resource[InputStream]] {
  /**
   * Obtain the InputStream Resource version of this object.
   *
   * @return the InputStream Resource version of this object.
   */
  def inputStream: S
}

/**
 * An object that can be converted to an output stream. For example
 * a WriteableByteChannel
 *
 * @param S
 *          the type of OutputStream that is created
 * 
 * @author  Jesse Eichar
 * @since   1.0
 */
trait OutputStreamable[S <: Resource[OutputStream]] {
  /**
   * Obtain the InputStream Resource version of this object.
   *
   * @return the InputStream Resource version of this object.
   */
  def outputStream: S
}

/**
 * An object that can be converted to a Reader. For example
 * an InputStream
 *
 * @param S
 *          the type of Reader that is created
 * 
 * @author  Jesse Eichar
 * @since   1.0
 */
trait Readable[S <: Resource[Reader]] {
  /**
   * Obtain the Reader Resource version of this object.
   *
   * @return the Reader Resource version of this object.
   */
  def reader(implicit codec: Codec): S
}

/**
 * An object that can be converted to a Writer. For example
 * an OutputStream
 *
 * @param S
 *          the type of Writer that is created
 * 
 * @author  Jesse Eichar
 * @since   1.0
 */
trait Writable[S <: Resource[Writer]] {
  /**
   * Obtain the Writer Resource version of this object.
   *
   * @return the Writer Resource version of this object.
   */
  def writer(implicit codec: Codec): S
}

/**
 * An object that can be converted to a WritableByteChannel. For example
 * an OutputStream
 *
 * @param S
 *          the type of WritableByteChannel that is created
 * 
 * @author  Jesse Eichar
 * @since   1.0
 */
trait WritableByteChannelable[S <: Resource[WritableByteChannel]] {
  /**
   * Obtain the Writer Resource version of this object.
   *
   * @return the Writer Resource version of this object.
   */
  def writableByteChannel: S
}

/**
 * An object that can be converted to a ReadableByteChannel. For example
 * an InputStream
 *
 * @param S
 *          the type of ReadableByteChannel that is created
 * 
 * @author  Jesse Eichar
 * @since   1.0
 */
trait ReadableByteChannelable[S <: Resource[ReadableByteChannel]] {
  /**
   * Obtain the Reader Resource version of this object.
   *
   * @return the Reader Resource version of this object.
   */
  def readableByteChannel: S
}


  /** A Resource with several conversion traits. */
  trait InputStreamResource extends Resource[InputStream] with Bufferable[InputStream, InputStreamResource]
      with Readable[ReaderResource] with ReadableByteChannelable[ReadableByteChannelResource]
      with ReadBytes
  /** A Resource with several conversion traits. */
  trait OutputStreamResource extends Resource[OutputStream] with Bufferable[OutputStream, OutputStreamResource]
      with Writable[WriterResource] with WritableByteChannelable[WritableByteChannelResource] 
      with WriteBytes
  /** A Resource with several conversion traits. */
  trait ReaderResource extends Resource[Reader] with Bufferable[Reader, ReaderResource]
      with ReadChars
  /** A Resource with several conversion traits. */
  trait WriterResource extends Resource[Writer] with Bufferable[Writer, WriterResource]
      with WriteChars
  /** A Resource with several conversion traits. */
  trait ReadableByteChannelResource extends Resource[ReadableByteChannel]
      with InputStreamable[InputStreamResource] with Readable[ReaderResource] 
      with ReadBytes
  /** A Resource with several conversion traits. */
  trait WritableByteChannelResource extends Resource[WritableByteChannel]
      with OutputStreamable[OutputStreamResource] with Writable[WriterResource]
      with WriteBytes
  /** A Resource with several conversion traits. */
  trait ByteChannelResource extends Resource[ByteChannel]
      with InputStreamable[InputStreamResource] with Readable[ReaderResource]
      with OutputStreamable[OutputStreamResource] with Writable[WriterResource]
      with ReadBytes with WriteBytes
  /** A Resource with several conversion traits. */
  trait FileChannelResource extends Resource[FileChannel]
      with InputStreamable[InputStreamResource] with Readable[ReaderResource]
      with OutputStreamable[OutputStreamResource] with Writable[WriterResource]
      with ReadBytes with WriteBytes

/**
 * Defines several factory methods for creating instances of Resource.
 * It also defines several useful Resource types. For example
 * ResourceType which is a Resource[Reader] with Bufferable[BufferedReader].
 * All the types that can be created with the factory methods can be created.
 * <p>
 * Example usage:
 * <pre><code>
 * val URL = new URL("http://scala-lang.org")
 * val resource: Resource[InputStream] = Resource.fromInputStream(url.openStream).buffered
 * </code></pre>
 * 
 * @author  Jesse Eichar
 * @since   1.0
 */
object Resource {

  // InputStream factory methods
  /**
   * Create an Resource instance with several conversion traits from an InputStream.
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * In other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * Resource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new InputStream
   * @return
   *          an InputStreamResource
   */
  def fromInputStream(opener: => InputStream): InputStreamResource = new InputStreamResourceImpl(opener)
  /**
   * Create an Resource instance from a BufferedInputStream
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * In other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * Resource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new BufferedInputStream
   *
   * @return
   *          a InputStreamResource that is backed by a BufferedInputStream
   */
  def fromBufferedInputStream(opener: => BufferedInputStream): InputStreamResource  = null // TODO

  // OutputStream factory methods
  /**
   * Create an Resource instance with several conversion traits from an OutputStream.
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * Out other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * Resource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new OutputStream
   *
   * @return
   *          an OutputStreamResource
   */
  def fromOutputStream(opener: => OutputStream): OutputStreamResource = new OutputStreamResourceImpl(opener)
  /**
   * Create an Resource instance from a BufferedOutputStream
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * Out other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * Resource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new BufferedOutputStream
   *
   * @return
   *          a OutputStreamResource that is backed by a BufferedOutputStream
   */
  def fromBufferedOutputStream(opener: => BufferedOutputStream): OutputStreamResource = null // TODO

  // Reader factory methods
  /**
   * Create an Resource instance with conversion traits from an Reader.
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * Out other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * Resource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new Reader
   *          this is only important if the stream may be converted to a
   * @param codec
   *          the codec representing the encoding of the strings that the Reader will Read
   *
   * @return
   *          an ReaderResource
   */
  def fromReader(opener: => Reader)(implicit codec: Codec): ReaderResource = new ReaderResourceImpl(opener, codec: Codec)
  /**
   * Create an Resource instance with conversion traits from an BufferedReader.
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * Out other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * Resource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new BufferedReader
   * @param codec
   *          the codec representing the encoding of the strings that the Reader will Read
   *
   * @return
   *          a ReaderResource that is backed by a BufferedReader
   */
  def fromBufferedReader(opener: => BufferedReader)(implicit codec: Codec): ReaderResource = null // TODO

  // Writer factory methods
  /**
   * Create an Resource instance with conversion traits from an Writer.
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * Out other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * Resource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new Writer
   * @param codec
   *          the codec representing the encoding of the strings that the writer will write out
   *
   * @return
   *          an WriterResource
   */
  def fromWriter(opener: => Writer)(implicit codec: Codec): WriterResource = new WriterResourceImpl(opener, codec)
  /**
   * Create an Resource instance with conversion traits from an BufferedWriter.
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * Out other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * Resource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new BufferedWriter
   * @param codec
   *          the codec representing the encoding of the strings that the writer will write out
   * @return
   *          a WriterResource that is backed by a BufferedWriter
   */
  def fromBufferedWriter(opener: => BufferedWriter)(implicit codec: Codec): WriterResource = null // TOOD

  // Channel factory methods
  /**
   * Create an Resource instance with several conversion traits from an ReadableByteChannel.
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * In other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * Resource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new ReadableByteChannel
   *
   * @return
   *          an ReadableByteChannelResource
   */
  def fromReadableByteChannel(opener: => ReadableByteChannel): ReadableByteChannelResource = new ReadableByteChannelResourceImpl(opener)
  /**
   * Create an Resource instance with several conversion traits from an WritableByteChannel.
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * In other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * Resource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new WritableByteChannel
   *
   * @return
   *          an WritableByteChannelResource
   */
  def fromWritableByteChannel(opener: => WritableByteChannel): WritableByteChannelResource = new WritableByteChannelResourceImpl(opener)
  /**
   * Create an Resource instance with several conversion traits from an ByteChannel.
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * In other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * Resource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new ByteChannel
   *
   * @return
   *          an ByteChannelResource
   */
  def fromByteChannel(opener: => ByteChannel): ByteChannelResource = new ByteChannelResourceImpl(opener)
  /**
   * Create an Resource instance with several conversion traits from an FileChannel.
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * In other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * Resource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new FileChannel
   *
   * @return
   *          an FileChannelResource
   */
  def fromFileChannel(opener: => FileChannel): FileChannelResource = new FileChannelResourceImpl(opener)
}

/***************************** InputStreamResource ************************************/
/**
 * A ManagedResource for accessing and using InputStreams.
 *
 * @see ManagedResource
 */
private[io] class InputStreamResourceImpl(opener: => InputStream) extends InputStreamResource {
  def open() = opener
  def buffered = Resource.fromBufferedInputStream(new BufferedInputStream(opener))
  def reader(implicit codec: Codec) =
    Resource.fromReader(new InputStreamReader(opener, codec.charSet))
  def readableByteChannel =
    Resource.fromReadableByteChannel(Channels.newChannel(open()))
  def bytesAsInts:Iterable[Int] = null // TODO
}

/***************************** OutputStreamResource ************************************/

/**
 * A ManagedResource for accessing and using OutputStreams.
 *
 * @see ManagedResource
 */
private[io] class OutputStreamResourceImpl(opener: => OutputStream) extends OutputStreamResource {
  def open() = opener
  protected def outputStream(openOptions : OpenOption*) = this
  def buffered = Resource.fromBufferedOutputStream(new BufferedOutputStream(opener))
  def writer(implicit codec:Codec) =
    Resource.fromWriter(new OutputStreamWriter(opener, codec.charSet))
  def writableByteChannel =
    Resource.fromWritableByteChannel(Channels.newChannel(open()))
}

/***************************** ReaderResource ************************************/

/**
 * A ManagedResource for accessing and using Readers.
 *
 * @see ManagedResource
 */
private[io] class ReaderResourceImpl(opener: => Reader, val sourceCodec:Codec) extends ReaderResource {
  def open() = opener
  def buffered = Resource.fromBufferedReader(new BufferedReader(opener))(sourceCodec)
  def withCodec(codec: Codec): ReaderResource = null // TODO
}

/***************************** WriterResource ************************************/
/**
 * A ManagedResource for accessing and using Writers.
 *
 * @see ManagedResource
 */
private[io] class WriterResourceImpl(opener: => Writer, val sourceCodec:Codec) extends WriterResource {
  def open() = opener
  def buffered = Resource.fromBufferedWriter(new BufferedWriter(opener))(sourceCodec)
  def withCodec(codec: Codec): WriterResource = null // TODO
}

/***************************** ByteChannelResource ************************************/
/**
 * A ManagedResource for accessing and using ByteChannels.
 *
 * @see ManagedResource
 */
private[io] class ByteChannelResourceImpl(opener: => ByteChannel) extends ByteChannelResource {
  def open() = opener
  protected def outputStream(openOptions : OpenOption*) = outputStream
  
  def inputStream = Resource.fromInputStream(Channels.newInputStream(opener))
  def outputStream = Resource.fromOutputStream(Channels.newOutputStream(opener))
  def reader(implicit codec: Codec) = Resource.fromReader(Channels.newReader(opener, codec.charSet.name()))
  def writer(implicit codec: Codec) = Resource.fromWriter(Channels.newWriter(opener, codec.charSet.name()))
  def bytesAsInts:Iterable[Int] = null // TODO
}


/***************************** ReadableByteChannelResource ************************************/

/**
 * A ManagedResource for accessing and using ByteChannels.
 *
 * @see ManagedResource
 */
private[io] class ReadableByteChannelResourceImpl(opener: => ReadableByteChannel) extends ReadableByteChannelResource {
  def open() = opener
  def inputStream = Resource.fromInputStream(Channels.newInputStream(opener))
  def reader(implicit codec:Codec) = Resource.fromReader(Channels.newReader(opener, codec.charSet.name()))
  def bytesAsInts:Iterable[Int] = null // TODO
}

/***************************** WritableByteChannelResource ************************************/

/**
 * A ManagedResource for accessing and using ByteChannels.
 *
 * @see ManagedResource
 */
private[io] class WritableByteChannelResourceImpl(opener: => WritableByteChannel) extends WritableByteChannelResource {
  def open() = opener
  protected def outputStream(openOptions : OpenOption*) = outputStream
  def outputStream = Resource.fromOutputStream(Channels.newOutputStream(opener))
  def writer(implicit codec:Codec) = Resource.fromWriter(Channels.newWriter(opener, codec.charSet.name()))
}

/***************************** FileChannelResource ************************************/
/**
 * A ManagedResource for accessing and using ByteChannels.
 *
 * @see ManagedResource
 */
private[io] class FileChannelResourceImpl(opener: => FileChannel) extends FileChannelResource {
  def open() = opener
  protected def outputStream(openOptions : OpenOption*) = outputStream
  def inputStream = Resource.fromInputStream(Channels.newInputStream(opener))
  def outputStream = Resource.fromOutputStream(Channels.newOutputStream(opener))
  def reader(implicit codec:Codec) = Resource.fromReader(Channels.newReader(opener, codec.charSet.name()))
  def writer(implicit codec:Codec) = Resource.fromWriter(Channels.newWriter(opener, codec.charSet.name()))
  def bytesAsInts:Iterable[Int] = null // TODO
}
