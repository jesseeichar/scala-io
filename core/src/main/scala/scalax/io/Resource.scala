/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import _root_.resource.{ManagedResourceOperations}
import java.nio.channels.{
  ByteChannel, ReadableByteChannel, WritableByteChannel,
  Channels
}
import Closer.Noop
import java.io._
import nio.SeekableFileChannel
import java.net.URL

/**
 * A Resource that can be used to do IO.  It wraps objects from the java.io package
 *
 * @param R
 *          The type of the resource that will be managed by the ManagedResource
 *
 * @author  Jesse Eichar
 * @since   1.0
 */
trait Resource[+R <: Closeable] extends ManagedResourceOperations[R] {

    protected def closer:Closer[R] = Noop
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

    def acquireFor[B](f : R => B) : Either[List[Throwable], B] = {

        val resource = open()

        var exceptions = List[Throwable]()
        val result = try {
            Some(f(resource))
        } catch {
            case e =>
                exceptions ::= e
                None
        } finally {
            exceptions ++= (Closer(resource.close()) ++ closer)(resource)
        }

        result match {
            case Some(r) => Right(r)
            case None => Left(exceptions)
        }
    }

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
trait Bufferable[R <: Resource[Closeable]] {
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
trait InputResource[+R <: Closeable] extends Resource[R] with Input {

    /**
     * Obtain the InputStream Resource version of this object.
     *
     * @return the InputStream Resource version of this object.
     */
    def inputStream: InputResource[InputStream]

    /**
     * Obtain the Reader Resource version of this object.
     *
     * @return the Reader Resource version of this object.
     */
    def reader(implicit sourceCodec: Codec) : ReadCharsResource[Reader]
    /**
     * Obtain the Reader Resource version of this object.
     *
     * @return the Reader Resource version of this object.
     */
    def readableByteChannel: InputResource[ReadableByteChannel]
    def size : Option[Long] = None
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
trait ReadCharsResource[+R <: Closeable] extends Resource[R] with ReadChars

/**
 * An object that can be converted to an output stream. For example
 * a WritableByteChannel
 *
 * @param S
 *          the type of OutputStream that is created
 *
 * @author  Jesse Eichar
 * @since   1.0
 */
trait OutputResource[+R <: Closeable] extends Resource[R] with Output {
  /**
   * Obtain the InputStream Resource version of this object.
   *
   * @return the InputStream Resource version of this object.
   */
  def outputStream: OutputResource[OutputStream]
  /**
   * Obtain the Writer Resource version of this object.
   *
   * @return the Writer Resource version of this object.
   */
  def writer(implicit sourceCodec: Codec) : WriteCharsResource[Writer]
  /**
   * Obtain the Writer Resource version of this object.
   *
   * @return the Writer Resource version of this object.
   */
  def writableByteChannel: OutputResource[WritableByteChannel]
}

/**
 * An object that can be converted to an Seekable object. For example
 * a FileChannel wrapped by a SeekableByteChannel
 *
 * @param S
 *          the type of OutputStream that is created
 *
 * @author  Jesse Eichar
 * @since   1.0
 */
trait SeekableResource[+R <: Closeable] extends Seekable with InputResource[R] with OutputResource[R]

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
trait WriteCharsResource[+R <: Closeable] extends Resource[R] with WriteChars

trait BufferableInputResource[+C <: Closeable, B <: Closeable] extends InputResource[C] with Bufferable[InputResource[B]]
trait BufferableOutputResource[+C <: Closeable, B <: Closeable] extends OutputResource[C] with Bufferable[OutputResource[B]]
trait BufferableReadCharsResource[+C <: Closeable, B <: Closeable] extends ReadCharsResource[C] with Bufferable[ReadCharsResource[B]]
trait BufferableWriteCharsResource[+C <: Closeable, B <: Closeable] extends WriteCharsResource[C] with Bufferable[WriteCharsResource[B]]

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
   * Create an Input Resource instance with several conversion traits from an InputStream.
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
   *          an InputResource
   */
  def fromInputStream[A <: InputStream](opener: => A)(implicit extraCloser:Closer[A]=Noop) : InputStreamResource[A] = new InputStreamResource[A](opener, extraCloser)
  /**
   * Create an Input Resource instance from a BufferedInputStream
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
  def fromBufferedInputStream[A <: BufferedInputStream](opener: => A)(implicit extraCloser:Closer[A]=Noop) : InputStreamResource[A]  = new InputStreamResource[A](opener, extraCloser){
      override def buffered = this;
  }

  // OutputStream factory methods
  /**
   * Create an Output Resource instance with several conversion traits from an OutputStream.
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
  def fromOutputStream[A <: OutputStream](opener: => A)(implicit extraCloser:Closer[A]=Noop) : OutputStreamResource[A] = new OutputStreamResource[A](opener,extraCloser)
  /**
   * Create an Output Resource instance from a BufferedOutputStream
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
  def fromBufferedOutputStream[A <: BufferedOutputStream](opener: => A)(implicit extraCloser:Closer[A]=Noop) : OutputStreamResource[A] = new OutputStreamResource[A](opener,extraCloser) {
    override def buffered = this
  }

  // Reader factory methods
  /**
   * Create an Input Resource instance with conversion traits from an Reader.
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
  def fromReader[A <: Reader](opener: => A)(implicit extraCloser:Closer[A]=Noop) : ReaderResource[A] = new ReaderResource[A](opener, extraCloser)
  /**
   * Create an Input Resource instance with conversion traits from an BufferedReader.
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
  def fromBufferedReader[A <: BufferedReader](opener: => A)(implicit extraCloser:Closer[A]=Noop): ReaderResource[A] = new ReaderResource[A](opener,extraCloser) {
      override def buffered = this
  }

  // Writer factory methods
  /**
   * Create an Output Resource instance with conversion traits from an Writer.
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
  def fromWriter[A <: Writer](opener: => A)(implicit extraCloser:Closer[A]=Noop) : WriterResource[A] = new WriterResource[A](opener,extraCloser)
  /**
   * Create an Output Resource instance with conversion traits from an BufferedWriter.
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
  def fromBufferedWriter[A <: BufferedWriter](opener: => A)(implicit extraCloser:Closer[A]=Noop): WriterResource[A] = new WriterResource[A](opener, extraCloser) {
      override def buffered = this
  }

  // Channel factory methods
  /**
   * Create an Input Resource instance with several conversion traits from an ReadableByteChannel.
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
  def fromReadableByteChannel[A <: ReadableByteChannel](opener: => A)(implicit extraCloser:Closer[A]=Noop) : ReadableByteChannelResource[A] = new ReadableByteChannelResource[A](opener, extraCloser)
  /**
   * Create an Output Resource instance with several conversion traits from an WritableByteChannel.
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
  def fromWritableByteChannel[A <: WritableByteChannel](opener: => A)(implicit extraCloser:Closer[A]=Noop) : WritableByteChannelResource[A] = new WritableByteChannelResource[A](opener,extraCloser)
  /**
   * Create an Input/Output Resource instance with several conversion traits from an ByteChannel.
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
  def fromByteChannel[A <: ByteChannel](opener: => A)(implicit extraCloser:Closer[A]=Noop) : ByteChannelResource[A] = new ByteChannelResource[A](opener,extraCloser)
  /**
   * Create an Input/Output/Seekable Resource instance with several conversion traits from an SeekableByteChannel.
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * In other words it is important to try and pass in a function for opening
   * the stream rather than the already opened stream so that the returned
   * Resource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new SeekableByteChannel
   *
   * @return
   *          an ByteChannelResource
   */
  def fromSeekableByteChannel[A <: SeekableByteChannel](opener: => A)(implicit extraCloser:Closer[A]=Noop) : SeekableByteChannelResource[A] = new SeekableByteChannelResource[A](opener,extraCloser)
  /**
   * Create an Input/Output/Seekable Resource instance with several conversion traits from an RandomAccess file.
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * In other words it is important to try and pass in a function for opening
   * the stream rather than the already opened stream so that the returned
   * Resource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new SeekableByteChannel
   *
   * @return
   *          an ByteChannelResource
   */
  def fromRandomAccessFile(opener: => RandomAccessFile)(implicit extraCloser:Closer[SeekableFileChannel]=Noop) : SeekableByteChannelResource[SeekableFileChannel] = {
    def open = new SeekableFileChannel(opener.getChannel)
    new SeekableByteChannelResource[SeekableFileChannel](open,extraCloser)
  }

  def fromURL(url:URL): InputStreamResource[InputStream] = fromInputStream(url.openStream)
  def fromURL(url:String): InputStreamResource[InputStream] = fromURL(new URL(url))
  def fromFile(file:File): SeekableByteChannelResource[SeekableByteChannel] = fromRandomAccessFile(new RandomAccessFile(file,"rw"))
  def fromFile(file:String): SeekableByteChannelResource[SeekableByteChannel] = fromRandomAccessFile(new RandomAccessFile(file,"rw"))
}

/***************************** InputStreamResource ************************************/
/**
 * A ManagedResource for accessing and using InputStreams.
 *
 * @see ManagedResource
 */
class InputStreamResource[+A <: InputStream](opener: => A, override protected val closer:Closer[A]) extends BufferableInputResource[A, BufferedInputStream] {
    def open() = opener

    def inputStream = this
    def buffered = Resource.fromBufferedInputStream(new BufferedInputStream(opener))
    def reader(implicit sourceCodec: Codec): ReaderResource[Reader] = Resource.fromReader(new InputStreamReader(opener, sourceCodec.charSet))
    def readableByteChannel = Resource.fromReadableByteChannel(Channels.newChannel(open()))
    def chars(implicit codec: Codec) = reader(codec).chars

    def bytesAsInts : ResourceView[Int] = ResourceTraversable.streamBased(this).view
}

/***************************** OutputStreamResource ************************************/

/**
 * A ManagedResource for accessing and using OutputStreams.
 *
 * @see ManagedResource
 */
class OutputStreamResource[+A <: OutputStream](opener: => A, override protected val closer:Closer[A]) extends BufferableOutputResource[A, BufferedOutputStream] {
    def open() = opener

    def outputStream = this
    def underlyingOutput = this
    def buffered = Resource.fromBufferedOutputStream(new BufferedOutputStream(opener))
    def writer(implicit sourceCodec: Codec): WriterResource[Writer] = Resource.fromWriter(new OutputStreamWriter(opener, sourceCodec.charSet))
    def writableByteChannel = Resource.fromWritableByteChannel(Channels.newChannel(open()))
}

/***************************** ReaderResource ************************************/

/**
 * A ManagedResource for accessing and using Readers.
 *
 * @see ManagedResource
 */
class ReaderResource[+A <: Reader](opener: => A, override protected val closer:Closer[A]) extends BufferableReadCharsResource[A, BufferedReader] {
    def open() = opener

    def buffered = Resource.fromBufferedReader(new BufferedReader(opener))

    override def chars : ResourceView[Char]= ResourceTraversable.readerBased(this).view
}

/***************************** WriterResource ************************************/
/**
 * A ManagedResource for accessing and using Writers.
 *
 * @see ManagedResource
 */
class WriterResource[+A <: Writer](opener: => A, override protected val closer:Closer[A]) extends BufferableWriteCharsResource[A, BufferedWriter] {
    def open() = opener

    def buffered = Resource.fromBufferedWriter(new BufferedWriter(opener))

    protected def writer = this
}

/***************************** ByteChannelResource ************************************/
/**
 * A ManagedResource for accessing and using ByteChannels.
 *
 * @see ManagedResource
 */
class ByteChannelResource[+A <: ByteChannel](opener: => A, override protected val closer:Closer[A]) extends InputResource[A] with OutputResource[A] {
    def open() = opener

    def inputStream = Resource.fromInputStream(Channels.newInputStream(opener))
    def outputStream = Resource.fromOutputStream(Channels.newOutputStream(opener))
    def underlyingOutput = outputStream
    def reader(implicit sourceCodec: Codec) = Resource.fromReader(Channels.newReader(opener, sourceCodec.charSet.name()))
    def writer(implicit sourceCodec: Codec) = Resource.fromWriter(Channels.newWriter(opener, sourceCodec.charSet.name()))
    def writableByteChannel = Resource.fromWritableByteChannel(opener)
    def readableByteChannel = Resource.fromReadableByteChannel(opener)

    def bytesAsInts = inputStream.bytesAsInts // TODO optimize for byteChannel
    def chars(implicit codec: Codec) = reader(codec).chars  // TODO optimize for byteChannel
}

/***************************** SeekableByteChannelResource ************************************/
/**
 * A ManagedResource for accessing and using SeekableByteChannels.
 *
 * @see ManagedResource
 */
class SeekableByteChannelResource[+A <: SeekableByteChannel](opener: => A, override protected val closer:Closer[A]) extends SeekableResource[A] {
    def open() = opener

    def inputStream = Resource.fromInputStream(Channels.newInputStream(opener))
    def outputStream = Resource.fromOutputStream(Channels.newOutputStream(opener))
    def reader(implicit sourceCodec: Codec) = Resource.fromReader(Channels.newReader(opener, sourceCodec.charSet.name()))
    def writer(implicit sourceCodec: Codec) = Resource.fromWriter(Channels.newWriter(opener, sourceCodec.charSet.name()))
    def writableByteChannel = Resource.fromWritableByteChannel(opener)
    def readableByteChannel = Resource.fromReadableByteChannel(opener)
    def byteChannel = Resource.fromByteChannel(opener)

    protected def channel(openOptions:OpenOption*) = this
  }


/***************************** ReadableByteChannelResource ************************************/

/**
 * A ManagedResource for accessing and using ByteChannels.
 *
 * @see ManagedResource
 */
class ReadableByteChannelResource[+A <: ReadableByteChannel](opener: => A, override protected val closer:Closer[A]) extends BufferableInputResource[A, BufferedInputStream] {
    def open() = opener

    def buffered = inputStream.buffered
    def inputStream = Resource.fromInputStream(Channels.newInputStream(opener))
    def reader(implicit sourceCodec: Codec) = Resource.fromReader(Channels.newReader(opener, sourceCodec.charSet.name()))
    def readableByteChannel = this
    def bytesAsInts = inputStream.bytesAsInts // TODO optimize for byteChannel
    def chars(implicit codec: Codec) = reader(codec).chars  // TODO optimize for byteChannel
}

/***************************** WritableByteChannelResource ************************************/

/**
 * A ManagedResource for accessing and using ByteChannels.
 *
 * @see ManagedResource
 */
class WritableByteChannelResource[+A <: WritableByteChannel](opener: => A, override protected val closer:Closer[A]) extends BufferableOutputResource[A, BufferedOutputStream] {
    def open() = opener

    def buffered = outputStream.buffered
    def outputStream = Resource.fromOutputStream(Channels.newOutputStream(opener))
    def underlyingOutput = outputStream
    def writer(implicit sourceCodec: Codec) = Resource.fromWriter(Channels.newWriter(opener, sourceCodec.charSet.name()))
    def writableByteChannel = this
}

