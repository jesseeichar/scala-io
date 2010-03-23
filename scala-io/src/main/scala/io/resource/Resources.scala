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
  ManagedResource, ManagedResourceOperations
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
import java.util.logging.{
    Logger, Level
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
trait Resource[+R <: Closeable] extends ManagedResourceOperations[R] {
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
            try {
                resource.close()
            } catch {
                case e => 
                    exceptions ::= e
            }
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
    def reader : InputResource[Reader]
    /**
     * Obtain the Reader Resource version of this object.
     *
     * @return the Reader Resource version of this object.
     */
    def readableByteChannel: InputResource[ReadableByteChannel]
}

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
  def writer : OutputResource[Writer]
  /**
   * Obtain the Writer Resource version of this object.
   *
   * @return the Writer Resource version of this object.
   */
  def writableByteChannel: OutputResource[WritableByteChannel]
}

trait BufferableInputResource[+C <: Closeable, B <: Closeable] extends InputResource[C] with Bufferable[InputResource[B]]
trait BufferableOutputResource[+C <: Closeable, B <: Closeable] extends OutputResource[C] with Bufferable[OutputResource[B]]

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
  def fromInputStream[A <: InputStream](opener: => A)(implicit codec: Codec) : InputStreamResource[A] = new InputStreamResource[A](opener, codec)
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
  def fromBufferedInputStream[A <: BufferedInputStream](opener: => A)(implicit codec: Codec) : InputStreamResource[A]  = new InputStreamResource[A](opener, codec){
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
  def fromOutputStream[A <: OutputStream](opener: => A)(implicit codec: Codec) : OutputStreamResource[A] = new OutputStreamResource[A](opener, codec)
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
  def fromBufferedOutputStream[A <: BufferedOutputStream](opener: => A)(implicit codec: Codec) : OutputStreamResource[A] = new OutputStreamResource[A](opener, codec) {
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
  def fromReader[A <: Reader](opener: => A)(implicit codec: Codec) : ReaderResource[A] = new ReaderResource[A](opener, codec)
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
  def fromBufferedReader[A <: BufferedReader](opener: => A)(implicit codec: Codec): ReaderResource[A] = new ReaderResource[A](opener, codec) {
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
  def fromWriter[A <: Writer](opener: => A)(implicit codec: Codec) : WriterResource[A] = new WriterResource[A](opener, codec)
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
  def fromBufferedWriter[A <: BufferedWriter](opener: => A)(implicit codec: Codec): WriterResource[A] = new WriterResource[A](opener, codec) {
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
  def fromReadableByteChannel[A <: ReadableByteChannel](opener: => A)(implicit codec: Codec) : ReadableByteChannelResource[A] = new ReadableByteChannelResource[A](opener, codec)
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
  def fromWritableByteChannel[A <: WritableByteChannel](opener: => A)(implicit codec: Codec) : WritableByteChannelResource[A] = new WritableByteChannelResource[A](opener, codec)
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
  def fromByteChannel[A <: ByteChannel](opener: => A)(implicit codec: Codec) : ByteChannelResource[A] = new ByteChannelResource[A](opener, codec)
}

/***************************** InputStreamResource ************************************/
/**
 * A ManagedResource for accessing and using InputStreams.
 *
 * @see ManagedResource
 */
class InputStreamResource[+A <: InputStream](opener: => A, val sourceCodec: Codec) extends BufferableInputResource[A, BufferedInputStream] {
    implicit val codec = getCodec()
    def open() = opener

    def inputStream = this
    def buffered = Resource.fromBufferedInputStream(new BufferedInputStream(opener))
    def reader = Resource.fromReader(new InputStreamReader(opener, sourceCodec.charSet))
    def readableByteChannel = Resource.fromReadableByteChannel(Channels.newChannel(open()))

    def bytesAsInts:Traversable[Int] = toTraversable {in => StreamIterator(in)}
    def withCodec(codec: Codec) = Resource.fromInputStream(opener)(codec)
}

/***************************** OutputStreamResource ************************************/

/**
 * A ManagedResource for accessing and using OutputStreams.
 *
 * @see ManagedResource
 */
class OutputStreamResource[+A <: OutputStream](opener: => A, val sourceCodec: Codec) extends BufferableOutputResource[A, BufferedOutputStream] {
    implicit val codec = getCodec()
    def open() = opener

    def outputStream = this
    def buffered = Resource.fromBufferedOutputStream(new BufferedOutputStream(opener))
    def writer = Resource.fromWriter(new OutputStreamWriter(opener, sourceCodec.charSet))
    def writableByteChannel = Resource.fromWritableByteChannel(Channels.newChannel(open()))

    protected def outputStream(openOptions : OpenOption*) = this
    def withCodec(codec: Codec) = Resource.fromOutputStream(opener)(codec)
}

/***************************** ReaderResource ************************************/

/**
 * A ManagedResource for accessing and using Readers.
 *
 * @see ManagedResource
 */
class ReaderResource[+A <: Reader](opener: => A, val sourceCodec:Codec) extends BufferableInputResource[A, BufferedReader] {
    implicit val codec = getCodec()
    def open() = opener

    def buffered = Resource.fromBufferedReader(new BufferedReader(opener))
    def reader = this
    def inputStream = Resource.fromInputStream(new CharInputStream(Left(opener)))
    def readableByteChannel = Resource.fromReadableByteChannel(Channels.newChannel (new CharInputStream(Left(opener))))

    def bytesAsInts = charsToInts
    override def chars(implicit codec: Codec = getCodec()): Traversable[Char] = toTraversable {reader => StreamIterator(reader, sourceCodec, codec)}
    def withCodec(codec: Codec) = Resource.fromReader(opener)(codec)
}

/***************************** WriterResource ************************************/
/**
 * A ManagedResource for accessing and using Writers.
 *
 * @see ManagedResource
 */
class WriterResource[+A <: Writer](opener: => A, val sourceCodec:Codec) extends BufferableOutputResource[A, BufferedWriter] {
    implicit val codec = getCodec()
    def open() = opener

    def writer = this
    def buffered = Resource.fromBufferedWriter(new BufferedWriter(opener))(sourceCodec)
    def outputStream = Resource.fromOutputStream(new WriterOutputStream(opener))
    def writableByteChannel = Resource.fromWritableByteChannel(Channels.newChannel(new WriterOutputStream(opener)))

    protected def outputStream(openOptions : OpenOption*) = outputStream
    def withCodec(codec: Codec) = Resource.fromWriter(opener)(codec)
}

/***************************** ByteChannelResource ************************************/
/**
 * A ManagedResource for accessing and using ByteChannels.
 *
 * @see ManagedResource
 */
class ByteChannelResource[+A <: ByteChannel](opener: => A, val sourceCodec:Codec) extends InputResource[A] with OutputResource[A] {
    implicit val codec = getCodec()
    def open() = opener
    protected def outputStream(openOptions : OpenOption*) = outputStream

    def inputStream = Resource.fromInputStream(Channels.newInputStream(opener))
    def outputStream = Resource.fromOutputStream(Channels.newOutputStream(opener))
    def reader = Resource.fromReader(Channels.newReader(opener, sourceCodec.charSet.name()))
    def writer = Resource.fromWriter(Channels.newWriter(opener, sourceCodec.charSet.name()))
    def writableByteChannel = Resource.fromWritableByteChannel(opener)
    def readableByteChannel = Resource.fromReadableByteChannel(opener)

    def bytesAsInts:Traversable[Int] = toTraversable {in => StreamIterator(Channels.newInputStream(opener))}
    def withCodec(codec: Codec) = Resource.fromByteChannel(opener)(codec)
}


/***************************** ReadableByteChannelResource ************************************/

/**
 * A ManagedResource for accessing and using ByteChannels.
 *
 * @see ManagedResource
 */
class ReadableByteChannelResource[+A <: ReadableByteChannel](opener: => A, val sourceCodec:Codec) extends BufferableInputResource[A, BufferedInputStream] {
    implicit val codec = getCodec()
    def open() = opener
    
    def buffered = inputStream.buffered
    def inputStream = Resource.fromInputStream(Channels.newInputStream(opener))
    def reader = Resource.fromReader(Channels.newReader(opener, sourceCodec.charSet.name()))
    def readableByteChannel = this
    def bytesAsInts:Traversable[Int] = toTraversable {in => StreamIterator(Channels.newInputStream(opener))}
    
    def withCodec(codec: Codec) = Resource.fromReadableByteChannel(opener)(codec)
}

/***************************** WritableByteChannelResource ************************************/

/**
 * A ManagedResource for accessing and using ByteChannels.
 *
 * @see ManagedResource
 */
class WritableByteChannelResource[+A <: WritableByteChannel](opener: => A, val sourceCodec:Codec) extends BufferableOutputResource[A, BufferedOutputStream] {
    implicit val codec = getCodec()
    def open() = opener
    protected def outputStream(openOptions : OpenOption*) = outputStream

    def buffered = outputStream.buffered    
    def outputStream = Resource.fromOutputStream(Channels.newOutputStream(opener))
    def writer = Resource.fromWriter(Channels.newWriter(opener, sourceCodec.charSet.name()))
    def writableByteChannel = this
    
    def withCodec(codec: Codec) = Resource.fromWritableByteChannel(opener)(codec)
}

