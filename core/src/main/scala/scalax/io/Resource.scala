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
import CloseAction.Noop
import java.io._
import nio.SeekableFileChannel
import java.net.URL

protected[io] abstract class ResourceAcquirer[R,U >: R,B](open:()=>R,f:R=>B,closeAction:CloseAction[U]) {
  def close(r:R):Unit
  def apply():Either[List[Throwable],B] = {
    val resource = open()

    var exceptions = List[Throwable]()
    val result = try {
        Some(f(resource))
    } catch {
        case e =>
            exceptions ::= e
            None
    } finally {
        exceptions ++= (closeAction :+ CloseAction(close _))(resource)
    }

    result match {
        case Some(r) => Right(r)
        case None => Left(exceptions)
    }

  }
}
protected[io] class CloseableResourceAcquirer[R <: Closeable,U >: R,B](open:()=>R,f:R=>B,closeAction:CloseAction[U])
    extends ResourceAcquirer[R,U,B](open,f,closeAction) {
  def close(r:R):Unit = r.close()
}

trait ResourceOps[+R, +Repr] {
    def prependCloseAction[B >: R](newAction: CloseAction[B]):Repr
    def appendCloseAction[B >: R](newAction: CloseAction[B]):Repr

    def prependCloseAction[B >: R](newAction: B => Unit):Repr = prependCloseAction(CloseAction(newAction))
    def appendCloseAction[B >: R](newAction: B => Unit):Repr = appendCloseAction(CloseAction(newAction))
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
trait Resource[+R <: Closeable] extends ManagedResourceOperations[R] with ResourceOps[R, Resource[R]]{
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


    def acquireFor[B](f : R => B) : Either[List[Throwable], B] = new CloseableResourceAcquirer(open,f,Noop)()

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
trait Bufferable[+R <: Resource[Closeable]] {
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
trait InputResource[+R <: Closeable] extends Resource[R] with Input with ResourceOps[R, InputResource[R]] {

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
trait ReadCharsResource[+R <: Closeable] extends Resource[R] with ReadChars with ResourceOps[R, ReadCharsResource[R]]

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
trait OutputResource[+R <: Closeable] extends Resource[R] with Output with ResourceOps[R, OutputResource[R]] {
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
trait SeekableResource[+R <: Closeable] extends Seekable with InputResource[R] with OutputResource[R] with ResourceOps[R, SeekableResource[R]]

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
trait WriteCharsResource[+R <: Closeable] extends Resource[R] with WriteChars with ResourceOps[R, WriteCharsResource[R]]

trait BufferableInputResource[+C <: Closeable, +B <: Closeable] extends InputResource[C] with Bufferable[InputResource[B]]
    with ResourceOps[C, BufferableInputResource[C,B]]
trait BufferableOutputResource[+C <: Closeable, +B <: Closeable] extends OutputResource[C] with Bufferable[OutputResource[B]]
    with ResourceOps[C, BufferableOutputResource[C,B]]
trait BufferableReadCharsResource[+C <: Closeable, +B <: Closeable] extends ReadCharsResource[C] with Bufferable[ReadCharsResource[B]]
    with ResourceOps[C, BufferableReadCharsResource[C,B]]
trait BufferableWriteCharsResource[+C <: Closeable, +B <: Closeable] extends WriteCharsResource[C] with Bufferable[WriteCharsResource[B]]
    with ResourceOps[C, BufferableWriteCharsResource[C,B]]

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
  def fromInputStream[A <: InputStream](opener: => A)(implicit extraCloser:CloseAction[A]=Noop) : InputStreamResource[A] = new InputStreamResource[A](opener, extraCloser)
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
  def fromBufferedInputStream[A <: BufferedInputStream](opener: => A)(implicit extraCloser:CloseAction[A]=Noop) : InputStreamResource[A]  = new InputStreamResource[A](opener, extraCloser){
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
  def fromOutputStream[A <: OutputStream](opener: => A)(implicit extraCloser:CloseAction[A]=Noop) : OutputStreamResource[A] = new OutputStreamResource[A](opener,extraCloser)
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
  def fromBufferedOutputStream[A <: BufferedOutputStream](opener: => A)(implicit extraCloser:CloseAction[A]=Noop) : OutputStreamResource[A] = new OutputStreamResource[A](opener,extraCloser) {
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
  def fromReader[A <: Reader](opener: => A)(implicit extraCloser:CloseAction[A]=Noop) : ReaderResource[A] = new ReaderResource[A](opener, extraCloser)
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
  def fromBufferedReader[A <: BufferedReader](opener: => A)(implicit extraCloser:CloseAction[A]=Noop): ReaderResource[A] = new ReaderResource[A](opener,extraCloser) {
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
  def fromWriter[A <: Writer](opener: => A)(implicit extraCloser:CloseAction[A]=Noop) : WriterResource[A] = new WriterResource[A](opener,extraCloser)
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
  def fromBufferedWriter[A <: BufferedWriter](opener: => A)(implicit extraCloser:CloseAction[A]=Noop): WriterResource[A] = new WriterResource[A](opener, extraCloser) {
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
  def fromReadableByteChannel[A <: ReadableByteChannel](opener: => A)(implicit extraCloser:CloseAction[A]=Noop) : ReadableByteChannelResource[A] = new ReadableByteChannelResource[A](opener, extraCloser)
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
  def fromWritableByteChannel[A <: WritableByteChannel](opener: => A)(implicit extraCloser:CloseAction[A]=Noop) : WritableByteChannelResource[A] = new WritableByteChannelResource[A](opener,extraCloser)
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
  def fromByteChannel[A <: ByteChannel](opener: => A)(implicit extraCloser:CloseAction[A]=Noop) : ByteChannelResource[A] = new ByteChannelResource[A](opener,extraCloser)
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
  def fromSeekableByteChannel[A <: SeekableByteChannel](opener: => A)(implicit extraCloser:CloseAction[A]=Noop) : SeekableByteChannelResource[A] = new SeekableByteChannelResource[A](opener,extraCloser)
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
  def fromRandomAccessFile(opener: => RandomAccessFile)(implicit extraCloser:CloseAction[SeekableFileChannel]=Noop) : SeekableByteChannelResource[SeekableFileChannel] = {
    def open = new SeekableFileChannel(opener.getChannel)
    new SeekableByteChannelResource[SeekableFileChannel](open,extraCloser)
  }

  def fromURL(url:URL)(implicit extraCloser:CloseAction[InputStream]=Noop): InputStreamResource[InputStream] = fromInputStream(url.openStream)(extraCloser)
  def fromURLString(url:String)(implicit extraCloser:CloseAction[InputStream]=Noop): InputStreamResource[InputStream] = fromURL(new URL(url))(extraCloser)
  def fromFile(file:File)(implicit extraCloser:CloseAction[SeekableFileChannel]=Noop): SeekableByteChannelResource[SeekableByteChannel] = fromRandomAccessFile(new RandomAccessFile(file,"rw"))(extraCloser)
  def fromFileString(file:String)(implicit extraCloser:CloseAction[SeekableFileChannel]=Noop): SeekableByteChannelResource[SeekableByteChannel] = fromRandomAccessFile(new RandomAccessFile(file,"rw"))(extraCloser)
}

