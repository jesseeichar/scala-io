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

/**
 * Essentially the resource aquire and close functionality.  This is extracted out into a class
 * so that subclasses can reuse the functionality.
 *
 * This was necessary so that subclasses could add [[scalax.io.CloseActions]] but still
 * prevent the signature of the [[scalax.io.CloseActions]] from polluting the type
 * signature of the Resource type.
 */
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

/**
 * Implementation that only works with Closables.  Maybe over engineering but maybe we will have other types of
 * Resources that aren't Closeable dependant
 */
protected[io] class CloseableResourceAcquirer[R <: Closeable,U >: R,B](open:()=>R,f:R=>B,closeAction:CloseAction[U])
    extends ResourceAcquirer[R,U,B](open,f,closeAction) {
  def close(r:R):Unit = r.close()
}

/**
 * A trait allowing adding close actions to a Resource.  The purpose of this trait
 * was to allow the correct subclass type to be returned when calling the methods but
 * keeping the Repr type from the main Resource signature
 *
 *
 * @note in standard immutable fashion the methods do not mutate the current object
 * but return a new instance with the modified behaviour
 *
 * @tparam R The type of object that is managed by this resource
 * @tparam Repr The actual type of the concrete subclass
 */
trait ResourceOps[+R, +Repr] {
  /**
   * Add a [[scalax.io.CloseAction]] to the front of the CloseAction queue.
   *
   * @param newAction The new action to prepend
   * @return a new instance with the added [[scalax.io.CloseAction]]
   */
  def prependCloseAction[B >: R](newAction: CloseAction[B]):Repr
  /**
   * Add a [[scalax.io.CloseAction]] to the end of the [[scalax.io.CloseAction]] queue (the last action executed).
   *
   * @note the actual closing of the resource is always performed after the last  action is executed
   *
   * @param newAction The new action to append
   * @return a new instance with the added [[scalax.io.CloseAction]]
   */
  def appendCloseAction[B >: R](newAction: CloseAction[B]):Repr

  /**
   * Creates a [[scalax.io.CloseAction]] from the function and passes it to prependCloseAction(CloseAction)
   *
   * @param newAction The new action to prepend
   * @return a new instance with the added [[scalax.io.CloseAction]]   *
   */
  def prependCloseAction[B >: R](newAction: B => Unit):Repr = prependCloseAction(CloseAction(newAction))
  /**
   * Creates a [[scalax.io.CloseAction]] from the function and passes it to appendCloseAction(CloseAction)
   *
   * @param newAction The new action to append
   * @return a new instance with the added [[scalax.io.CloseAction]]   *
   */
  def appendCloseAction[B >: R](newAction: B => Unit):Repr = appendCloseAction(CloseAction(newAction))
}
/**
 * A Resource that can be used to do IO.  Primarily it wraps objects from the java io and makes
 * them more Scala friendly.
 *
 * A common question about Resource is: "Why no simply expose [[scalax.io.Input]],
 * [[scalax.io.Output]], [[scalax.io.Seekable]], [[scalax.io.ReadChars]], [[scalax.io.WriteChars]]?
 * Why do we need a Resource[R] object?"
 *
 * There are several reasons for this.  There are several situations where a developer
 * needs access to the underlying resource.
 *
 * Perhaps they need to interact with a Java API which does not use the [[scalax.io.Input]], etc...
 * APIs.  Another possibility is that the resource may be a specific implementation like PrintStream
 * and they want to use those custom APIs instead of the Scala IO apis.  In that case Resource provides
 * them with the ARM functionality that is very useful.
 *
 * @tparam R
 *          The type of the resource that will be managed by the ManagedResource
 *
 * @author  Jesse Eichar
 * @since   1.0
 */
trait Resource[+R <: Closeable] extends ManagedResourceOperations[R] with ResourceOps[R, Resource[R]]{
  /**
   * Creates a new instance of the underlying resource (or opens it).
   * Sometimes the code block used to create the Resource is non-reusable in
   * which case this Resource can only be used once.  This is not recommended.
   * When creating a resource it is recommended to pass the code block for creating
   * the resource to the resource so that the resource can be reused.  Of course this
   * is not always possible
   *
   * This method should only be used with care in cases when Automatic
   * Resource Management cannot be used because the
   * {@link InputStream} must be closed manually.
   *
   * This is public only to permit interoperability with certain Java APIs.
   * A better pattern of use should be:
   * {{{
   * resource.acquireFor {
   *   // call java API
   * }
   * }}}
   * or
   * {{{
   * val calculatedResult = resource.acquireAndGet {
   *   // cal java API that returns a result
   * }
   * }}}
   *
   * @return the actual resource that has been openned
   */
    def open(): R

    def acquireFor[B](f : R => B) : Either[List[Throwable], B] = new CloseableResourceAcquirer(open,f,Noop)()

}

/**
 * An Object that has an associated Buffered object. For example InputStream
 * has BufferedInputStream
 *
 * @tparam R
 *          The resource type that is returned when the buffered method is called
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
 * An Resource object that is a also an [[scalax.io.Input]].  This trait adds methods
 * for converting between common io types such as to a [[scalax.io.ReaderResource]] or
 * [[scalax.io.ReadableByteChannelResource]]
 *
 * @tparam R
 *          the type of underlying resource that is created
 *
 * @author  Jesse Eichar
 * @since   1.0
 */
trait InputResource[+R <: Closeable] extends Resource[R] with Input with ResourceOps[R, InputResource[R]] {

    /**
     * Obtain the [[scalax.io.InputStreamResource]](typically) version of this object.
     *
     * @return the [[scalax.io.InputStreamResource]](typically) version of this object.
     */
    def inputStream: InputResource[InputStream]

    /**
     * Obtain the [[scalax.io.ReadCharsResource]] version of this object.
     *
     * @param codec the codec used to convert from bytes to characters
     *
     * @return the [[scalax.io.ReadCharsResource]] version of this object.
     */
    def reader(implicit sourceCodec: Codec = Codec.default) : ReadCharsResource[Reader]
    /**
     * Obtain the [[scalax.io.ReadableByteChannelResource]](typically) version of this object.
     *
     * @return the [[scalax.io.ReadableByteChannelResource]](typically) version of this object.
     */
    def readableByteChannel: InputResource[ReadableByteChannel]
    def size : Option[Long] = None
}

/**
 * An object that in addition to being a resource is also a [[scalax.io.ReadChars]] Resource.
 *
 * @tparam R The underlying resource
 *
 * @author  Jesse Eichar
 * @since   1.0
 */
trait ReadCharsResource[+R <: Closeable] extends Resource[R] with ReadChars with ResourceOps[R, ReadCharsResource[R]]

/**
 * An Resource object that is a also an [[scalax.io.Output]].  This trait adds methods
 * for converting between common io types such as to a [[scalax.io.WriterResource]] or
 * [[scalax.io.WritableByteChannelResource]]
 *
 * @tparam R
 *          the type of underlying resource that is created
 *
 * @author  Jesse Eichar
 * @since   1.0
 */
trait OutputResource[+R <: Closeable] extends Resource[R] with Output with ResourceOps[R, OutputResource[R]] {
  /**
   * Obtain the [[scalax.io.OutputStreamResource]](typically) version of this object.
   *
   * @return the [[scalax.io.OutputStreamResource]](typically) version of this object.
   */
  def outputStream: OutputResource[OutputStream]
  /**
   * Obtain the [[scalax.io.WriteCharsResource]] version of this object.
   *
   * @param codec the codec used to convert from bytes to characters
   *
   * @return the [[scalax.io.WriteCharsResource]] version of this object.
   */
  def writer(implicit sourceCodec: Codec = Codec.default) : WriteCharsResource[Writer]
  /**
   * Obtain the [[scalax.io.WritableByteChannel]](typically) version of this object.
   *
   * @return the [[scalax.io.WritableByteChannel]](typically) version of this object.
   */
  def writableByteChannel: OutputResource[WritableByteChannel]
}

/**
 * An object that can be viewed as a Seekable object. For example
 * a FileChannel.
 *
 * @param R
 *          the type of the underlying Resource
 *
 * @author  Jesse Eichar
 * @since   1.0
 */
trait SeekableResource[+R <: Closeable] extends Seekable with InputResource[R] with OutputResource[R] with ResourceOps[R, SeekableResource[R]]

/**
 * An object that in addition to being a resource is also a [[scalax.io.WriteChars]] Resource.
 *
 * @tparam R The underlying resource
 *
 * @author  Jesse Eichar
 * @since   1.0
 */
trait WriteCharsResource[+R <: Closeable] extends Resource[R] with WriteChars with ResourceOps[R, WriteCharsResource[R]]

/**
 * A trait consisting of the Combination of [[scalax.io.InputResource]] and [[scalax.io.Bufferable]].  Primary use is to simply the signature of
 * the objects created by the Resource factory object.
 */
trait BufferableInputResource[+C <: Closeable, +B <: Closeable] extends InputResource[C] with Bufferable[InputResource[B]]
    with ResourceOps[C, BufferableInputResource[C,B]]
/**
 * A trait consisting of the Combination of [[scalax.io.OutputResource]] and [[scalax.io.Bufferable]].  Primary use is to simply the signature of
 * the objects created by the Resource factory object.
 */
trait BufferableOutputResource[+C <: Closeable, +B <: Closeable] extends OutputResource[C] with Bufferable[OutputResource[B]]
    with ResourceOps[C, BufferableOutputResource[C,B]]
/**
 * A trait consisting of the Combination of [[scalax.io.ReadCharsResource]] and [[scalax.io.Bufferable]].  Primary use is to simply the signature of
 * the objects created by the Resource factory object.
 */
trait BufferableReadCharsResource[+C <: Closeable, +B <: Closeable] extends ReadCharsResource[C] with Bufferable[ReadCharsResource[B]]
    with ResourceOps[C, BufferableReadCharsResource[C,B]]
/**
 * A trait consisting of the Combination of [[scalax.io.WriteCharsResource]] and [[scalax.io.Bufferable]].  Primary use is to simply the signature of
 * the objects created by the Resource factory object.
 */
trait BufferableWriteCharsResource[+C <: Closeable, +B <: Closeable] extends WriteCharsResource[C] with Bufferable[WriteCharsResource[B]]
    with ResourceOps[C, BufferableWriteCharsResource[C,B]]



/**
 * Defines several factory methods for creating instances of Resource.
 *
 * '''Note:''' It is very important to try an pass a method that creates/opens the underlying resource or
 * the actual creation code as the opener.  This is important so that the resource can
 * be used more than once.  If an opened resource is passed to the factory method the resulting resource can only
 * be used once since it closes the resource when it is closed.
 *
 * '''Example:'''
 *
 * {{{
 *    val URL = new URL("http://scala-lang.org")
 *    val resource: Resource[InputStream] = Resource.fromInputStream(url.openStream).buffered
 * }}}
 *
 * @author  Jesse Eichar
 * @since   1.0
 *
 * @define closeActionParam  @param extraCloser An optional parameter for specifying an additional action to perform as the Resource is closed. This action will be executed just before close.  Close actions can also be added to existing Resources
 * @define openDisclaimer  The opener param is a by-name argument an is use to open a new stream.
 * In other words it is important to try and pass in a function for opening
 * the stream rather than the already opened stream so that the returned
 * Resource can be used multiple time
 */
object Resource {
  // InputStream factory methods
  /**
   *  Create an [[scalax.io.InputStreamResource]] from an InputStream or subclass
   *
   * $openDisclaimer
   *
   * @param opener the function for opening a new InputStream
   *
   * $closeActionParam
   *
   * @return an InputStreamResource
   */
  def fromInputStream[A <: InputStream](opener: => A)(implicit extraCloser:CloseAction[A]=Noop) : InputStreamResource[A] = new InputStreamResource[A](opener, extraCloser)
  /**
   * Create an Input Resource instance from a BufferedInputStream
   *
   * $openDisclaimer
   *
   * the buffered method simply returns the same instance
   *
   * @param opener the function for opening a new BufferedInputStream
   *
   * $closeActionParam
   *
   * @return a InputStreamResource that is backed by a BufferedInputStream
   */
  def fromBufferedInputStream[A <: BufferedInputStream](opener: => A)(implicit extraCloser:CloseAction[A]=Noop) : InputStreamResource[A]  = new InputStreamResource[A](opener, extraCloser){
      override def buffered = this;
  }

  // OutputStream factory methods
  /**
   * Create an Output Resource instance from an OutputStream.
   *
   * $openDisclaimer
   *
   * @param opener the function for opening a new OutputStream
   *
   * $closeActionParam
   *
   * @return an OutputStreamResource
   */
  def fromOutputStream[A <: OutputStream](opener: => A)(implicit extraCloser:CloseAction[A]=Noop) : OutputStreamResource[A] = new OutputStreamResource[A](opener,extraCloser)
  /**
   * Create an Output Resource instance from a BufferedOutputStream
   *
   * $openDisclaimer
   *
   *
   * @param opener the function for opening a new BufferedOutputStream
   *
   *
   * $closeActionParam
   *
   * @return a OutputStreamResource that is backed by a BufferedOutputStream
   */
  def fromBufferedOutputStream[A <: BufferedOutputStream](opener: => A)(implicit extraCloser:CloseAction[A]=Noop) : OutputStreamResource[A] = new OutputStreamResource[A](opener,extraCloser) {
    override def buffered = this
  }

  // Reader factory methods
  /**
   * Create an ReadChars Resource instance from an Reader.
   *
   * $openDisclaimer
   *
   * @param opener the function for opening a new Reader
   *
   * $closeActionParam
   *
   * @return an ReaderResource
   */
  def fromReader[A <: Reader](opener: => A)(implicit extraCloser:CloseAction[A]=Noop) : ReaderResource[A] = new ReaderResource[A](opener, extraCloser)
  /**
   * Create an ReadChars Resource instance from an BufferedReader.
   *
   * $openDisclaimer
   *
   *
   * @param opener the function for opening a new BufferedReader
   *
   * $closeActionParam
   *
   * @return a ReaderResource that is backed by a BufferedReader
   */
  def fromBufferedReader[A <: BufferedReader](opener: => A)(implicit extraCloser:CloseAction[A]=Noop): ReaderResource[A] = new ReaderResource[A](opener,extraCloser) {
      override def buffered = this
  }

  // Writer factory methods
  /**
   * Create an WriteChars Resource instance with conversion traits from an Writer.
   *
   * $openDisclaimer
   *
   * @param opener the function for opening a new Writer
   *
   * $closeActionParam
   *
   * @return an WriterResource
   */
  def fromWriter[A <: Writer](opener: => A)(implicit extraCloser:CloseAction[A]=Noop) : WriterResource[A] = new WriterResource[A](opener,extraCloser)
  /**
   * Create an WriteChars Resource instance with conversion traits from an BufferedWriter.
   *
   * $openDisclaimer
   *
   * @param opener the function for opening a new BufferedWriter
   *
   * $closeActionParam
   *
   * @return a WriterResource that is backed by a BufferedWriter
   */
  def fromBufferedWriter[A <: BufferedWriter](opener: => A)(implicit extraCloser:CloseAction[A]=Noop): WriterResource[A] = new WriterResource[A](opener, extraCloser) {
      override def buffered = this
  }

  // Channel factory methods
  /**
   * Create an Input Resource instance from an ReadableByteChannel.
   *
   * $openDisclaimer
   *
   * @param opener the function for opening a new ReadableByteChannel
   *
   * $closeActionParam
   *
   * @return an ReadableByteChannelResource
   */
  def fromReadableByteChannel[A <: ReadableByteChannel](opener: => A)(implicit extraCloser:CloseAction[A]=Noop) : ReadableByteChannelResource[A] = new ReadableByteChannelResource[A](opener, extraCloser)
  /**
   * Create an Output Resource instance from an WritableByteChannel.
   *
   * $openDisclaimer
   *
   * @param opener the function for opening a new WritableByteChannel
   *
   * $closeActionParam
   *
   * @return an WritableByteChannelResource
   */
  def fromWritableByteChannel[A <: WritableByteChannel](opener: => A)(implicit extraCloser:CloseAction[A]=Noop) : WritableByteChannelResource[A] = new WritableByteChannelResource[A](opener,extraCloser)
  /**
   * Create an Input/Output Resource instance from a ByteChannel.
   *
   * $openDisclaimer
   *
   * @param opener the function for opening a new ByteChannel
   *
   * $closeActionParam
   *
   * @return a ByteChannelResource
   */
  def fromByteChannel[A <: ByteChannel](opener: => A)(implicit extraCloser:CloseAction[A]=Noop) : ByteChannelResource[A] = new ByteChannelResource[A](opener,extraCloser)
  /**
   * Create an Input/Output/Seekable Resource instance from a SeekableByteChannel.
   *
   * $openDisclaimer
   *
   * @param opener the function for opening a new SeekableByteChannel
   *
   * $closeActionParam
   *
   * @return a SeekableByteChannelResource
   */
  def fromSeekableByteChannel[A <: SeekableByteChannel](opener: => A)(implicit extraCloser:CloseAction[A]=Noop) : SeekableByteChannelResource[A] = new SeekableByteChannelResource[A](opener,extraCloser)
  /**
   * Create an Input/Output/Seekable Resource instance from a RandomAccess file.
   *
   * $openDisclaimer
   *
   * @param opener the function for opening a new SeekableByteChannel
   *
   * $closeActionParam
   *
   * @return a SeekableByteChannelResource
   */
  def fromRandomAccessFile(opener: => RandomAccessFile)(implicit extraCloser:CloseAction[SeekableFileChannel]=Noop) : SeekableByteChannelResource[SeekableFileChannel] = {
    def open = new SeekableFileChannel(opener.getChannel)
    new SeekableByteChannelResource[SeekableFileChannel](open,extraCloser)
  }

  /**
   * Creates an Input Resource from a URL
   *
   * @param url the url to use for constructing a InputStreamResource
   *
   * $closeActionParam
   *
   * @return an InputStreamResource
   */
  def fromURL(url:URL)(implicit extraCloser:CloseAction[InputStream]=Noop): InputStreamResource[InputStream] = fromInputStream(url.openStream)(extraCloser)

  /**
   * Converts the string to a URL and creates an Input Resource from the URL
   *
   * @param url the url string to use for constructing a InputStreamResource
   *
   * $closeActionParam
   *
   * @return an InputStreamResource
   *
   * @throws java.net.MalformedURLException if the url string is not a valid URL
   */
  def fromURLString(url:String)(implicit extraCloser:CloseAction[InputStream]=Noop): InputStreamResource[InputStream] = fromURL(new URL(url))(extraCloser)
  /**
   * Creates a Seekable Resource from a File
   *
   * @param file the file to use for constructing a Seekable Resource
   *
   * $closeActionParam
   *
   * @return a SeekableByteChannelResource
   * @throws java.io.IOException if file does not exist
   */
  def fromFile(file:File)(implicit extraCloser:CloseAction[SeekableFileChannel]=Noop): SeekableByteChannelResource[SeekableByteChannel] = fromRandomAccessFile(new RandomAccessFile(file,"rw"))(extraCloser)
  /**
   * Create a file from string then create a Seekable Resource from a File
   *
   * @param file the file to use for constructing a Seekable Resource
   *
   * $closeActionParam
   *
   * @return a SeekableByteChannelResource
   * @throws java.io.IOException if file does not exist
   */
  def fromFileString(file:String)(implicit extraCloser:CloseAction[SeekableFileChannel]=Noop): SeekableByteChannelResource[SeekableByteChannel] = fromRandomAccessFile(new RandomAccessFile(file,"rw"))(extraCloser)

def fromClasspath(name: String,
                  cl: Class[_] )
                 (implicit extraCloser: CloseAction[InputStream] = Noop) : InputStreamResource[InputStream]= {
    val url = cl.getClassLoader.getResource(name)
    require(url != null)
    Resource.fromURL(url)(extraCloser)
  }
}

